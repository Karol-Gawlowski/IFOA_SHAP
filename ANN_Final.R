# try running code from git...
# try poisson on mine
# grid search over a tree based model for benchmark / comparison with NN
# try even EARLIER stopping

library(tidyverse)
library(caret) # one hot encoding
library(DALEX) # largest standalone XAI library I know
library(shapper) #extension to DALEX, essentially an API for py SHAP
library(reticulate)

seed=2
set.seed(seed)

library(tensorflow)
library(keras)

use_condaenv(condaenv = "TEST_ENV2",required = T) # 
set_random_seed(seed, disable_gpu = FALSE)
py_module_available("tensorflow")
is_keras_available()

py_config()
################ data handling ################ 

# upload the data and apply transofmations and corrections
data=read.csv2("data/freMTPL2freq.csv",sep = ",") %>% #read.csv2("data/freMTPLfreq.csv",sep = ",") %>% 
  as_tibble() %>%
  mutate(VehPower  = factor(VehPower, order = TRUE,levels = 4:15),
         across(c(Area,Region,VehBrand,VehGas),factor)) %>%
  rowwise() %>% 
  mutate(ClaimNb = as.integer(min(ClaimNb,4)),
         VehAge = as.integer(min(VehAge,20)),
         DrivAge = as.integer(min(DrivAge,90)),
         Exposure = as.numeric(min(Exposure,1)), 
         Density = log(Density)) %>% 
  ungroup() #%>% 
# mutate(ClaimNb = ClaimNb/Exposure) # not in Wutrich?

# remove ID's
data = data %>% select(-c(IDpol,Exposure))

# save min and max of numerical columns
minmax = data.frame(max = apply(data %>% select_if(Negate(is.factor)),2,max),
                    min = apply(data %>% select_if(Negate(is.factor)),2,min)) %>% t() %>%  as.data.frame()

# train/test split
sample_TTS = sample(x = 1:nrow(data),
                    size = round(0.85 * nrow(data)),
                    replace = FALSE)

train = data[sample_TTS,] %>% as.data.frame() #%>% as.matrix()
test = data[-sample_TTS,] %>% as.data.frame() #%>% as.matrix()


# Normalization of numeric variables (maybe there is a cleaner way using mutate_if)
data_prep = function(data){
  
  Normalized = apply(data %>% 
                       select(-ClaimNb) %>%  #####
                       select_if(Negate(is.factor)), 
                     2, 
                     FUN = function(x){return((x-min(x))/(max(x)-min(x)))}) %>% 
    as_tibble()
  
  data = data %>% select_if(is.factor) %>% cbind(Normalized)
  
  data$VehPower = factor(data$VehPower, order = FALSE)
  
  # OHE of factor variables
  OHE = dummyVars("~.", data = data %>% 
                    select_if(is.factor)) %>% 
    predict(newdata = data %>% 
              select_if(is.factor)) %>% 
    as_tibble()
  
  data = data %>% select_if(Negate(is.factor)) %>% cbind(OHE)
  
  data = as_tibble(data)
  
  return(data)
}

# data prep separately for tarin and test
train = data_prep(train)
test = data_prep(test)

################ prep for modeling ################ 

# utility function for analysing model performance
model_evalulation = function(model,
                             data = test,
                             type = "NN",
                             ClaimNBadj = FALSE){
  if (type=="NN"){
    
    Evaluation = model %>% evaluate(data %>% select(-ClaimNb) %>% as.matrix() , data$ClaimNb)
    
    if( ClaimNBadj==TRUE){
    
    # rescaled Predictions (since min(ClaimNb)==0 then it's just times max(ClaimNb)) 
    Predictions = data.frame(Predicted = (model %>% predict(data %>% select(-ClaimNb) %>% as.matrix()) - 1),
                             Actual = data$ClaimNb) # *minmax$ClaimNb[1] 
    }else{
      # rescaled Predictions (since min(ClaimNb)==0 then it's just times max(ClaimNb)) 
      Predictions = data.frame(Predicted = (model %>% predict(data %>% select(-ClaimNb) %>% as.matrix())),
                               Actual = data$ClaimNb) # *minmax$ClaimNb[1] 
    }
    
  }else if(type=="GLM"){
    
    Predictions = data.frame(Predicted = exp(predict(model,newdata = data %>% select(-ClaimNb))), 
                             Actual = data$ClaimNb)
    
    Evaluation = data.frame(#loss = NA,
      absolute_error = mean(abs(as.matrix(Predictions$Predicted - data$ClaimNb))),
      mean_squared_error = mean(as.matrix((Predictions$Predicted - data$ClaimNb)^2)))
    
  }
  
  AvE = Predictions %>% mutate(Actual = as.factor(Actual)) %>% 
    group_by(Actual) %>% 
    summarise(count = n(),
              mean_pred = mean(Predicted),
              sd_pred = sd(Predicted),
              min = min(Predicted),
              max = max(Predicted),
              Q1 = quantile(Predicted,probs = 0.25),
              Q2 = quantile(Predicted,probs = 0.5),
              Q3 = quantile(Predicted,probs = 0.75),
              IQR = (Q3-Q1)/Q2,
              Negative_Pred = sum(Predicted<0))
  
  
  Sorted_by_MSE = data %>% mutate(Predictions = Predictions$Predicted,
                                  SquaredError = (Predictions - ClaimNb)^2) %>% arrange(-SquaredError)
  
  return(list(Evaluation = Evaluation,
              Predictions = Predictions,
              AvE = AvE,
              Sorted_by_MSE = Sorted_by_MSE))
}

eval_list = list()

early_stop = callback_early_stopping(monitor = "val_loss", patience = 10)

################  model training 1  ################ 
model1 = keras_model_sequential(input_shape = c(ncol(train))) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  
  # layer_dense(units = 64, activation = 'relu') %>%
  # layer_dense(units = 32, activation = 'relu') %>%
  # layer_dense(units = 16, activation = 'relu') %>%
  # layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 1)

summary(model1)

model1 %>% 
  compile(
    loss = "poisson",
    # metrics = list("mse","poisson")
    optimizer = optimizer_rmsprop()
    # optimizer = "adam"
    ) %>% 
  fit(
    # callbacks = list(early_stop),
    # train %>% select(-ClaimNb) %>% as.matrix(), 
    # train$ClaimNb+1,
    train %>% as.matrix(), 
    (data[sample_TTS,"ClaimNb"]+1) %>% as.matrix(),
    
    batch_size = 4096, #4096
    epochs = 60,
    validation_split = 0.2,
    shuffle = TRUE ,
    sample_weight =  ((data[sample_TTS,"ClaimNb"]>0)*1+1) %>% as.matrix()
  )

# plot(history)

eval_list$model1 = model_evalulation(model1,
                                     cbind(test,data[-sample_TTS,"ClaimNb"]),
                                     type = "NN",
                                     ClaimNBadj = TRUE)

eval_list$model1$Evaluation
eval_list$model1$AvE
eval_list$model1$Sorted_by_MSE

#########################################


################  model training 2  ################ 

early_stop = callback_early_stopping(monitor = "val_loss", patience = 6)


model2 = keras_model_sequential(input_shape = c(ncol(train))) %>% 
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  # layer_dense(units = 128, activation = 'relu') %>%
  # layer_dense(units = 128, activation = 'relu') %>%
  
  # layer_dense(units = 64, activation = 'relu') %>%
  # layer_dense(units = 32, activation = 'relu') %>%
  # layer_dense(units = 16, activation = 'relu') %>%
  # layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'relu')

summary(model2)

model2 %>% 
  compile(
    loss = "mse",
    # metrics = list("mse","poisson")
    optimizer = optimizer_rmsprop()
    # optimizer = "adam"
  ) %>% 
  fit(
    callbacks = list(early_stop),
    # train %>% select(-ClaimNb) %>% as.matrix(), 
    # train$ClaimNb+1,
    train %>% as.matrix(), 
    (data[sample_TTS,"ClaimNb"]) %>% as.matrix(),
    
    batch_size = 1024, #4096
    epochs = 60,
    validation_split = 0.2,
    shuffle = TRUE ,
    sample_weight =  ((data[sample_TTS,"ClaimNb"]>0)*1+1) %>% as.matrix()
  )

# plot(history)

eval_list$model2 = model_evalulation(model2,
                                     cbind(test,data[-sample_TTS,"ClaimNb"]),
                                     type = "NN")

eval_list$model2$Evaluation
eval_list$model2$AvE
eval_list$model2$Sorted_by_MSE

model2 %>% save_model_tf("models/model2")

#########################################

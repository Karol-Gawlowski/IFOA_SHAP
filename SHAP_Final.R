# https://docs.seldon.io/projects/alibi/en/stable/methods/KernelSHAP.html <- formulas

library(tidyverse)
library(reshape2)

set.seed(1)

data_shap=read.csv2("data/Shapley_data.csv",
                    col.names = c("Age","Temp","Height","y"),
                    sep = ",",
                    header = TRUE)

models=list()

# # Model to explain
models[["1,1,1"]] = lm(formula = y~.,data = data_shap)

# proper models for each coalition
models[["1,1,0"]] = lm(formula = y~Age+Temp,data = data_shap)
models[["1,0,1"]] = lm(formula = y~Age+Height,data = data_shap)
models[["0,1,1"]] = lm(formula = y~Temp+Height,data = data_shap)
models[["1,0,0"]] = lm(formula = y~Age,data = data_shap)
models[["0,1,0"]] = lm(formula = y~Temp,data = data_shap)
models[["0,0,1"]] = lm(formula = y~Height,data = data_shap)
models[["0,0,0"]] = lm(formula = y~.,data=data_shap %>% select(y))

# coalition matrix
weights = data.frame(coalition = c("1,1,1","1,1,0","1,0,1","0,1,1","1,0,0","0,1,0","0,0,1","0,0,0"),
                     z = c(3,2,2,2,1,1,1,0)) %>% 
  mutate(weight = (3-1)/(choose(n = 3,k = z)*z*(3-z))) 

# remove non finite values
weights$weight[which(!is.finite(weights$weight))]=0

####        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        # 
###        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ## 
##        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ### 
#        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        #### 

# SHAP

manual_shap = function(data = data_shap, 
                         table = weights,
                         obsID = 1,
                         iterations = 1000,
                         seed=1,
                         plot=FALSE){
  
  set.seed(seed)
  cols = colnames(data)[1:3]  
  coal = table$coalition
  
  # object to store  values
  results = data.frame(coal = coal)
  results[,c(cols[1],cols[2],cols[3],"yhat")]=NA
  
  for (i in 1:length(coal)){
    
    # prepare object to store actual and resampled data (step 3 from excel file)
    storage = NA
    # storage = data.frame(matrix(NA,nrow = sample_size,ncol=length(cols)))
    storage = data.frame(matrix(NA,nrow = 1,ncol=length(cols)))
    colnames(storage) = cols
    
    if (coal[i]!="0,0,0"){
      # retrieve colnames from coal
      actual = cols[str_split(coal[i],pattern = ",")[[1]] %>% strtoi() * (1:3)]
      # either retrieve actual values (1's in coal vector) and resample the remaining
      storage[,actual] = data[obsID,actual]
    }
    
    if (coal[i]!="1,1,1"){
      # retrieve colnames from coal
      sampled = cols[(1 - str_split(coal[i],pattern = ",")[[1]] %>% strtoi()) * (1:3)]
      # either retrieve actual values (1's in coal vector) and resample the remaining
      storage[,sampled] = apply(data[,sampled] %>% as.data.frame(), 2, mean) 
    }
    
    # predict
    results[i,c(cols,"yhat")] = storage %>% mutate(yhat = predict.lm(object = models[["1,1,1"]],newdata = storage) %>% as.matrix())
    
  }
  
  # coalitions as a df
  S = as.data.frame(matrix(str_split(coal,pattern = ",") %>% unlist() %>% strtoi(), ncol = 3,byrow = TRUE))
  colnames(S) = cols
  
  # yhat and coalitions
  results = cbind(results,S)
  
  # obtain SHAP 
  SHAP = lm(yhat~.,data = results[-c(1:4)],weights = table$weight)
  
  SHAP_final = SHAP$coefficients[cols]
  
  return(SHAP = SHAP_final)
  
}


####        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        # 
###        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ## 
##        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ### 
#        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        #### 

# Proper Shapley values

pure_shapley = function(data = data_shap, 
                        table = weights %>% filter(weight!=0),
                        # table = weights,
                        obsID = 1){
  
  N = 3 # nr of variables
  
  cols = colnames(data)[1:3] # names of variables
  coal = table$coalition # coalitions 
  phi = rep(0,3) # store shapley values
  observation = data[obsID,1:3] # observation to explain
  
  # manipulate coalitions
  S = as.data.frame(matrix(str_split(coal,pattern = ",") %>% unlist() %>% strtoi() ,ncol = 3,byrow = TRUE))
  colnames(S) = cols
  S = cbind(S,coal)
  
  # loop through variables 
  for (i in 1:N){
    
    # number of coalitions that don't include i'th variable
    M = nrow(S[S[,i]!=1,]) # always 3 in our example
    
    # loop through coalitions without i'th variable
    for (j in S[S[,i]!=1,4]){
      
      # include the i ' th variable in j ' th coalition
      k = str_split(j,pattern = ",")[[1]]  %>% strtoi()
      k[i] = 1
      k = paste(k,sep="",collapse = ",")
      
      # subset variables according to coalition including (col_k) and excluding (col_j) i'th variable
      col_k = (str_split(k,pattern = ",")[[1]]  %>% strtoi())*(1:N)
      col_j = (str_split(j,pattern = ",")[[1]]  %>% strtoi())*(1:N)
      
      observation_k = as.data.frame(observation[,col_k])
      observation_j = as.data.frame(observation[,col_j])
      
      # to handle one-dimensional named df's
      colnames(observation_k) = colnames(observation)[col_k]
      colnames(observation_j) = colnames(observation)[col_j] 
      
      # compute the contribution of feature i for coalition j   
      Contribution = predict.lm(object = models[[k]],newdata = observation_k) - predict.lm(object = models[[j]],newdata = observation_j)
      
      # V 1
      # phi[i] = phi[i] + (1/choose(n = M-1,k = sum(S[which(S[,4]==j),1:3]))) * Contribution
      
      # V 2
      # phi[i] = phi[i] + Contribution
      
      # V 3
      # K = sum(col_j/col_j,na.rm = TRUE)
      # phi[i] = phi[i] + ( (factorial(K)*factorial(M - K - 1)) / factorial(M) ) *  Contribution
      
      # V 4
      K = sum(col_j/col_j,na.rm = TRUE)
      phi[i] = phi[i] + (factorial(K)*factorial(N-1-K)) * Contribution
      
      
      
    }
  }
  
  # V 1
  # phi = phi / M
  
  # V 4
  phi = phi / factorial(N) # 
  
  
  names(phi)=cols
  
  return(SHAPLEY = phi)
}


####        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        # 
###        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ## 
##        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ### 
#        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        ///        #### 
# additivity axiom check

results = data.frame(matrix(NA,ncol = 3,nrow=17))
colnames(results)= c("actual","Shapley","SHAP")

average_prediction = predict.lm(object = models[["1,1,1"]],newdata = data[,1:3]) %>% mean()

for (i in 1:17){
  
  obsID = i
  
  actual_prediction = predict.lm(object = models[["1,1,1"]],newdata = data[obsID,1:3]) %>% as.matrix() %>% c()
  
  pure_shapley_v = pure_shapley(obsID = obsID)
  SHAP_check = manual_shap(obsID = obsID,iterations = 1000, plot = TRUE)
  
  # this difference should be equal to the sum of shapley values 
  results[i,] = data.frame(actual = actual_prediction  - average_prediction,
                           pure_shapley = sum(pure_shapley_v),
                           SHAP = sum(SHAP_check))
  
}


results %>% mutate(diff_Shapley = actual - Shapley,
                   diff_SHAP = actual - SHAP)


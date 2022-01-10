library(xgboost)


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
    
  }else if(type == "XGB"){
    
    Predictions = data.frame(Predicted = predict(model,newdata = data %>% select(-ClaimNb) %>% as.matrix()),
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


#create hyperparameter grid
hyper_grid <- expand.grid(max_depth = seq(3, 6, 1), eta = seq(.2, .35, .01))  





for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    data = train[, 2:34],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j]
  )
  
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
}    



m1_xgb <-
  xgboost(
    # data = data[sample_TTS,] %>% select(-ClaimNb) %>% ,
    # label = data[sample_TTS,]$ClaimNb ,
    
    data = train %>% as.matrix() ,
    label = data[sample_TTS,]$ClaimNb ,
    
    nrounds = 50,
    objective = "count:poisson",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25,
    verbose = 2
  )  




m2_xgb <-
  xgboost(
    # data = data[sample_TTS,] %>% select(-ClaimNb) %>% ,
    # label = data[sample_TTS,]$ClaimNb ,
    
    data = train %>% as.matrix() ,
    label = data[sample_TTS,]$ClaimNb ,
    weight = ((data[sample_TTS,"ClaimNb"]>0)*1+1) %>% as.matrix(),
    
    nrounds = 150,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 8,
    eta = .15,
    verbose = 2
  )  



eval_list$xgb1 = model_evalulation(model = m2_xgb,
                  data = data.frame(test, ClaimNb = data[-sample_TTS,"ClaimNb"]),
                  type = "XGB")

eval_list$xgb1$Evaluation
eval_list$xgb1$AvE


########### model 3

m3_xgb <-
  xgboost(
    # data = data[sample_TTS,] %>% select(-ClaimNb) %>% ,
    # label = data[sample_TTS,]$ClaimNb ,
    
    data = train %>% as.matrix() ,
    label = data[sample_TTS,]$ClaimNb ,
    weight = ((data[sample_TTS,"ClaimNb"]>0)*1+1) %>% as.matrix(),
    
    nrounds = 150,
    objective = "count:poisson",
    early_stopping_rounds = 3,
    max_depth = 8,
    eta = .15,
    verbose = 2
  )  



eval_list$xgb3 = model_evalulation(model = m3_xgb,
                                   data = data.frame(test, ClaimNb = data[-sample_TTS,"ClaimNb"]),
                                   type = "XGB")

eval_list$xgb3$Evaluation
eval_list$xgb3$AvE


########### model 4

m4_xgb <-
  xgboost(
    # data = data[sample_TTS,] %>% select(-ClaimNb) %>% ,
    # label = data[sample_TTS,]$ClaimNb ,
    
    data = train %>% as.matrix() ,
    label = data[sample_TTS,]$ClaimNb ,
    weight = (data[sample_TTS,"ClaimNb"]+1) %>% as.matrix(),
    
    nrounds = 200,
    objective = "count:poisson",
    early_stopping_rounds = 3,
    max_depth = 4,
    eta = .3,
    verbose = 2
  )  



eval_list$xgb4 = model_evalulation(model = m4_xgb,
                                   data = data.frame(test, ClaimNb = data[-sample_TTS,"ClaimNb"]),
                                   type = "XGB")

eval_list$xgb4$Evaluation
eval_list$xgb4$AvE



########### model 5

XGB_validation = 1:10000

dtrain = xgb.DMatrix(data = train[-XGB_validation,] %>% as.matrix(), 
                     label = data[sample_TTS,]$ClaimNb[-XGB_validation])

dvalid = xgb.DMatrix(data = train[XGB_validation,] %>% as.matrix(), 
                     label = data[sample_TTS,]$ClaimNb[XGB_validation])

watchlist <- list(train=dtrain, test=dvalid)

m5_xgb <-
  xgboost(
    # data = data[sample_TTS,] %>% select(-ClaimNb) %>% ,
    # label = data[sample_TTS,]$ClaimNb ,
    
    # data = train[-XGB_validation,] %>% as.matrix() ,
    # label = data[sample_TTS,]$ClaimNb[-XGB_validation],
    data = dtrain,
    
    nrounds = 200,
    
    
    weight = (data[sample_TTS,"ClaimNb"]+1) %>% as.matrix(),
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 8,
    eta = .3,
    verbose = 2,
    watchlist = watchlist
  )  


eval_list$xgb5 = model_evalulation(model = m5_xgb,
                                   data = data.frame(test, ClaimNb = data[-sample_TTS,"ClaimNb"]),
                                   type = "XGB")

eval_list$xgb5$Evaluation
eval_list$xgb5$AvE

###########################################################################
###########################################################################
###########################################################################
###########################################################################


test_xgb = xgb.train(
  # data = data[sample_TTS,] %>% select(-ClaimNb) %>% ,
  # label = data[sample_TTS,]$ClaimNb ,
  
  # data = train[-XGB_validation,] %>% as.matrix() ,
  # label = data[sample_TTS,]$ClaimNb[-XGB_validation],
  data = dtrain,
  
  nrounds = 200,
  
  weight = (data[sample_TTS,"ClaimNb"]+1) %>% as.matrix(),
  objective = "reg:squarederror",
  early_stopping_rounds = 10,
  max_depth = 8,
  eta = .3,
  verbose = 2,
  watchlist = watchlist
)  



hyper_grid <- expand.grid(max_depth = c(3, 5, 7),
                          objective = c("count:poisson","reg:squarederror"),
                          eta = seq(.1, .35, .05))

XGB_validation = 1:50000

dtrain = xgb.DMatrix(data = train[-XGB_validation,] %>% as.matrix(), 
                     label = data[sample_TTS,]$ClaimNb[-XGB_validation])

dvalid = xgb.DMatrix(data = train[XGB_validation,] %>% as.matrix(), 
                     label = data[sample_TTS,]$ClaimNb[XGB_validation])

watchlist <- list(train = dtrain, 
                  test = dvalid)

save_XGBs = list()
save_XGBs$models = list()
save_XGBs$evals = list()

Save_XGB_results = cbind(data.frame(iter = 1:nrow(hyper_grid),
                              mse = NA),hyper_grid)

for (i in 1:nrow(hyper_grid)){
  
  save_XGBs$models[[i]] = xgb.train(
    data = dtrain,
    watchlist = watchlist,
    early_stopping_rounds = 10,
    # weight = (data[sample_TTS,"ClaimNb"]+1) %>% as.matrix(),
    verbose = 1,
    
    nrounds = 200,
    objective = hyper_grid$objective[i],
    max_depth = hyper_grid$max_depth[i],
    eta = hyper_grid$eta[i]
  )  
  
  save_XGBs$evals[[i]] = model_evalulation(model = save_XGBs$models[[i]],
                                     data = data.frame(test, ClaimNb = data[-sample_TTS,"ClaimNb"]),
                                     type = "XGB")
  
  Save_XGB_results$mse[i] = save_XGBs$evals[[i]]$Evaluation$mean_squared_error
  
  print(paste("iteration:",i))
  
}

Save_XGB_results %>% arrange(-mse)


save_XGBs$evals[[2]]$AvE


# save(save_XGBs,file = "models/XGB.rda")
# load("models/XGB.rda")

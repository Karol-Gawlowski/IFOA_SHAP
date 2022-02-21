# Normalization of numeric variables (maybe there is a cleaner way using mutate_if)
data_prep = function(data){
  
  Normalized = apply(data %>% select(-ClaimNb) %>% select_if(Negate(is.factor)), 
                     2, 
                     FUN = function(x){return((x-min(x))/(max(x)-min(x)))}) %>% 
    as_tibble()
  
  data = data %>% select_if(is.factor) %>% cbind(Normalized)
  
  data$VehPower = factor(data$VehPower, order = FALSE)
  
  # OHE of factor variables
  OHE = dummyVars("~.", data = data %>% select_if(is.factor)) %>% 
    predict(newdata = data %>% select_if(is.factor)) %>% 
    as_tibble()
  
  data = data %>% select_if(Negate(is.factor)) %>% cbind(OHE) %>% as_tibble()
  
  return(data)
}


# as per Wutrich
ExposureWeightedPoissonDevianceLoss = function(predicted,
                                               actual,
                                               exposure=NULL){
  
  output = rep(NA,length(actual))
  
  if (is.null(exposure)){
    exposure = rep(1,length(actual))
  }
  
  # actual zero 
  k = (actual == 0)
  output[k] = 2*predicted[k]*exposure[k]
  
  # actual non zero and predicted non-zero
  m = (actual != 0 & predicted != 0 )
  output[m] = 2*actual[m]*(predicted[m]*exposure[m]/actual[m] - 1 - log(predicted[m]*exposure[m]/actual[m]))
  
  # actual non-zero and predicted zero
  n =  (actual != 0 & predicted == 0 )
  output[n] = 2*actual[n]
  
  output = sum(output)
  
  return(output)
}

# utility function for analysing model performance
model_evalulation = function(model,
                             data = test,
                             type = "NN",
                             ClaimNBadj = FALSE){
  if (type=="NN"){
    
    if( ClaimNBadj==TRUE){
      
      # rescaled Predictions 
      Predictions = data.frame(Predicted = (model %>% predict(data %>% select(-ClaimNb) %>% as.matrix()) %>% - 1),
                               Actual = data$ClaimNb) 
    }else{
      
      Predictions = data.frame(Predicted = (model %>% predict(data %>% select(-ClaimNb) %>% as.matrix())),
                               Actual = data$ClaimNb) 
    }
    
  }else if(type=="GLM"){
    
    Predictions = data.frame(Predicted = predict(model,newdata = data %>% select(-ClaimNb,-Exposure,-IDpol),type="response"), 
                             Actual = data$ClaimNb)
    
  }else if(type == "XGB"){
    
    Predictions = data.frame(Predicted = predict(model,newdata = data %>% select(-ClaimNb) %>% as.matrix()),
                             Actual = data$ClaimNb)
  }
  
  Evaluation = data.frame(
    absolute_error = mean(abs(as.matrix(Predictions$Predicted - data$ClaimNb))),
    mean_squared_error = mean(as.matrix((Predictions$Predicted - data$ClaimNb)^2)),
    ExposureWeightedPoissonDevianceLoss = ExposureWeightedPoissonDevianceLoss(Predictions$Predicted,data$ClaimNb)
  )
  
  
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

# for DALEX and SHAP explainer objects
predict_wrapper=function(model,new_data){
  return(model %>% predict(new_data %>% as.matrix()))
}

predict_wrapper_GLM=function(model,new_data){
  return(predict(model,newdata = new_data,type="response"))
}



# Customized and corrected SHAP plot
CustomSHAPplot=function(dalex_output,
                        epsilon = 0.007){
  
  colnames(dalex_output) = str_replace_all(colnames(dalex_output),pattern = "_",replacement = "")
  
  # supply values in names for continuous variables
  dalex_output$vname[dalex_output$vname=="VehAge"]=paste("VehAge",round(dalex_output$VehAge[1],3),sep=".")
  dalex_output$vname[dalex_output$vname=="DrivAge"]=paste("DrivAge",round(dalex_output$DrivAge[1],3),sep=".")
  dalex_output$vname[dalex_output$vname=="BonusMalus"]=paste("BonusMalus",round(dalex_output$BonusMalus[1],3),sep=".")
  
  dalex_output = dalex_output %>% as_tibble()
  
  avg_pred = dalex_output$yhatmean
  total_pred = dalex_output$yhat
  other_pred = sum(dalex_output$attribution[abs(dalex_output$attribution)<=epsilon])
  
  plot_data = rbind(data.frame(values = c(avg_pred[1],other_pred),
                               labels = c("Mean Prediction","Other")),
                    dalex_output %>% 
                      as_tibble() %>% 
                      filter(abs(attribution) > epsilon) %>% 
                      mutate(attribution = attribution) %>%
                      transmute(values = attribution,
                                labels = vname) %>% 
                      arrange(abs(values))) %>% 
    mutate(values = round(values,3))
  
  plt = plot_data %>% 
    waterfall(fill_by_sign = TRUE,
              calc_total = TRUE,
              total_axis_text = "Final Prediction")+
    coord_flip()+
    ggtitle("SHAP plot")+
    xlab("SHAP contributions")+
    ylab("Features")+
    theme_light()+
    theme(text=element_text(family="serif"),
          legend.justification = c("right", "top"))
  
  return(plt)
}
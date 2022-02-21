# don't enter next epoch if there are no significant performance gains 
early_stop = callback_early_stopping(monitor = "val_loss", patience = 3)

# Neural network structure
Neural_Net = keras_model_sequential(input_shape = c(ncol(train))) %>% 
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'relu')

# Fitting the model
Neural_Net %>% 
  compile(
    loss = "poisson",
    optimizer = optimizer_rmsprop()
  ) %>% 
  fit(
    callbacks = list(early_stop),
    train %>% as.matrix(), 
    (data[sample_TTS,"ClaimNb"]/data_input$Exposure[sample_TTS]) %>% as.matrix(),
    batch_size = 2^13,
    epochs = 10,
    validation_split = 0.1,
    shuffle = TRUE ,
    sample_weight = data_input$Exposure[sample_TTS] %>% as.matrix() 
  )


eval_list$Neural_Net = model_evalulation(Neural_Net,
                                     cbind(test,data[-sample_TTS,"ClaimNb"]),
                                     type = "NN",
                                     ClaimNBadj = FALSE)

eval_list$Neural_Net$Evaluation
eval_list$Neural_Net$AvE
eval_list$Neural_Net$Sorted_by_MSE

# Neural_Net %>% save_model_tf("models/Poiss_Loss_Good3")


# Poisson deviance loss
# https://deliverypdf.ssrn.com/delivery.php?ID=900085111104075000070018100123113011117031086047004028106097087126074073076106064018018122042001016007048127081118083100002001014043030086068071028118096106089079091084028081113008000023088094066122119070101119118113011114125120115094070026064022029&EXT=pdf&INDEX=TRUE

# custom loss
# https://towardsdatascience.com/custom-loss-functions-for-deep-learning-predicting-home-values-with-keras-for-r-532c9e098d1f
# # Mean Squared Log Absolute Error
# MSLAE <- function( y_true, y_pred ) {
#   K <- backend()
#   K$mean( K$pow( K$abs( K$log( K$relu(y_true *1000 ) + 1 ) - 
#                           K$log( K$relu(y_pred*1000 ) + 1)), 2))
# }
# 
# # Compile the model, and select one of the loss functions
# losses <- c(keras::loss_mean_squared_error,  
#             keras::loss_mean_squared_logarithmic_error, MLAE, MSLAE)
# model %>% compile(
#   optimizer = "rmsprop",
#   loss = losses[1],
#   metrics = c("mae")
# )

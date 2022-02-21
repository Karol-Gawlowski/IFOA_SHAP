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

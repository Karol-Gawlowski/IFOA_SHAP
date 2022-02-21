GLM = glm(formula = ClaimNb/Exposure ~ Area+VehPower+VehAge+DrivAge+BonusMalus+VehBrand+VehGas+Density+Region,
          family = poisson(link = log),
          weights = Exposure,
          data = data_input[sample_TTS,] %>% select(-IDpol))

eval_list$GLM = model_evalulation(GLM,
                                  data_input[-sample_TTS,],
                                  type = "GLM")

eval_list$GLM$AvE
eval_list$GLM$Evaluation


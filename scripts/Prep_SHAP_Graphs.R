# Use the same observations for parts .2 for comaprability purposes
library(waterfalls)

explainer = list()
residuals = list()
SHAP = list()

row_subset = sample(1:nrow(test),1000)

cbind(test,data[-sample_TTS,"ClaimNb"])[88763,]

# find interesting observations to investigate
eval_list$model3$Predictions %>% 
  mutate(ID=row_number()) %>% 
  arrange(-Actual,Predicted)


# 1 Neural Network
# Prepare explainer from DALEX
explainer$Neural_Net = explain(
  model = Neural_Net,
  data = test[row_subset,],
  y = (data[-sample_TTS,"ClaimNb"] %>% as.matrix())[row_subset],
  predict_function = predict_wrapper,
  label = "Neural_Net"
)

# An instance with 1 claim and very low error
SHAP$Neural_Net_1 = shap(explainer$Neural_Net, 
                       new_observation = test[678,],
                       method = "KernelSHAP"
)

SHAP$Neural_Net_1_plot = CustomSHAPplot(dalex_output = SHAP$Neural_Net_1)

# An instance with 0 claims and very high error
SHAP$Neural_Net_2 =  shap(explainer$Neural_Net, 
                                           new_observation = test[12892,],
                                           method = "KernelSHAP"
)

SHAP$Neural_Net_2_plot = CustomSHAPplot(dalex_output = SHAP$Neural_Net_2)

# An instance with 2 claims 
SHAP$Neural_Net_3 =  shap(explainer$Neural_Net, 
                          new_observation = test[88763,],
                          method = "KernelSHAP"
)

SHAP$Neural_Net_3_plot = CustomSHAPplot(dalex_output = SHAP$Neural_Net_3)


ggsave(plot = SHAP$Neural_Net_1_plot,
       path = "graphs", 
       filename = "SHAP_Neural_Net_1_plot.png",
       device = "png")

ggsave(plot = SHAP$Neural_Net_2_plot,
       path = "graphs", 
       filename = "SHAP_Neural_Net_2_plot.png",
       device = "png")

ggsave(plot = SHAP$Neural_Net_3_plot,
       path = "graphs", 
       filename = "SHAP_Neural_Net_3_plot.png",
       device = "png")

# 2. XGB
explainer$XGB = explain(
  model = XGB,
  data = test[row_subset,],
  y = (data[-sample_TTS,"ClaimNb"] %>% as.matrix())[row_subset],
  predict_function = "_____",
  label = "XGB"
)

# An instance with 1 claim and very low error
SHAP$XGB_1 = shap(explainer$XGB, 
                  new_observation = test[678,],
                  method = "KernelSHAP"
)

SHAP$XGB_1_plot = CustomSHAPplot(dalex_output = SHAP$XGB_1)

# An instance with 0 claims and very high error
SHAP$XGB_2 =  shap(explainer$XGB, 
                   new_observation = test[12892,],
                   method = "KernelSHAP"
)

SHAP$XGB_2_plot = CustomSHAPplot(dalex_output = SHAP$XGB_2)

# An instance with 2 claims 
SHAP$XGB_3 =  shap(explainer$XGB, 
                   new_observation = test[29805,],
                   method = "KernelSHAP"
)

SHAP$XGB_3_plot = CustomSHAPplot(dalex_output = SHAP$XGB_3)


# 3. GLM
colnames_GLM = c("Area","VehPower","VehAge","DrivAge","BonusMalus","VehBrand","VehGas","Density","Region")

explainer$GLM = explain(
  model = GLM,
  data = data_input[-sample_TTS,colnames_GLM][row_subset,]  %>% mutate(VehPower = factor(VehPower,ordered = FALSE)),
  y = (data_input[-sample_TTS,"ClaimNb"])[row_subset,],
  # predict_function = predict_wrapper_GLM,
  label = "GLM"
)

# An instance with 1 claim and very low error for NN and high err for GLM
SHAP$GLM_1 = shap(explainer$GLM, 
                  new_observation = data_input[-sample_TTS,colnames_GLM][678,],
                  method = "KernelSHAP"
)

# predict_wrapper_GLM(GLM,data_input[row_subset,colnames_GLM][678,] %>% mutate(VehPower = factor(VehPower,ordered = FALSE)))

SHAP$GLM_1_plot = CustomSHAPplot(dalex_output = SHAP$GLM_1)

# An instance with 0 claims and very high error
SHAP$GLM_2 =  shap(explainer$GLM, 
                   new_observation = data_input[row_subset,][12892,],
                   method = "KernelSHAP"
)

SHAP$GLM_2_plot = CustomSHAPplot(dalex_output = SHAP$GLM_2)

# An instance with 2 claims 
SHAP$GLM_3 =  shap(explainer$GLM, 
                   new_observation = data_input[row_subset,][29805,],
                   method = "KernelSHAP"
)

SHAP$GLM_3_plot = CustomSHAPplot(dalex_output = SHAP$GLM_3)

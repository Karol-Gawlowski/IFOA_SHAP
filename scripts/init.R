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

library(skimr)
library(xgboost)

# object to store results
eval_list = list()

source("scripts/functions.R")

# upload, transform and correct the data
data_input=read.csv2("data/freMTPL2freq.csv",sep = ",") %>% 
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
data = data_input %>% select(-c(IDpol,Exposure))

# train/test split
sample_TTS = sample(x = 1:nrow(data),size = round(0.85 * nrow(data)),replace = FALSE)

train = data[sample_TTS,] %>% as.data.frame() #%>% as.matrix()
test = data[-sample_TTS,] %>% as.data.frame() #%>% as.matrix()

# data prep separately for train and test
train = data_prep(train)
test = data_prep(test)


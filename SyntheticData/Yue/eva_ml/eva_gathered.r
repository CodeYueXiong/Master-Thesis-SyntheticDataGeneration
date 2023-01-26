# load the required packages
library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(synthpop)
library(ggplot2)
library(dbplyr)
library(here)

library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3viz)
library(mlr3verse)
library(mlr3benchmark)

library(e1071)
library(MASS)
library(glmnet)
library(ranger)

# set the working directory
wd <- "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy/SyntheticData/Yue"
# wd <- "/Volumes/ru27req/MasterThesisRoxy/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# load the stored benchmark results based on different models
bm_cart_models <- readRDS("./syn1_cart/bm_models_cart.rds")
bm_rf_models <- readRDS("./syn2_rf/bm_rf_models.rds")
bm_bag_models <- readRDS("./syn3_bag/bm_bag_models.rds")
bm_polyreg_models <- readRDS("./syn4_polyreg/bm_polyreg_models.rds")
bm_norm_models <- readRDS("./syn5_norm/bm_norm_models.rds")
bm_normrank_models <- readRDS("./syn6_normrank/bm_normrank_models.rds")

bm_cart_multinom <- bm_cart_models$aggregate(msr("classif.acc"))[learner_id=="classif.multinom",]
bm_cart_ranger <- bm_cart_models$aggregate(msr("classif.acc"))[learner_id=="classif.ranger",]
mlr3misc::map(as.data.table(bm_cart_models)$learner, "model")

bm_cart_ranger

bm_rf_models$aggregate(msr("classif.acc"))[learner_id=="classif.multinom",]
bm_rf_models$aggregate(msr("classif.acc"))[learner_id=="classif.ranger",]

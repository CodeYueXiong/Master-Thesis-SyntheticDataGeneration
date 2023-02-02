##########################################################################
########-----------------experiment with norm--------------------#########
# the difference is based on the the use of method("weight"),
# it differs from sample, norm, normrank
##########################################################################

# load the required packages
library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(synthpop)
library(ggplot2)
library(dbplyr)
library(data.table)
library(here)


# set the working directory
# wd <- "F:/Master-Thesis-DifferentialPrivacy"
wd <- "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
# wd <- "/Volumes/ru27req/MasterThesisRoxy/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# load the preprocessed original data
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)
# Encoding c1_m,
# -99      1      2 
# 8958 148525 102816
# 1      2      3 
# 8958 148525 102816
bindori_dataset_threshold_chr$C1_m <- as.integer(bindori_dataset_threshold_chr$C1_m)
# var C3
bindori_dataset_threshold_chr$C3 <- as.integer(bindori_dataset_threshold_chr$C3)
# var C5
# -99      1      2      3      4      5      6 
# 13673 106397  58551  21450  12313  41348   6567
bindori_dataset_threshold_chr$C5 <- as.integer(bindori_dataset_threshold_chr$C5)
# var C8
# -99      1      2 
# 11413 247412   1474
bindori_dataset_threshold_chr$C8 <- as.integer(bindori_dataset_threshold_chr$C8)
# var D1
# -99      1      2      3      4      5 
# 18545   2111   6152  26588  45796 161107 
bindori_dataset_threshold_chr$D1 <- as.integer(bindori_dataset_threshold_chr$D1)
# var D3
# -99      1      2      3      4 
# 21808  58804 103986  53107  22594
bindori_dataset_threshold_chr$D3 <- as.integer(bindori_dataset_threshold_chr$D3)
# var D4
# -99      1      2      3      4 
# 18800  10534  20719  45985 164261 
bindori_dataset_threshold_chr$D4 <- as.integer(bindori_dataset_threshold_chr$D4)
# var E3
# -99      1      2      3      4 
# 37572  92584 127854    509   1780 
bindori_dataset_threshold_chr$E3 <- as.integer(bindori_dataset_threshold_chr$E3)
# var E4
# -99     1     2     3     4     5     6     7 
# 33977 20154 45379 48939 46555 36414 23079  5802
bindori_dataset_threshold_chr$E4 <- as.integer(bindori_dataset_threshold_chr$E4)
# var B4
# -99   [0, 1) [1, 5) 
# 249422  290  10587
bindori_dataset_threshold_chr$B4 <- as.integer(bindori_dataset_threshold_chr$B4)
# var E6,
#   -99 [0, 9) 
# 57585 202714
#     1      2 
# 57585 202714  
bindori_dataset_threshold_chr$E6 <- as.integer(bindori_dataset_threshold_chr$E6)


# also, we can probably subset those columns with constant inputs
cols_remove <- c("B13_1", "B13_2", "B13_3", "B13_4",
                 "B13_5", "B13_6", "B13_7",
                 "B14_1", "B14_2", "B14_3", "B14_4", "B14_5",
                 "D6_1", "D6_2", "D6_3", "F3_de")
bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(-all_of(cols_remove))
# also for those B1b_x like vars and D10, we try exclude them from the synthesis
cols_rm_bd <- c("B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
                "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11","B1b_x12", "B1b_x13", "D10",
                "C0_1", "C0_2", "C0_3", "C0_4", "C0_5", "C0_6")

bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(-all_of(cols_rm_bd))
ncol(bindori_dataset_threshold_chr)==54


##########################################################################
######---------------synthetic data with synthpop-------------------######
##########################################################################

# first of all, we try extract all the methods with m=0 set in cart
settings_default <- syn(bindori_dataset_threshold_chr, method = "cart", m = 0)
# now we take a look at the extracted settings with $method and $visit.sequence
arg_method <- settings_default$method
arg_col <- settings_default$visit.sequence


# for var weight, we should always try "parametric", sample, norm, normrank
# ----------------------------try norm for weight-------------------------------

# # demographics vars E2,E3,E4,E5,E7, we try finding the column index first
# E2_index <- match("E2",names(bindori_dataset_threshold_chr))
# E3_index <- match("E3",names(bindori_dataset_threshold_chr)) # always try "sample"
# E4_index <- match("E4",names(bindori_dataset_threshold_chr))
# E5_index <- match("E5",names(bindori_dataset_threshold_chr))
# E7_index <- match("E7",names(bindori_dataset_threshold_chr))

para_weight_list <- c("sample", "norm", "normrank")

syn_norm_experiment <- function(para_weight_list, index, bindori_dataset_threshold_chr, arg_method, arg_col) {
  ### specify the method to use for group of vars ###
  # E3 and weight
  arg_method[['E3']] <- "sample"
  arg_method[['weight']] <- para_weight_list[index]  # sample(1), norm(2), normrank(3) to choose
  # E2, E4, E5, E7, ranger for exp_norm, except E2
  # arg_method[['E2']] <- "norm"
  arg_method[['E4']] <- "norm"
  arg_method[['E5']] <- "norm"
  arg_method[['E7']] <- "norm"
  # B3 to B11, cart
  # arg_method[['B3']] <- "norm"
  # arg_method[['B4']] <- "norm"
  # arg_method[['B5']] <- "norm"
  # arg_method[['B6']] <- "norm"
  # arg_method[['B7']] <- "norm"
  # arg_method[['B8']] <- "norm"
  # arg_method[['B9']] <- "norm"
  # arg_method[['B10']] <- "norm"
  # arg_method[['B11']] <- "norm"
  # C3, C8, cart
  # arg_method[['C3']] <- "norm"
  # arg_method[['C8']] <- "norm"
  # C2, C4, C5, C6
  arg_method[['C2']] <- "norm"
  arg_method[['C4']] <- "norm"
  arg_method[['C5']] <- "norm"
  arg_method[['C6']] <- "norm"
  # D1, D2, D4, D5
  arg_method[['D1']] <- "norm"
  arg_method[['D2']] <- "norm"
  arg_method[['D4']] <- "norm"
  arg_method[['D5']] <- "norm"
  
  syn_dataset <- NULL
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:54,1)], visit.sequence = arg_col[c(2:54, 1)])
  
  write.syn(syn_dataset, filename = paste("norm", para_weight_list[index], "syn", sep="_"), filetype = "rda")
  message("syn done!")
}

# tryout for norm_sample
sds_normsample_tryout <- syn_norm_experiment(para_weight_list, index=1, bindori_dataset_threshold_chr, arg_method, arg_col)
# tryout for norm_norm
sds_normnorm_tryout <- syn_norm_experiment(para_weight_list, index=2, bindori_dataset_threshold_chr, arg_method, arg_col)
# tryout for norm_normrank
sds_normnormrank_tryout <- syn_norm_experiment(para_weight_list, index=3, bindori_dataset_threshold_chr, arg_method, arg_col)


# ##########################################################
# ------------------ normsample ----------------------------
# ##########################################################
sds_normsample_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_norm_sample_syn.RData")
sds_normsample_tryout <- object
# encode variables as integers for ods and normsample
bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_normsample_tryout$syn[[1]][,2:54] <- sapply(sds_normsample_tryout$syn[[1]][,2:54],as.integer)

sds_normsample_tryout$syn <- sds_normsample_tryout$syn[[1]]
#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_normsample_model1 <- lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                                    data = sds_normsample_tryout)
summary(lm_normsample_model1)
compare_normsample_model1 <- compare(lm_normsample_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the normsample object
saveRDS(lm_normsample_model1, "./SyntheticData/Yue/lm_normsample_model1.rds")
saveRDS(compare_normsample_model1, "./SyntheticData/Yue/compare_normsample_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_normsample_model2 <- lm.synds(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                                    data = sds_normsample_tryout)
summary(lm_normsample_model2)
compare_normsample_model2 <- compare(lm_normsample_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the normsample object
saveRDS(lm_normsample_model2, "./SyntheticData/Yue/lm_normsample_model2.rds")
saveRDS(compare_normsample_model2, "./SyntheticData/Yue/compare_normsample_model2.rds")

rep_normsample <- replicated.uniques(sds_normsample_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_normsample, "./SyntheticData/Yue/rep_normsample.rds")

# ##########################################################
# ------------------ normnorm ----------------------------
# ##########################################################

# load the syn object for norm norm
sds_normnorm_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_norm_norm_syn.RData")
sds_normnorm_tryout <- object
# encode as integer
sds_normnorm_tryout$syn[[1]][,2:54] <- sapply(sds_normnorm_tryout$syn[[1]][,2:54],as.integer)
sds_normnorm_tryout$syn <- sds_normnorm_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_normnorm_model1 <- lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                                  data = sds_normnorm_tryout)
summary(lm_normnorm_model1)
compare_normnorm_model1 <- compare(lm_normnorm_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the normnorm object
saveRDS(lm_normnorm_model1, "./SyntheticData/Yue/lm_normnorm_model1.rds")
saveRDS(compare_normnorm_model1, "./SyntheticData/Yue/compare_normnorm_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_normnorm_model2 <- lm.synds(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                                  data = sds_normnorm_tryout)
summary(lm_normnorm_model2)
compare_normnorm_model2 <- compare(lm_normnorm_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the normnorm object
saveRDS(lm_normnorm_model2, "./SyntheticData/Yue/lm_normnorm_model2.rds")
saveRDS(compare_normnorm_model2, "./SyntheticData/Yue/compare_normnorm_model2.rds")

rep_normnorm <- replicated.uniques(sds_normnorm_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_normnorm, "./SyntheticData/Yue/rep_normnorm.rds")

# ##########################################################
# ------------------ normnormrank ----------------------------
# ##########################################################
# sds_normnormrank_tryout <- syn_norm_experiment(para_weight_list, index=3, bindori_dataset_threshold_chr, arg_method, arg_col)
# load the syn object for norm normrank
sds_normnormrank_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_norm_normrank_syn.RData")
sds_normnormrank_tryout <- object
# encode as integer
sds_normnormrank_tryout$syn[[1]][,2:54] <- sapply(sds_normnormrank_tryout$syn[[1]][,2:54],as.integer)
sds_normnormrank_tryout$syn <- sds_normnormrank_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_normnormrank_model1 <- lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                                      data = sds_normnormrank_tryout)
summary(lm_normnormrank_model1)
compare_normnormrank_model1 <- compare(lm_normnormrank_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the normnormrank object
saveRDS(lm_normnormrank_model1, "./SyntheticData/Yue/lm_normnormrank_model1.rds")
saveRDS(compare_normnormrank_model1, "./SyntheticData/Yue/compare_normnormrank_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_normnormrank_model2 <- lm.synds(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                                      data = sds_normnormrank_tryout)
summary(lm_normnormrank_model2)
compare_normnormrank_model2 <- compare(lm_normnormrank_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the normnormrank object
saveRDS(lm_normnormrank_model2, "./SyntheticData/Yue/lm_normnormrank_model2.rds")
saveRDS(compare_normnormrank_model2, "./SyntheticData/Yue/compare_normnormrank_model2.rds")

rep_normnormrank <- replicated.uniques(sds_normnormrank_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_normnormrank, "./SyntheticData/Yue/rep_normnormrank.rds")


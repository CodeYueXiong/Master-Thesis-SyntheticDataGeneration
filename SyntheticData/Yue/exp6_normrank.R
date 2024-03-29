##########################################################################
########-----------------experiment with normrank--------------------#########
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


# set the working directory
# wd <- "F:/Master-Thesis-DifferentialPrivacy"
wd <- "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
# wd <- "/Volumes/ru27req/MasterThesisRoxy/Master-Thesis-DifferentialPrivacy"
setwd(wd)
wd <- "/Users/roxy/Desktop/Master-Thesis-SyntheticDataGeneration"
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

syn_normrank_experiment <- function(para_weight_list, index, bindori_dataset_threshold_chr, arg_method, arg_col) {
  ### specify the method to use for group of vars ###
  # E3 and weight
  arg_method[['E3']] <- "sample"
  arg_method[['weight']] <- para_weight_list[index]  # sample(1), norm(2), normrank(3) to choose
  # E2, E4, E5, E7, ranger for exp_norm, except E2
  arg_method[['E4']] <- "normrank"
  arg_method[['E5']] <- "normrank"
  arg_method[['E7']] <- "normrank"

  # C2, C4, C5, C6
  arg_method[['C2']] <- "normrank"
  arg_method[['C4']] <- "normrank"
  arg_method[['C5']] <- "normrank"
  arg_method[['C6']] <- "normrank"
  # D1, D2, D4, D5
  arg_method[['D1']] <- "normrank"
  arg_method[['D2']] <- "normrank"
  arg_method[['D4']] <- "normrank"
  arg_method[['D5']] <- "normrank"
  
  syn_dataset <- NULL
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:54,1)], visit.sequence = arg_col[c(2:54, 1)])
  
  write.syn(syn_dataset, filename = paste("normrank", para_weight_list[index], "syn", sep="_"), filetype = "rda")
  message("syn done!")
}

# tryout for normrank_sample
sds_normranksample_tryout <- syn_normrank_experiment(para_weight_list, index=1, bindori_dataset_threshold_chr, arg_method, arg_col)
# tryout for normrank_norm
sds_normranknorm_tryout <- syn_normrank_experiment(para_weight_list, index=2, bindori_dataset_threshold_chr, arg_method, arg_col)
# tryout for normrank_normrank
sds_normranknormrank_tryout <- syn_normrank_experiment(para_weight_list, index=3, bindori_dataset_threshold_chr, arg_method, arg_col)

# ##########################################################
# ------------------ normranksample ----------------------------
# ##########################################################
sds_normranksample_tryout <- load("./SyntheticData/Yue/synobject_normrank_sample_syn.RData")
sds_normranksample_tryout <- object
# encode variables as integers for ods and normranksample
bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_normranksample_tryout$syn[[1]][,2:54] <- sapply(sds_normranksample_tryout$syn[[1]][,2:54],as.integer)

sds_normranksample_tryout$syn <- sds_normranksample_tryout$syn[[1]]
#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_normranksample_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                       B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                       B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                       B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                       C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                       D1 + D2 + D3 + D4 + D5 + 
                                       E2 + E3 + E4 + E7 + E5 + E6,
                                    data = sds_normranksample_tryout)
summary(lm_normranksample_model1)
compare_normranksample_model1 <- compare(lm_normranksample_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_normranksample_model1
# Save the normranksample object
saveRDS(lm_normranksample_model1, "./SyntheticData/Yue/lm_normranksample_model1.rds")
saveRDS(compare_normranksample_model1, "./SyntheticData/Yue/compare_normranksample_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_normranksample_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                       B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                       B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                       B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                       C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                       E2 + E3 + E4 + E7 + E5 + E6,
                                    data = sds_normranksample_tryout)
summary(lm_normranksample_model2)
compare_normranksample_model2 <- compare(lm_normranksample_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_normranksample_model2
# Save the normranksample object
saveRDS(lm_normranksample_model2, "./SyntheticData/Yue/lm_normranksample_model2.rds")
saveRDS(compare_normranksample_model2, "./SyntheticData/Yue/compare_normranksample_model2.rds")

rep_normranksample <- replicated.uniques(sds_normranksample_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_normranksample, "./SyntheticData/Yue/rep_normranksample.rds")
# ##########################################################
# ------------------ normranknorm ----------------------------
# ##########################################################

# load the syn object for normrank norm
sds_normranknorm_tryout <- load("./SyntheticData/Yue/synobject_normrank_norm_syn.RData")
sds_normranknorm_tryout <- object
# encode as integer
sds_normranknorm_tryout$syn[[1]][,2:54] <- sapply(sds_normranknorm_tryout$syn[[1]][,2:54],as.integer)
sds_normranknorm_tryout$syn <- sds_normranknorm_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_normranknorm_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                     B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                     B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                     B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                     C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                     D1 + D2 + D3 + D4 + D5 + 
                                     E2 + E3 + E4 + E7 + E5 + E6,
                                  data = sds_normranknorm_tryout)
summary(lm_normranknorm_model1)
compare_normranknorm_model1 <- compare(lm_normranknorm_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_normranknorm_model1
# Save the normranknorm object
saveRDS(lm_normranknorm_model1, "./SyntheticData/Yue/lm_normranknorm_model1.rds")
saveRDS(compare_normranknorm_model1, "./SyntheticData/Yue/compare_normranknorm_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_normranknorm_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                     B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                     B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                     B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                     C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                     E2 + E3 + E4 + E7 + E5 + E6,
                                  data = sds_normranknorm_tryout)
summary(lm_normranknorm_model2)
compare_normranknorm_model2 <- compare(lm_normranknorm_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_normranknorm_model2
# Save the normranknorm object
saveRDS(lm_normranknorm_model2, "./SyntheticData/Yue/lm_normranknorm_model2.rds")
saveRDS(compare_normranknorm_model2, "./SyntheticData/Yue/compare_normranknorm_model2.rds")

rep_normranknorm <- replicated.uniques(sds_normranknorm_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_normranknorm, "./SyntheticData/Yue/rep_normranknorm.rds")
# ##########################################################
# ------------------ normranknormrank ----------------------------
# ##########################################################
# sds_normranknormrank_tryout <- syn_normrank_experiment(para_weight_list, index=3, bindori_dataset_threshold_chr, arg_method, arg_col)
# load the syn object for normrank normrank
sds_normranknormrank_tryout <- load("./SyntheticData/Yue/synobject_normrank_normrank_syn.RData")
sds_normranknormrank_tryout <- object
# encode as integer
sds_normranknormrank_tryout$syn[[1]][,2:54] <- sapply(sds_normranknormrank_tryout$syn[[1]][,2:54],as.integer)
sds_normranknormrank_tryout$syn <- sds_normranknormrank_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_normranknormrank_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                         B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                         B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                         B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                         C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                         D1 + D2 + D3 + D4 + D5 + 
                                         E2 + E3 + E4 + E7 + E5 + E6,
                                      data = sds_normranknormrank_tryout)
summary(lm_normranknormrank_model1)
compare_normranknormrank_model1 <- compare(lm_normranknormrank_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_normranknormrank_model1
# Save the normranknormrank object
saveRDS(lm_normranknormrank_model1, "./SyntheticData/Yue/lm_normranknormrank_model1.rds")
saveRDS(compare_normranknormrank_model1, "./SyntheticData/Yue/compare_normranknormrank_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_normranknormrank_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                         B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                         B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                         B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                         C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                         E2 + E3 + E4 + E7 + E5 + E6,
                                      data = sds_normranknormrank_tryout)
summary(lm_normranknormrank_model2)
compare_normranknormrank_model2 <- compare(lm_normranknormrank_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_normranknormrank_model2
# Save the normranknormrank object
saveRDS(lm_normranknormrank_model2, "./SyntheticData/Yue/lm_normranknormrank_model2.rds")
saveRDS(compare_normranknormrank_model2, "./SyntheticData/Yue/compare_normranknormrank_model2.rds")

rep_normranknormrank <- replicated.uniques(sds_normranknormrank_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_normranknormrank, "./SyntheticData/Yue/rep_normranknormrank.rds")

##########################################################################
#####---------------experiment with cart-------------------######
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
wd <- "/Users/roxy/Desktop/Master-Thesis-SyntheticDataGeneration"
setwd(wd)

# load the preprocessed original data
load("bindori_dataset_preprocessed_normal.rda")
bindori_dataset_threshold_normal <- bindori_dataset_threshold_chr
load("bindori_dataset_preprocessed_factor.rda")
bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr
# check the occurrences of open response variables
table(bindori_dataset_threshold_normal$B2, bindori_dataset_threshold_chr$B2)
table(bindori_dataset_threshold_normal$B4, bindori_dataset_threshold_chr$B4)
table(bindori_dataset_threshold_normal$E5, bindori_dataset_threshold_chr$E5)
table(bindori_dataset_threshold_normal$E6, bindori_dataset_threshold_chr$E6)
# we have the dataframe here named as "bindori_dataset_threshold_chr"
# Encoding, var B2, not needed for cart
# -1    -99 [0, 1) [1, 3) 
#  3 151336   2405 106555
#  1      2      3      4 
#  3 151336   2405 106555 
# bindori_dataset_threshold_chr$B2 <- as.integer(bindori_dataset_threshold_chr$B2)
# var B4,
#    -99 [0, 1) [1, 5)
# 249422    290  10587
#      1      2      3 
# 249422    290  10587 
# bindori_dataset_threshold_chr$B4 <- as.integer(bindori_dataset_threshold_chr$B4)
# var E5,
#   -99 [0, 1) [1, 2) 
# 45595   8766 205938 
#     1      2      3 
# 45595   8766 205938
# bindori_dataset_threshold_chr$E5 <- as.integer(bindori_dataset_threshold_chr$E5)
# var E6,
#   -99 [0, 9) 
# 57585 202714 
#     1      2 
# 57585 202714  
# bindori_dataset_threshold_chr$E6 <- as.integer(bindori_dataset_threshold_chr$E6)

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
str(bindori_dataset_threshold_chr)
# nrow(bindori_dataset_threshold_chr)

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

syn_cart_experiment <- function(para_weight_list, index, bindori_dataset_threshold_chr, arg_method, arg_col) {
  ### specify the method to use for group of vars ###
  # E3 and weight
  arg_method[['E3']] <- "sample"
  arg_method[['weight']] <- para_weight_list[index]  # sample(1), norm(2), normrank(3) to choose
  # E2, E4, E5, E7, cart for exp_cart
  arg_method[['E2']] <- "cart"
  arg_method[['E4']] <- "cart"
  arg_method[['E5']] <- "cart"
  arg_method[['E7']] <- "cart"
  # B3 to B11, cart
  arg_method[['B3']] <- "cart"
  arg_method[['B4']] <- "cart"
  arg_method[['B5']] <- "cart"
  arg_method[['B6']] <- "cart"
  arg_method[['B7']] <- "cart"
  arg_method[['B8']] <- "cart"
  arg_method[['B9']] <- "cart"
  arg_method[['B10']] <- "cart"
  arg_method[['B11']] <- "cart"
  # C3, C8
  arg_method[['C3']] <- "cart"
  arg_method[['C8']] <- "cart"
  # C2, C4, C5, C6
  arg_method[['C2']] <- "cart"
  arg_method[['C4']] <- "cart"
  arg_method[['C5']] <- "cart"
  arg_method[['C6']] <- "cart"
  # D1, D2, D4, D5
  arg_method[['D1']] <- "cart"
  arg_method[['D2']] <- "cart"
  arg_method[['D4']] <- "cart"
  arg_method[['D5']] <- "cart"
  
  syn_dataset <- NULL
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:54,1)], visit.sequence = arg_col[c(2:54, 1)])
  
  # write.syn(syn_dataset, filename = paste("cart", para_weight_list[index], "syn", sep="_"), filetype = "rda")
  
  message("syn done!")
  
  return(syn_dataset)
}

# ##########################################################
# ------------------ cartsample ----------------------------
# ##########################################################
# sds_cartsample_tryout <- syn_cart_experiment(para_weight_list, index=1, bindori_dataset_threshold_chr, arg_method, arg_col)
sds_cartsample_tryout <- load("./SyntheticData/Yue/synobject_cart_sample_syn.RData")
sds_cartsample_tryout <- object
# encode as integer
bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_cartsample_tryout$syn[[1]][,2:54] <- sapply(sds_cartsample_tryout$syn[[1]][,2:54],as.integer)
sds_cartsample_tryout$syn <- sds_cartsample_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_cartsample_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                        B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                        B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                        B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                        C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                        D1 + D2 + D3 + D4 + D5 + 
                                        E2 + E3 + E4 + E7 + E5 + E6,
                          data = sds_cartsample_tryout)
summary(lm_cartsample_model1)
compare_cartsample_model1 <- compare(lm_cartsample_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_cartsample_model1
# Save the cartsample object
saveRDS(lm_cartsample_model1, "./SyntheticData/Yue/lm_cartsample_model1.rds")
saveRDS(compare_cartsample_model1, "./SyntheticData/Yue/compare_cartsample_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_cartsample_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                      B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                      B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                      B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                      C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                      E2 + E3 + E4 + E7 + E5 + E6,
                                 data = sds_cartsample_tryout)
summary(lm_cartsample_model2)
compare_cartsample_model2 <- compare(lm_cartsample_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_cartsample_model2
# Save the cartsample object
saveRDS(lm_cartsample_model2, "./SyntheticData/Yue/lm_cartsample_model2.rds")
saveRDS(compare_cartsample_model2, "./SyntheticData/Yue/compare_cartsample_model2.rds")

rep_cartsample <- replicated.uniques(sds_cartsample_tryout,bindori_dataset_threshold_chr)

readRDS("./SyntheticData/Yue/rep_cartsample.rds")
readRDS("./SyntheticData/Yue/rep_cartnorm.rds")
saveRDS(rep_cartsample, "./SyntheticData/Yue/rep_cartsample.rds")
# ##########################################################
# ------------------ cartnorm ----------------------------
# ##########################################################

# load the syn object for cart norm
sds_cartnorm_tryout <- load("./SyntheticData/Yue/synobject_cart_norm_syn.RData")
sds_cartnorm_tryout <- object
# encode as integer
sds_cartnorm_tryout$syn[[1]][,2:54] <- sapply(sds_cartnorm_tryout$syn[[1]][,2:54],as.integer)
sds_cartnorm_tryout$syn <- sds_cartnorm_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_cartnorm_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                      B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                      B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                      B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                      C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                      D1 + D2 + D3 + D4 + D5 + 
                                      E2 + E3 + E4 + E7 + E5 + E6,
                                 data = sds_cartnorm_tryout)
summary(lm_cartnorm_model1)
compare_cartnorm_model1 <- compare(lm_cartnorm_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_cartnorm_model1

# Save the cartnorm object
saveRDS(lm_cartnorm_model1, "./SyntheticData/Yue/lm_cartnorm_model1.rds")
saveRDS(compare_cartnorm_model1, "./SyntheticData/Yue/compare_cartnorm_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_cartnorm_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                 B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                 B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                 B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                 C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                 E2 + E3 + E4 + E7 + E5 + E6,
                                 data = sds_cartnorm_tryout)
summary(lm_cartnorm_model2)
compare_cartnorm_model2 <- compare(lm_cartnorm_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_cartnorm_model2
# Save the cartnorm object
saveRDS(lm_cartnorm_model2, "./SyntheticData/Yue/lm_cartnorm_model2.rds")
saveRDS(compare_cartnorm_model2, "./SyntheticData/Yue/compare_cartnorm_model2.rds")

rep_cartnorm <- replicated.uniques(sds_cartnorm_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_cartnorm, "./SyntheticData/Yue/rep_cartnorm.rds")

# ##########################################################
# ------------------ cartnormrank ----------------------------
# ##########################################################
# sds_cartnormrank_tryout <- syn_cart_experiment(para_weight_list, index=3, bindori_dataset_threshold_chr, arg_method, arg_col)
# load the syn object for cart normrank
sds_cartnormrank_tryout <- load("./SyntheticData/Yue/synobject_cart_normrank_syn.RData")
sds_cartnormrank_tryout <- object
# encode as integer
sds_cartnormrank_tryout$syn[[1]][,2:54] <- sapply(sds_cartnormrank_tryout$syn[[1]][,2:54],as.integer)
sds_cartnormrank_tryout$syn <- sds_cartnormrank_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_cartnormrank_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                     B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                     B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                     B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                     C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                     D1 + D2 + D3 + D4 + D5 + 
                                     E2 + E3 + E4 + E7 + E5 + E6,
                               data = sds_cartnormrank_tryout)
summary(lm_cartnormrank_model1)
compare_cartnormrank_model1 <- compare(lm_cartnormrank_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_cartnormrank_model1
# Save the cartnormrank object
saveRDS(lm_cartnormrank_model1, "./SyntheticData/Yue/lm_cartnormrank_model1.rds")
saveRDS(compare_cartnormrank_model1, "./SyntheticData/Yue/compare_cartnormrank_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_cartnormrank_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                     B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                     B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                     B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                     C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                     E2 + E3 + E4 + E7 + E5 + E6,
                               data = sds_cartnormrank_tryout)
summary(lm_cartnormrank_model2)
compare_cartnormrank_model2 <- compare(lm_cartnormrank_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_cartnormrank_model2
# Save the cartnormrank object
saveRDS(lm_cartnormrank_model2, "./SyntheticData/Yue/lm_cartnormrank_model2.rds")
saveRDS(compare_cartnormrank_model2, "./SyntheticData/Yue/compare_cartnormrank_model2.rds")

rep_cartnormrank <- replicated.uniques(sds_cartnormrank_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_cartnormrank, "./SyntheticData/Yue/rep_cartnormrank.rds")

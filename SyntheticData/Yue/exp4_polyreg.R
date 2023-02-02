##########################################################################
########---------------experiment with polyreg-------------------#########
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
# we have the dataframe here named as "bindori_dataset_threshold_chr"
# Encoding, var B2, 
# -1    -99 [0, 1) [1, 3) 
#  3 151336   2405 106555
#  1      2      3      4 
#  3 151336   2405 106555 
bindori_dataset_threshold_chr$B2 <- as.factor(bindori_dataset_threshold_chr$B2)
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

syn_polyreg_experiment <- function(para_weight_list, index, bindori_dataset_threshold_chr, arg_method, arg_col) {
  ### specify the method to use for group of vars ###
  # E3 and weight
  arg_method[['E3']] <- "sample"
  arg_method[['weight']] <- para_weight_list[index]  # sample(1), norm(2), normrank(3) to choose
  # E2, E4, E5, E7, ranger for exp_polyreg
  arg_method[['E2']] <- "polyreg"
  arg_method[['E4']] <- "polyreg"
  arg_method[['E5']] <- "polyreg"
  arg_method[['E7']] <- "polyreg"
  # B3 to B11, ranger
  arg_method[['B3']] <- "polyreg"
  arg_method[['B4']] <- "polyreg"
  arg_method[['B5']] <- "polyreg"
  arg_method[['B6']] <- "polyreg"
  arg_method[['B7']] <- "polyreg"
  arg_method[['B8']] <- "polyreg"
  arg_method[['B9']] <- "polyreg"
  arg_method[['B10']] <- "polyreg"
  arg_method[['B11']] <- "polyreg"
  # C3, C8
  arg_method[['C3']] <- "polyreg"
  arg_method[['C8']] <- "polyreg"
  # C2, C4, C5, C6
  arg_method[['C2']] <- "polyreg"
  arg_method[['C4']] <- "polyreg"
  arg_method[['C5']] <- "polyreg"
  arg_method[['C6']] <- "polyreg"
  # D1, D2, D4, D5
  arg_method[['D1']] <- "polyreg"
  arg_method[['D2']] <- "polyreg"
  arg_method[['D4']] <- "polyreg"
  arg_method[['D5']] <- "polyreg"
  
  syn_dataset <- NULL
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:54,1)], visit.sequence = arg_col[c(2:54, 1)], polyreg.maxit=10000)
  
  write.syn(syn_dataset, filename = paste("polyreg", para_weight_list[index], "syn", sep="_"), filetype = "rda")
  message("syn done!")
}

# tryout for polyreg_sample
sds_polyregsample_tryout <- syn_polyreg_experiment(para_weight_list, index=1, bindori_dataset_threshold_chr, arg_method, arg_col)
# tryout for polyreg_norm
sds_polyregnorm_tryout <- syn_polyreg_experiment(para_weight_list, index=2, bindori_dataset_threshold_chr, arg_method, arg_col)
# tryout for polyreg_normrank
sds_polyregnormrank_tryout <- syn_polyreg_experiment(para_weight_list, index=3, bindori_dataset_threshold_chr, arg_method, arg_col)


# ##########################################################
# ------------------ polyregsample ----------------------------
# ##########################################################
sds_polyregsample_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_polyreg_sample_syn.RData")
sds_polyregsample_tryout <- object
# encode variables as integers for ods and polyregsample
bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_polyregsample_tryout$syn[[1]][,2:54] <- sapply(sds_polyregsample_tryout$syn[[1]][,2:54],as.integer)

sds_polyregsample_tryout$syn <- sds_polyregsample_tryout$syn[[1]]
#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_polyregsample_model1 <- lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                                 data = sds_polyregsample_tryout)
summary(lm_polyregsample_model1)
compare_polyregsample_model1 <- compare(lm_polyregsample_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the polyregsample object
saveRDS(lm_polyregsample_model1, "./SyntheticData/Yue/lm_polyregsample_model1.rds")
saveRDS(compare_polyregsample_model1, "./SyntheticData/Yue/compare_polyregsample_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_polyregsample_model2 <- lm.synds(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                                 data = sds_polyregsample_tryout)
summary(lm_polyregsample_model2)
compare_polyregsample_model2 <- compare(lm_polyregsample_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the polyregsample object
saveRDS(lm_polyregsample_model2, "./SyntheticData/Yue/lm_polyregsample_model2.rds")
saveRDS(compare_polyregsample_model2, "./SyntheticData/Yue/compare_polyregsample_model2.rds")

rep_polyregsample <- replicated.uniques(sds_polyregsample_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_polyregsample, "./SyntheticData/Yue/rep_polyregsample.rds")

# ##########################################################
# ------------------ polyregnorm ----------------------------
# ##########################################################

# load the syn object for polyreg norm
sds_polyregnorm_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_polyreg_norm_syn.RData")
sds_polyregnorm_tryout <- object
# encode as integer
sds_polyregnorm_tryout$syn[[1]][,2:54] <- sapply(sds_polyregnorm_tryout$syn[[1]][,2:54],as.integer)
sds_polyregnorm_tryout$syn <- sds_polyregnorm_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_polyregnorm_model1 <- lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                               data = sds_polyregnorm_tryout)
summary(lm_polyregnorm_model1)
compare_polyregnorm_model1 <- compare(lm_polyregnorm_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the polyregnorm object
saveRDS(lm_polyregnorm_model1, "./SyntheticData/Yue/lm_polyregnorm_model1.rds")
saveRDS(compare_polyregnorm_model1, "./SyntheticData/Yue/compare_polyregnorm_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_polyregnorm_model2 <- lm.synds(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                               data = sds_polyregnorm_tryout)
summary(lm_polyregnorm_model2)
compare_polyregnorm_model2 <- compare(lm_polyregnorm_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the polyregnorm object
saveRDS(lm_polyregnorm_model2, "./SyntheticData/Yue/lm_polyregnorm_model2.rds")
saveRDS(compare_polyregnorm_model2, "./SyntheticData/Yue/compare_polyregnorm_model2.rds")

rep_polyregnorm <- replicated.uniques(sds_polyregnorm_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_polyregnorm, "./SyntheticData/Yue/rep_polyregnorm.rds")

# ##########################################################
# ------------------ polyregnormrank ----------------------------
# ##########################################################
# sds_polyregnormrank_tryout <- syn_polyreg_experiment(para_weight_list, index=3, bindori_dataset_threshold_chr, arg_method, arg_col)
# load the syn object for polyreg normrank
sds_polyregnormrank_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_polyreg_normrank_syn.RData")
sds_polyregnormrank_tryout <- object
# encode as integer
sds_polyregnormrank_tryout$syn[[1]][,2:54] <- sapply(sds_polyregnormrank_tryout$syn[[1]][,2:54],as.integer)
sds_polyregnormrank_tryout$syn <- sds_polyregnormrank_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_polyregnormrank_model1 <- lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                                   data = sds_polyregnormrank_tryout)
summary(lm_polyregnormrank_model1)
compare_polyregnormrank_model1 <- compare(lm_polyregnormrank_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the polyregnormrank object
saveRDS(lm_polyregnormrank_model1, "./SyntheticData/Yue/lm_polyregnormrank_model1.rds")
saveRDS(compare_polyregnormrank_model1, "./SyntheticData/Yue/compare_polyregnormrank_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_polyregnormrank_model2 <- lm.synds(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                                   data = sds_polyregnormrank_tryout)
summary(lm_polyregnormrank_model2)
compare_polyregnormrank_model2 <- compare(lm_polyregnormrank_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the polyregnormrank object
saveRDS(lm_polyregnormrank_model2, "./SyntheticData/Yue/lm_polyregnormrank_model2.rds")
saveRDS(compare_polyregnormrank_model2, "./SyntheticData/Yue/compare_polyregnormrank_model2.rds")

rep_polyregnormrank <- replicated.uniques(sds_polyregnormrank_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_polyregnormrank, "./SyntheticData/Yue/rep_polyregnormrank.rds")

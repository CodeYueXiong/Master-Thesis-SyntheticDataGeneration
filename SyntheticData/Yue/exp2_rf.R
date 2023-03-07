##########################################################################
#####---------------experiment with random forest-------------------######
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
# wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
wd <- "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
# wd <- "/dss/dsshome1/0C/ru27req2/Master-Thesis-DifferentialPrivacy"
# wd <- "/Volumes/ru27req/MasterThesisRoxy/Master-Thesis-DifferentialPrivacy"
setwd(wd)
wd <- "/Users/roxy/Desktop/Master-Thesis-SyntheticDataGeneration"
setwd(wd)

# load the preprocessed original data
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)

# load the preprocessed original data
load("ods_preprocess_0802.rda")
str(ods_threshold_0802)
# we have the dataframe here named as "ods_threshold_0802", encode not needed for rf except E6
# Encoding, var B2, 
# -1    -99 [0, 1) [1, 3) 
#  3 151336   2405 106555
#  1      2      3      4 
#  3 151336   2405 106555 
# ods_threshold_0802$B2 <- as.integer(ods_threshold_0802$B2)
# var B4,
#    -99 [0, 1) [1, 5)
# 249422    290  10587
#      1      2      3 
# 249422    290  10587 
# ods_threshold_0802$B4 <- as.integer(ods_threshold_0802$B4)
# var E5,
#   -99 [0, 1) [1, 2) 
# 45595   8766 205938 
#     1      2      3 
# 45595   8766 205938
# ods_threshold_0802$E5 <- as.integer(ods_threshold_0802$E5)
# var E6,
#   -99 [0, 9) 
# 57585 202714 
#     1      2 
# 57585 202714  
ods_threshold_0802$E6 <- as.integer(ods_threshold_0802$E6)

# also, we can probably subset those columns with constant inputs
cols_remove <- c("B13_1", "B13_2", "B13_3", "B13_4",
                 "B13_5", "B13_6", "B13_7",
                 "B14_1", "B14_2", "B14_3", "B14_4", "B14_5",
                 "D6_1", "D6_2", "D6_3", "F3_de")
ods_threshold_0802 <- ods_threshold_0802[, !(names(ods_threshold_0802) %in% cols_remove)]
# also for those B1b_x like vars and D10, we try exclude them from the synthesis
cols_rm_bd <- c("B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
                "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11","B1b_x12", "B1b_x13", "D10",
                "C0_1", "C0_2", "C0_3", "C0_4", "C0_5", "C0_6")

ods_threshold_0802 <- ods_threshold_0802[, !(names(ods_threshold_0802) %in% cols_rm_bd)]
ncol(ods_threshold_0802)==54
nrow(ods_threshold_0802)

# also, we can probably subset those columns with constant inputs
cols_remove <- c("B13_1", "B13_2", "B13_3", "B13_4",
                 "B13_5", "B13_6", "B13_7",
                 "B14_1", "B14_2", "B14_3", "B14_4", "B14_5",
                 "D6_1", "D6_2", "D6_3", "F3_de")
bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr[, !(names(bindori_dataset_threshold_chr) %in% cols_remove)]
# also for those B1b_x like vars and D10, we try exclude them from the synthesis
cols_rm_bd <- c("B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
                "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11","B1b_x12", "B1b_x13", "D10",
                "C0_1", "C0_2", "C0_3", "C0_4", "C0_5", "C0_6")

bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr[, !(names(bindori_dataset_threshold_chr) %in% cols_rm_bd)]
ncol(bindori_dataset_threshold_chr)==54
# decrease the size by randomly subsetting 5000 instances from the dataset
# bindori_dataset_threshold_chr[sample(nrow(real_data), size = 1000),]

##########################################################################
######---------------synthetic data with synthpop-------------------######
##########################################################################

# first of all, we try extract all the methods with m=0 set in cart
settings_default <- syn(ods_threshold_0802, method = "cart", m = 0)
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

syn_ranger_experiment <- function(para_weight_list, index, bindori_dataset_threshold_chr, arg_method, arg_col) {
  ### specify the method to use for group of vars ###
  # E3 and weight
  arg_method[['E3']] <- "sample"
  arg_method[['weight']] <- para_weight_list[index]  # sample(1), norm(2), normrank(3) to choose
  # E2, E4, E5, E7, ranger for exp_ranger
  arg_method[['E2']] <- "rf"
  arg_method[['E4']] <- "rf"
  arg_method[['E5']] <- "rf"
  arg_method[['E7']] <- "rf"
  # B3 to B11, rf
  arg_method[['B3']] <- "rf"
  arg_method[['B4']] <- "rf"
  arg_method[['B5']] <- "rf"
  arg_method[['B6']] <- "rf"
  arg_method[['B7']] <- "rf"
  arg_method[['B8']] <- "rf"
  arg_method[['B9']] <- "rf"
  arg_method[['B10']] <- "rf"
  arg_method[['B11']] <- "rf"
  # C3, C8
  arg_method[['C3']] <- "rf"
  arg_method[['C8']] <- "rf"
  # C2, C4, C5, C6
  arg_method[['C2']] <- "rf"
  arg_method[['C4']] <- "rf"
  arg_method[['C5']] <- "rf"
  arg_method[['C6']] <- "rf"
  # D1, D2, D4, D5
  arg_method[['D1']] <- "rf"
  arg_method[['D2']] <- "rf"
  arg_method[['D4']] <- "rf"
  arg_method[['D5']] <- "rf"
  
  syn_dataset <- NULL
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:54,1)], visit.sequence = arg_col[c(2:54, 1)], rf.ntree=2)
  
  write.syn(syn_dataset, filename = paste("rf0802", para_weight_list[index], "syn", sep="_"), filetype = "rda")
  message("syn done!")
}

# # tryout for ranger_sample
sds_rangersample_tryout <- syn_ranger_experiment(para_weight_list, index=1, ods_threshold_0802, arg_method, arg_col)
# # tryout for ranger_norm
sds_rangernorm_tryout <- syn_ranger_experiment(para_weight_list, index=2, ods_threshold_0802, arg_method, arg_col)
# # tryout for ranger_normrank
# sds_rangernormrank_tryout <- syn_ranger_experiment(para_weight_list, index=3, ods_threshold_0802, arg_method, arg_col)

# ##########################################################
# ------------------ rfsample ----------------------------
# ##########################################################
# 0802
sds_rfsample0802_tryout <- load("./SyntheticData/Yue/synobject_rf0802_sample_syn.RData")
sds_rfsample0802_tryout <- object
sds_rfsample0802_tryout$syn[[1]][,2:54] <- sapply(sds_rfsample0802_tryout$syn[[1]][,2:54],as.integer)
sds_rfsample0802_tryout$syn <- sds_rfsample0802_tryout$syn[[1]]
# 0803
sds_rfsample0803_tryout <- load("./SyntheticData/Yue/synobject_rf0803_sample_syn.RData")
sds_rfsample0803_tryout <- object
sds_rfsample0803_tryout$syn[[1]][,2:54] <- sapply(sds_rfsample0803_tryout$syn[[1]][,2:54],as.integer)
sds_rfsample0803_tryout$syn <- sds_rfsample0803_tryout$syn[[1]]
# 0804
sds_rfsample0804_tryout <- load("./SyntheticData/Yue/synobject_rf0804_sample_syn.RData")
sds_rfsample0804_tryout <- object
sds_rfsample0804_tryout$syn[[1]][,2:54] <- sapply(sds_rfsample0804_tryout$syn[[1]][,2:54],as.integer)
sds_rfsample0804_tryout$syn <- sds_rfsample0804_tryout$syn[[1]]
# 0805
sds_rfsample0805_tryout <- load("./SyntheticData/Yue/synobject_rf0805_sample_syn.RData")
sds_rfsample0805_tryout <- object
sds_rfsample0805_tryout$syn[[1]][,2:54] <- sapply(sds_rfsample0805_tryout$syn[[1]][,2:54],as.integer)
sds_rfsample0805_tryout$syn <- sds_rfsample0805_tryout$syn[[1]]
# 0806
sds_rfsample0806_tryout <- load("./SyntheticData/Yue/synobject_rf0806_sample_syn.RData")
sds_rfsample0806_tryout <- object
sds_rfsample0806_tryout$syn[[1]][,2:54] <- sapply(sds_rfsample0806_tryout$syn[[1]][,2:54],as.integer)
sds_rfsample0806_tryout$syn <- sds_rfsample0806_tryout$syn[[1]]
# 0807
sds_rfsample0807_tryout <- load("./SyntheticData/Yue/synobject_rf0807_sample_syn.RData")
sds_rfsample0807_tryout <- object
sds_rfsample0807_tryout$syn[[1]][,2:54] <- sapply(sds_rfsample0807_tryout$syn[[1]][,2:54],as.integer)
sds_rfsample0807_tryout$syn <- sds_rfsample0807_tryout$syn[[1]]
# 0808
sds_rfsample0808_tryout <- load("./SyntheticData/Yue/synobject_rf0808_sample_syn.RData")
sds_rfsample0808_tryout <- object
sds_rfsample0808_tryout$syn[[1]][,2:54] <- sapply(sds_rfsample0808_tryout$syn[[1]][,2:54],as.integer)
sds_rfsample0808_tryout$syn <- sds_rfsample0808_tryout$syn[[1]]

# concatenate the rfsample synthetic datasets
list_sds_rfsample_tryout <- list(sds_rfsample0802_tryout$syn, sds_rfsample0803_tryout$syn, 
                                  sds_rfsample0804_tryout$syn, sds_rfsample0805_tryout$syn, 
                                  sds_rfsample0806_tryout$syn, sds_rfsample0807_tryout$syn, sds_rfsample0808_tryout$syn)

# Concatenate the dataframes vertically
sds_rfsample_tryout <- sds_rfsample0802_tryout
sds_rfsample_tryout$syn <- do.call(rbind, list_sds_rfsample_tryout)
# encode as integer
bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_rfsample_tryout$syn[,2:54] <- sapply(sds_rfsample_tryout$syn[,2:54],as.integer)

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_rfsample_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                 B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                 B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                 B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                 C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                 D1 + D2 + D3 + D4 + D5 + 
                                 E2 + E3 + E4 + E7 + E5 + E6,
                                data = sds_rfsample_tryout)
summary(lm_rfsample_model1)
compare_rfsample_model1 <- compare(lm_rfsample_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_rfsample_model1
# Save the rfsample object
saveRDS(lm_rfsample_model1, "./SyntheticData/Yue/lm_rfsample_model1.rds")
saveRDS(compare_rfsample_model1, "./SyntheticData/Yue/compare_rfsample_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_rfsample_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                 B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                 B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                 B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                 C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                 E2 + E3 + E4 + E7 + E5 + E6,
                                data = sds_rfsample_tryout)
summary(lm_rfsample_model2)
compare_rfsample_model2 <- compare(lm_rfsample_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_rfsample_model2
# Save the rfsample object
saveRDS(lm_rfsample_model2, "./SyntheticData/Yue/lm_rfsample_model2.rds")
saveRDS(compare_rfsample_model2, "./SyntheticData/Yue/compare_rfsample_model2.rds")

rep_rfsample <- replicated.uniques(sds_rfsample_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_rfsample, "./SyntheticData/Yue/rep_rfsample.rds")

# ##########################################################
# ------------------ rfnorm ----------------------------
# ##########################################################
# 0802
sds_rfnorm0802_tryout <- load("./SyntheticData/Yue/synobject_rf0802_norm_syn.RData")
sds_rfnorm0802_tryout <- object
sds_rfnorm0802_tryout$syn[[1]][,2:54] <- sapply(sds_rfnorm0802_tryout$syn[[1]][,2:54],as.integer)
sds_rfnorm0802_tryout$syn <- sds_rfnorm0802_tryout$syn[[1]]
# 0803
sds_rfnorm0803_tryout <- load("./SyntheticData/Yue/synobject_rf0803_norm_syn.RData")
sds_rfnorm0803_tryout <- object
sds_rfnorm0803_tryout$syn[[1]][,2:54] <- sapply(sds_rfnorm0803_tryout$syn[[1]][,2:54],as.integer)
sds_rfnorm0803_tryout$syn <- sds_rfnorm0803_tryout$syn[[1]]
# 0804
sds_rfnorm0804_tryout <- load("./SyntheticData/Yue/synobject_rf0804_norm_syn.RData")
sds_rfnorm0804_tryout <- object
sds_rfnorm0804_tryout$syn[[1]][,2:54] <- sapply(sds_rfnorm0804_tryout$syn[[1]][,2:54],as.integer)
sds_rfnorm0804_tryout$syn <- sds_rfnorm0804_tryout$syn[[1]]
# 0805
sds_rfnorm0805_tryout <- load("./SyntheticData/Yue/synobject_rf0805_norm_syn.RData")
sds_rfnorm0805_tryout <- object
sds_rfnorm0805_tryout$syn[[1]][,2:54] <- sapply(sds_rfnorm0805_tryout$syn[[1]][,2:54],as.integer)
sds_rfnorm0805_tryout$syn <- sds_rfnorm0805_tryout$syn[[1]]
# 0806
sds_rfnorm0806_tryout <- load("./SyntheticData/Yue/synobject_rf0806_norm_syn.RData")
sds_rfnorm0806_tryout <- object
sds_rfnorm0806_tryout$syn[[1]][,2:54] <- sapply(sds_rfnorm0806_tryout$syn[[1]][,2:54],as.integer)
sds_rfnorm0806_tryout$syn <- sds_rfnorm0806_tryout$syn[[1]]
# 0807
sds_rfnorm0807_tryout <- load("./SyntheticData/Yue/synobject_rf0807_norm_syn.RData")
sds_rfnorm0807_tryout <- object
sds_rfnorm0807_tryout$syn[[1]][,2:54] <- sapply(sds_rfnorm0807_tryout$syn[[1]][,2:54],as.integer)
sds_rfnorm0807_tryout$syn <- sds_rfnorm0807_tryout$syn[[1]]
# 0808
sds_rfnorm0808_tryout <- load("./SyntheticData/Yue/synobject_rf0808_norm_syn.RData")
sds_rfnorm0808_tryout <- object
sds_rfnorm0808_tryout$syn[[1]][,2:54] <- sapply(sds_rfnorm0808_tryout$syn[[1]][,2:54],as.integer)
sds_rfnorm0808_tryout$syn <- sds_rfnorm0808_tryout$syn[[1]]

# concatenate the rfnorm synthetic datasets
list_sds_rfnorm_tryout <- list(sds_rfnorm0802_tryout$syn, sds_rfnorm0803_tryout$syn, 
                                sds_rfnorm0804_tryout$syn, sds_rfnorm0805_tryout$syn, 
                                sds_rfnorm0806_tryout$syn, sds_rfnorm0807_tryout$syn, sds_rfnorm0808_tryout$syn)

# Concatenate the dataframes vertically
sds_rfnorm_tryout <- sds_rfnorm0802_tryout
sds_rfnorm_tryout$syn <- do.call(rbind, list_sds_rfnorm_tryout)
# encode as integer
bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_rfnorm_tryout$syn[,2:54] <- sapply(sds_rfnorm_tryout$syn[,2:54],as.integer)

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_rfnorm_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                               B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                               B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                               B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                               C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                               D1 + D2 + D3 + D4 + D5 + 
                               E2 + E3 + E4 + E7 + E5 + E6,
                              data = sds_rfnorm_tryout)
summary(lm_rfnorm_model1)
compare_rfnorm_model1 <- compare(lm_rfnorm_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_rfnorm_model1
# Save the rfnorm object
saveRDS(lm_rfnorm_model1, "./SyntheticData/Yue/lm_rfnorm_model1.rds")
saveRDS(compare_rfnorm_model1, "./SyntheticData/Yue/compare_rfnorm_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_rfnorm_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                               B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                               B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                               B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                               C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                               E2 + E3 + E4 + E7 + E5 + E6,
                              data = sds_rfnorm_tryout)
summary(lm_rfnorm_model2)
compare_rfnorm_model2 <- compare(lm_rfnorm_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_rfnorm_model2
# Save the rfnorm object
saveRDS(lm_rfnorm_model2, "./SyntheticData/Yue/lm_rfnorm_model2.rds")
saveRDS(compare_rfnorm_model2, "./SyntheticData/Yue/compare_rfnorm_model2.rds")

rep_rfnorm <- replicated.uniques(sds_rfnorm_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_rfnorm, "./SyntheticData/Yue/rep_rfnorm.rds")

# ##########################################################
# ------------------ rfnormrank ----------------------------
# ##########################################################
# 0802
sds_rfnormrank0802_tryout <- load("./SyntheticData/Yue/synobject_rf0802_normrank_syn.RData")
sds_rfnormrank0802_tryout <- object
sds_rfnormrank0802_tryout$syn[[1]][,2:54] <- sapply(sds_rfnormrank0802_tryout$syn[[1]][,2:54],as.integer)
sds_rfnormrank0802_tryout$syn <- sds_rfnormrank0802_tryout$syn[[1]]
# 0803
sds_rfnormrank0803_tryout <- load("./SyntheticData/Yue/synobject_rf0803_normrank_syn.RData")
sds_rfnormrank0803_tryout <- object
sds_rfnormrank0803_tryout$syn[[1]][,2:54] <- sapply(sds_rfnormrank0803_tryout$syn[[1]][,2:54],as.integer)
sds_rfnormrank0803_tryout$syn <- sds_rfnormrank0803_tryout$syn[[1]]
# 0804
sds_rfnormrank0804_tryout <- load("./SyntheticData/Yue/synobject_rf0804_normrank_syn.RData")
sds_rfnormrank0804_tryout <- object
sds_rfnormrank0804_tryout$syn[[1]][,2:54] <- sapply(sds_rfnormrank0804_tryout$syn[[1]][,2:54],as.integer)
sds_rfnormrank0804_tryout$syn <- sds_rfnormrank0804_tryout$syn[[1]]
# 0805
sds_rfnormrank0805_tryout <- load("./SyntheticData/Yue/synobject_rf0805_normrank_syn.RData")
sds_rfnormrank0805_tryout <- object
sds_rfnormrank0805_tryout$syn[[1]][,2:54] <- sapply(sds_rfnormrank0805_tryout$syn[[1]][,2:54],as.integer)
sds_rfnormrank0805_tryout$syn <- sds_rfnormrank0805_tryout$syn[[1]]
# 0806
sds_rfnormrank0806_tryout <- load("./SyntheticData/Yue/synobject_rf0806_normrank_syn.RData")
sds_rfnormrank0806_tryout <- object
sds_rfnormrank0806_tryout$syn[[1]][,2:54] <- sapply(sds_rfnormrank0806_tryout$syn[[1]][,2:54],as.integer)
sds_rfnormrank0806_tryout$syn <- sds_rfnormrank0806_tryout$syn[[1]]
# 0807
sds_rfnormrank0807_tryout <- load("./SyntheticData/Yue/synobject_rf0807_normrank_syn.RData")
sds_rfnormrank0807_tryout <- object
sds_rfnormrank0807_tryout$syn[[1]][,2:54] <- sapply(sds_rfnormrank0807_tryout$syn[[1]][,2:54],as.integer)
sds_rfnormrank0807_tryout$syn <- sds_rfnormrank0807_tryout$syn[[1]]
# 0808
sds_rfnormrank0808_tryout <- load("./SyntheticData/Yue/synobject_rf0808_normrank_syn.RData")
sds_rfnormrank0808_tryout <- object
sds_rfnormrank0808_tryout$syn[[1]][,2:54] <- sapply(sds_rfnormrank0808_tryout$syn[[1]][,2:54],as.integer)
sds_rfnormrank0808_tryout$syn <- sds_rfnormrank0808_tryout$syn[[1]]

# concatenate the rfnormrank synthetic datasets
list_sds_rfnormrank_tryout <- list(sds_rfnormrank0802_tryout$syn, sds_rfnormrank0803_tryout$syn, 
                                    sds_rfnormrank0804_tryout$syn, sds_rfnormrank0805_tryout$syn, 
                                    sds_rfnormrank0806_tryout$syn, sds_rfnormrank0807_tryout$syn, sds_rfnormrank0808_tryout$syn)

# Concatenate the dataframes vertically
sds_rfnormrank_tryout <- sds_rfnormrank0802_tryout
sds_rfnormrank_tryout$syn <- do.call(rbind, list_sds_rfnormrank_tryout)
# encode as integer
# bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_rfnormrank_tryout$syn[,2:54] <- sapply(sds_rfnormrank_tryout$syn[,2:54],as.integer)

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_rfnormrank_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                   B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                   B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                   B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                   C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                   D1 + D2 + D3 + D4 + D5 + 
                                   E2 + E3 + E4 + E7 + E5 + E6,
                                  data = sds_rfnormrank_tryout)
summary(lm_rfnormrank_model1)
compare_rfnormrank_model1 <- compare(lm_rfnormrank_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_rfnormrank_model1
# Save the rfnormrank object
saveRDS(lm_rfnormrank_model1, "./SyntheticData/Yue/lm_rfnormrank_model1.rds")
saveRDS(compare_rfnormrank_model1, "./SyntheticData/Yue/compare_rfnormrank_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_rfnormrank_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                   B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                   B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                   B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                   C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                   E2 + E3 + E4 + E7 + E5 + E6,
                                  data = sds_rfnormrank_tryout)
summary(lm_rfnormrank_model2)
compare_rfnormrank_model2 <- compare(lm_rfnormrank_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_rfnormrank_model2
# Save the rfnormrank object
saveRDS(lm_rfnormrank_model2, "./SyntheticData/Yue/lm_rfnormrank_model2.rds")
saveRDS(compare_rfnormrank_model2, "./SyntheticData/Yue/compare_rfnormrank_model2.rds")

rep_rfnormrank <- replicated.uniques(sds_rfnormrank_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_rfnormrank, "./SyntheticData/Yue/rep_rfnormrank.rds")
compare_cartsample_model1$ci.overlap

coef_diff_compare_m1 <- data.frame(diff_cartsample_m1 = data.frame(compare_cartsample_model1$coef.diff)[4],
                                   diff_cartnorm_m1 = data.frame(compare_cartnorm_model1$coef.diff)[4],
                                   diff_cartnormrank_m1 = data.frame(compare_cartnormrank_model1$coef.diff)[4],
                                   diff_rfsample_m1 = data.frame(compare_rfsample_model1$coef.diff)[4],
                                   diff_rfnorm_m1 = data.frame(compare_rfnorm_model1$coef.diff)[4],
                                   diff_rfnormrank_m1 = data.frame(compare_rfnormrank_model1$coef.diff)[4],
                                   diff_bagsample_m1 = data.frame(compare_bagsample_model1$coef.diff)[4],
                                   diff_bagnorm_m1 = data.frame(compare_bagnorm_model1$coef.diff)[4],
                                   diff_bagnormrank_m1 = data.frame(compare_bagnormrank_model1$coef.diff)[4],
                                   diff_polyregsample_m1 = data.frame(compare_polyregsample_model1$coef.diff)[4],
                                   diff_polyregnorm_m1 = data.frame(compare_polyregnorm_model1$coef.diff)[4],
                                   diff_polyregnormrank_m1 = data.frame(compare_polyregnormrank_model1$coef.diff)[4],
                                   diff_normsample_m1 = data.frame(compare_normsample_model1$coef.diff)[4],
                                   diff_normnorm_m1 = data.frame(compare_normnorm_model1$coef.diff)[4],
                                   diff_normnormrank_m1 = data.frame(compare_normnormrank_model1$coef.diff)[4],
                                   diff_normranksample_m1 = data.frame(compare_normranksample_model1$coef.diff)[4],
                                   diff_normranknorm_m1 = data.frame(compare_normranknorm_model1$coef.diff)[4],
                                   diff_normranknormrank_m1 = data.frame(compare_normranknormrank_model1$coef.diff)[4],
                                   diff_terrancev1_m1 = data.frame(compare_terrancev1_model1$coef.diff)[4],
                                   diff_terrancev2_m1 = data.frame(compare_terrancev2_model1$coef.diff)[4])

coefpicked_diff_compare_m1 <- data.frame(
                                   diff_cartnorm_m1 = data.frame(compare_cartnorm_model1$coef.diff)[4],
                                  
                                   diff_rfnorm_m1 = data.frame(compare_rfnorm_model1$coef.diff)[4],
                                  
                                   diff_bagnorm_m1 = data.frame(compare_bagnorm_model1$coef.diff)[4],
                                   diff_polyregnorm_m1 = data.frame(compare_polyregnorm_model1$coef.diff)[4],
                                   
                                   diff_normnorm_m1 = data.frame(compare_normnorm_model1$coef.diff)[4],
                                   
                                   diff_normranknorm_m1 = data.frame(compare_normranknorm_model1$coef.diff)[4],
                                   
                                   diff_terrancev2_m1 = data.frame(compare_terrancev2_model1$coef.diff)[4]
                                   )

# List of new names
new_names <- c("cart1", "cart2", "cart3",
               "rf1", "rf2", "rf3",
               "bag1", "bag2", "bag3",
               "polyreg1", "polyreg2", "polyreg3",
               "norm1", "norm2", "norm3",
               "normrank1", "normrank2", "normrank3",
               "terrance1", "terrance2")
new_names <- c("cart2",
               "rf2",
               "bag2",
               "polyreg2",
               "norm2",
               "normrank2",
               "terrance2")

# Rename columns using the list of new names
coef_diff_compare_m1 <- coef_diff_compare_m1 %>% 
  rename_with(~ new_names, everything())

coefpicked_diff_compare_m1 <- coefpicked_diff_compare_m1 %>% 
  rename_with(~ new_names, everything())

# Define the order of the models
model_order <- names(coef_diff_compare_m1)
model_order <- names(coefpicked_diff_compare_m1)
# Convert to long format and set the factor levels
df_long <- coef_diff_compare_m1 %>%
  gather(key = "model", value = "value") %>%
  mutate(model = factor(model, levels = model_order))
df_long <- coefpicked_diff_compare_m1 %>%
  gather(key = "model", value = "value") %>%
  mutate(model = factor(model, levels = model_order))


ggplot(df_long, aes(x = model, y = value, fill = model)) +
  geom_boxplot() +
  scale_fill_hue(c = 60, l = 70, h = c(0, 360)) +
  labs(x = "Model1", y = "Standardized absolute difference in coefficients") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"))

coef_diff_compare_m2 <- data.frame(diff_cartsample_m2 = data.frame(compare_cartsample_model2$coef.diff)[4],
                                   diff_cartnorm_m2 = data.frame(compare_cartnorm_model2$coef.diff)[4],
                                   diff_cartnormrank_m2 = data.frame(compare_cartnormrank_model2$coef.diff)[4],
                                   diff_rfsample_m2 = data.frame(compare_rfsample_model2$coef.diff)[4],
                                   diff_rfnorm_m2 = data.frame(compare_rfnorm_model2$coef.diff)[4],
                                   diff_rfnormrank_m2 = data.frame(compare_rfnormrank_model2$coef.diff)[4],
                                   diff_bagsample_m2 = data.frame(compare_bagsample_model2$coef.diff)[4],
                                   diff_bagnorm_m2 = data.frame(compare_bagnorm_model2$coef.diff)[4],
                                   diff_bagnormrank_m2 = data.frame(compare_bagnormrank_model2$coef.diff)[4],
                                   diff_polyregsample_m2 = data.frame(compare_polyregsample_model2$coef.diff)[4],
                                   diff_polyregnorm_m2 = data.frame(compare_polyregnorm_model2$coef.diff)[4],
                                   diff_polyregnormrank_m2 = data.frame(compare_polyregnormrank_model2$coef.diff)[4],
                                   diff_normsample_m2 = data.frame(compare_normsample_model2$coef.diff)[4],
                                   diff_normnorm_m2 = data.frame(compare_normnorm_model2$coef.diff)[4],
                                   diff_normnormrank_m2 = data.frame(compare_normnormrank_model2$coef.diff)[4],
                                   diff_normranksample_m2 = data.frame(compare_normranksample_model2$coef.diff)[4],
                                   diff_normranknorm_m2 = data.frame(compare_normranknorm_model2$coef.diff)[4],
                                   diff_normranknormrank_m2 = data.frame(compare_normranknormrank_model2$coef.diff)[4],
                                   diff_terrancev1_m2 = data.frame(compare_terrancev1_model2$coef.diff)[4],
                                   diff_terrancev2_m2 = data.frame(compare_terrancev2_model2$coef.diff)[4])
ncol(coef_diff_compare_m2)
coefpicked_diff_compare_m2 <- data.frame(
                                   diff_cartnorm_m2 = data.frame(compare_cartnorm_model2$coef.diff)[4],
                                   
                                  
                                   diff_rfnorm_m2 = data.frame(compare_rfnorm_model2$coef.diff)[4],
                                   
                                   diff_bagsample_m2 = data.frame(compare_bagsample_model2$coef.diff)[4],
                                  
                                   
                                   diff_polyregnorm_m2 = data.frame(compare_polyregnorm_model2$coef.diff)[4],
                                   
                                   diff_normnormrank_m2 = data.frame(compare_normnormrank_model2$coef.diff)[4],
                                   diff_normranknormrank_m2 = data.frame(compare_normranknormrank_model2$coef.diff)[4],
                                   diff_terrancev1_m2 = data.frame(compare_terrancev1_model2$coef.diff)[4])
# List of new names
new_names <- c("cart1", "cart2", "cart3",
               "rf1", "rf2", "rf3",
               "bag1", "bag2", "bag3",
               "polyreg1", "polyreg2", "polyreg3",
               "norm1", "norm2", "norm3",
               "normrank1", "normrank2", "normrank3",
               "terrance1", "terrance2")
# List of new names
new_names <- c("cart2",
               "rf2",
               "bag1",
               "polyreg2",
               "norm3",
               "normrank3",
               "terrance1")
# Rename columns using the list of new names
coef_diff_compare_m2 <- coef_diff_compare_m2 %>% 
  rename_with(~ new_names, everything())
coefpicked_diff_compare_m2 <- coefpicked_diff_compare_m2 %>% 
  rename_with(~ new_names, everything())

# Define the order of the models
model_order <- names(coef_diff_compare_m2)
model_order <- names(coefpicked_diff_compare_m2)
# Convert to long format and set the factor levels
df_long <- coef_diff_compare_m2 %>%
  gather(key = "model", value = "value") %>%
  mutate(model = factor(model, levels = model_order))
df_long <- coefpicked_diff_compare_m2 %>%
  gather(key = "model", value = "value") %>%
  mutate(model = factor(model, levels = model_order))

ggplot(df_long, aes(x = model, y = value, fill = model)) +
  geom_boxplot() +
  scale_fill_hue(c = 60, l = 70, h = c(0, 360)) +
  labs(x = "Model2", y = "Standardized absolute difference in coefficients") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"))



coef_ci_compare_m1 <- data.frame(ci_cartsample_m1 = compare_cartsample_model1$ci.overlap,
                                 ci_cartnorm_m1 = compare_cartnorm_model1$ci.overlap,
                                 ci_cartnormrank_m1 = compare_cartnormrank_model1$ci.overlap,
                                 ci_rfsample_m1 = compare_rfsample_model1$ci.overlap,
                                 ci_rfnorm_m1 = compare_rfnorm_model1$ci.overlap,
                                 ci_rfnormrank_m1 = compare_rfnormrank_model1$ci.overlap,
                                 ci_bagsample_m1 = compare_bagsample_model1$ci.overlap,
                                 ci_bagnorm_m1 = compare_bagnorm_model1$ci.overlap,
                                 ci_bagnormrank_m1 = compare_bagnormrank_model1$ci.overlap,
                                 ci_polyregsample_m1 = compare_polyregsample_model1$ci.overlap,
                                 ci_polyregnorm_m1 = compare_polyregnorm_model1$ci.overlap,
                                 ci_polyregnormrank_m1 = compare_polyregnormrank_model1$ci.overlap,
                                 ci_normsample_m1 = compare_normsample_model1$ci.overlap,
                                 ci_normnorm_m1 = compare_normnorm_model1$ci.overlap,
                                 ci_normnormrank_m1 = compare_normnormrank_model1$ci.overlap,
                                 ci_normranksample_m1 = compare_normranksample_model1$ci.overlap,
                                 ci_normranknorm_m1 = compare_normranknorm_model1$ci.overlap,
                                 ci_normranknormrank_m1 = compare_normranknormrank_model1$ci.overlap,
                                 ci_terrancev1_m1 = compare_terrancev1_model1$ci.overlap,
                                 ci_terrancev2_m1 = compare_terrancev2_model1$ci.overlap)

coefpicked_ci_compare_m1 <- data.frame(
                                 ci_cartnorm_m1 = compare_cartnorm_model1$ci.overlap,
                                 
                                 ci_rfnorm_m1 = compare_rfnorm_model1$ci.overlap,
                                 
                                 ci_bagnorm_m1 = compare_bagnorm_model1$ci.overlap,
                                 
                                 ci_polyregnorm_m1 = compare_polyregnorm_model1$ci.overlap,
                                 
                                 ci_normnorm_m1 = compare_normnorm_model1$ci.overlap,
                                 
                                 ci_normranknorm_m1 = compare_normranknorm_model1$ci.overlap,
                                 
                                 ci_terrancev2_m1 = compare_terrancev2_model1$ci.overlap)
ncol(coef_ci_compare_m1)
# List of new names
new_names <- c("cart1", "cart2", "cart3",
               "rf1", "rf2", "rf3",
               "bag1", "bag2", "bag3",
               "polyreg1", "polyreg2", "polyreg3",
               "norm1", "norm2", "norm3",
               "normrank1", "normrank2", "normrank3",
               "terrance1", "terrance2")
new_names <- c("cart2",
               "rf2",
               "bag2",
               "polyreg2",
               "norm2",
               "normrank2",
               "terrance2")
# Rename columns using the list of new names
coef_ci_compare_m1 <- coef_ci_compare_m1 %>% 
  rename_with(~ new_names, everything())

coefpicked_ci_compare_m1 <- coefpicked_ci_compare_m1 %>% 
  rename_with(~ new_names, everything())

# Define the order of the models
model_order <- names(coef_ci_compare_m1)
model_order <- names(coefpicked_ci_compare_m1)
# Convert to long format and set the factor levels
df_long <- coef_ci_compare_m1 %>%
  gather(key = "model", value = "value") %>%
  mutate(model = factor(model, levels = model_order))
df_long <- coefpicked_ci_compare_m1 %>%
  gather(key = "model", value = "value") %>%
  mutate(model = factor(model, levels = model_order))

ggplot(df_long, aes(x = model, y = value, fill = model)) +
  geom_boxplot() +
  scale_fill_hue(c = 60, l = 70, h = c(0, 360)) +
  labs(x = "Model1", y = "95% CI overlap") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"))

coef_ci_compare_m1 <- data.frame(ci_cartsample_m1 = compare_cartsample_model2$ci.overlap,
                                 ci_cartnorm_m1 = compare_cartnorm_model2$ci.overlap,
                                 ci_cartnormrank_m1 = compare_cartnormrank_model2$ci.overlap,
                                 ci_rfsample_m1 = compare_rfsample_model2$ci.overlap,
                                 ci_rfnorm_m1 = compare_rfnorm_model2$ci.overlap,
                                 ci_rfnormrank_m1 = compare_rfnormrank_model2$ci.overlap,
                                 ci_bagsample_m1 = compare_bagsample_model2$ci.overlap,
                                 ci_bagnorm_m1 = compare_bagnorm_model2$ci.overlap,
                                 ci_bagnormrank_m1 = compare_bagnormrank_model2$ci.overlap,
                                 ci_polyregsample_m1 = compare_polyregsample_model2$ci.overlap,
                                 ci_polyregnorm_m1 = compare_polyregnorm_model2$ci.overlap,
                                 ci_polyregnormrank_m1 = compare_polyregnormrank_model2$ci.overlap,
                                 ci_normsample_m1 = compare_normsample_model2$ci.overlap,
                                 ci_normnorm_m1 = compare_normnorm_model2$ci.overlap,
                                 ci_normnormrank_m1 = compare_normnormrank_model2$ci.overlap,
                                 ci_normranksample_m1 = compare_normranksample_model2$ci.overlap,
                                 ci_normranknorm_m1 = compare_normranknorm_model2$ci.overlap,
                                 ci_normranknormrank_m1 = compare_normranknormrank_model2$ci.overlap,
                                 ci_terrancev1_m1 = compare_terrancev1_model2$ci.overlap,
                                 ci_terrancev2_m1 = compare_terrancev2_model2$ci.overlap)
coef_ci_compare_m2 <- data.frame(
                                 ci_cartnorm_m2 = compare_cartnorm_model2$ci.overlap,
                                 ci_rfnorm_m2 = compare_rfnorm_model2$ci.overlap,
                                 
                                 ci_bagsample_m2 = compare_bagsample_model2$ci.overlap,
                                 
                                 ci_polyregnorm_m2 = compare_polyregnorm_model2$ci.overlap,
                                 
                                 ci_normnormrank_m2 = compare_normnormrank_model2$ci.overlap,
                                 
                                 ci_normranknormrank_m2 = compare_normranknormrank_model2$ci.overlap,
                                 ci_terrancev1_m2 = compare_terrancev1_model2$ci.overlap
                                 )

# List of new names
new_names <- c("cart1", "cart2", "cart3",
               "rf1", "rf2", "rf3",
               "bag1", "bag2", "bag3",
               "polyreg1", "polyreg2", "polyreg3",
               "norm1", "norm2", "norm3",
               "normrank1", "normrank2", "normrank3",
               "terrance1", "terrance2")


new_names <- c("cart2",
               "rf2",
               "bag1",
               "polyreg2",
               "norm3",
               "normrank3",
               "terrance1")

# Rename columns using the list of new names
coef_ci_compare_m2 <- coef_ci_compare_m2 %>% 
  rename_with(~ new_names, everything())

# Define the order of the models
model_order <- names(coef_ci_compare_m2)

# Convert to long format and set the factor levels
df_long <- coef_ci_compare_m2 %>%
  gather(key = "model", value = "value") %>%
  mutate(model = factor(model, levels = model_order))

ggplot(df_long, aes(x = model, y = value, fill = model)) +
  geom_boxplot() +
  scale_fill_hue(c = 60, l = 70, h = c(0, 360)) +
  labs(x = "Model2", y = "95% CI overlap") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"))

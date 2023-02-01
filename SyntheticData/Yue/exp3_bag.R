##########################################################################
########---------------experiment with bagging-------------------#########
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
load("ods_preprocess_0808.rda")
str(ods_threshold_0808)

# load the preprocessed original data
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)
# we have the dataframe here named as "ods_threshold_0802"
# Encoding, var B2, 
# -1    -99 [0, 1) [1, 3) 
#  3 151336   2405 106555
#  1      2      3      4 
#  3 151336   2405 106555 
# ods_threshold_0805$B2 <- as.integer(ods_threshold_0805$B2)
# var B4,
#    -99 [0, 1) [1, 5)
# 249422    290  10587
#      1      2      3 
# 249422    290  10587 
# ods_threshold_0805$B4 <- as.integer(ods_threshold_0805$B4)
# var E5,
#   -99 [0, 1) [1, 2) 
# 45595   8766 205938 
#     1      2      3 
# 45595   8766 205938
# ods_threshold_0805$E5 <- as.integer(ods_threshold_0805$E5)
# var E6,
#   -99 [0, 9) 
# 57585 202714 
#     1      2 
# 57585 202714  
# ods_threshold_0808$E6 <- as.integer(ods_threshold_0808$E6)
# 
# # also, we can probably subset those columns with constant inputs
# cols_remove <- c("B13_1", "B13_2", "B13_3", "B13_4",
#                  "B13_5", "B13_6", "B13_7",
#                  "B14_1", "B14_2", "B14_3", "B14_4", "B14_5",
#                  "D6_1", "D6_2", "D6_3", "F3_de")
# ods_threshold_0808 <- ods_threshold_0808 %>% select(-all_of(cols_remove))
# # also for those B1b_x like vars and D10, we try exclude them from the synthesis
# cols_rm_bd <- c("B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
#                 "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11","B1b_x12", "B1b_x13", "D10",
#                 "C0_1", "C0_2", "C0_3", "C0_4", "C0_5", "C0_6")
# 
# ods_threshold_0808 <- ods_threshold_0808 %>% select(-all_of(cols_rm_bd))
# ncol(ods_threshold_0808)==54
# nrow(ods_threshold_0808)

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
settings_default <- syn(ods_threshold_0808, method = "cart", m = 0)
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

syn_bag_experiment <- function(para_weight_list, index, bindori_dataset_threshold_chr, arg_method, arg_col) {
  ### specify the method to use for group of vars ###
  # E3 and weight
  arg_method[['E3']] <- "sample"
  arg_method[['weight']] <- para_weight_list[index]  # sample(1), norm(2), normrank(3) to choose
  # E2, E4, E5, E7, ranger for exp_bag
  arg_method[['E2']] <- "bag"
  arg_method[['E4']] <- "bag"
  arg_method[['E5']] <- "bag"
  arg_method[['E7']] <- "bag"
  # B3 to B11, ranger
  arg_method[['B3']] <- "bag"
  arg_method[['B4']] <- "bag"
  arg_method[['B5']] <- "bag"
  arg_method[['B6']] <- "bag"
  arg_method[['B7']] <- "bag"
  arg_method[['B8']] <- "bag"
  arg_method[['B9']] <- "bag"
  arg_method[['B10']] <- "bag"
  arg_method[['B11']] <- "bag"
  # C3, C8
  arg_method[['C3']] <- "bag"
  arg_method[['C8']] <- "bag"
  # C2, C4, C5, C6
  arg_method[['C2']] <- "bag"
  arg_method[['C4']] <- "bag"
  arg_method[['C5']] <- "bag"
  arg_method[['C6']] <- "bag"
  # D1, D2, D4, D5
  arg_method[['D1']] <- "bag"
  arg_method[['D2']] <- "bag"
  arg_method[['D4']] <- "bag"
  arg_method[['D5']] <- "bag"
  
  syn_dataset <- NULL
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:54,1)], visit.sequence = arg_col[c(2:54, 1)], bag.ntree=2)
  
  write.syn(syn_dataset, filename = paste("bag0808", para_weight_list[index], "syn", sep="_"), filetype = "rda")
  message("syn done!")
}

# ##########################################################
# ------------------ bagsample ----------------------------
# ##########################################################
# 0802
sds_bagsample0802_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0802_sample_syn.RData")
sds_bagsample0802_tryout <- object
sds_bagsample0802_tryout$syn[[1]][,2:54] <- sapply(sds_bagsample0802_tryout$syn[[1]][,2:54],as.integer)
sds_bagsample0802_tryout$syn <- sds_bagsample0802_tryout$syn[[1]]
# 0803
sds_bagsample0803_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0803_sample_syn.RData")
sds_bagsample0803_tryout <- object
sds_bagsample0803_tryout$syn[[1]][,2:54] <- sapply(sds_bagsample0803_tryout$syn[[1]][,2:54],as.integer)
sds_bagsample0803_tryout$syn <- sds_bagsample0803_tryout$syn[[1]]
# 0804
sds_bagsample0804_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0804_sample_syn.RData")
sds_bagsample0804_tryout <- object
sds_bagsample0804_tryout$syn[[1]][,2:54] <- sapply(sds_bagsample0804_tryout$syn[[1]][,2:54],as.integer)
sds_bagsample0804_tryout$syn <- sds_bagsample0804_tryout$syn[[1]]
# 0805
sds_bagsample0805_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0805_sample_syn.RData")
sds_bagsample0805_tryout <- object
sds_bagsample0805_tryout$syn[[1]][,2:54] <- sapply(sds_bagsample0805_tryout$syn[[1]][,2:54],as.integer)
sds_bagsample0805_tryout$syn <- sds_bagsample0805_tryout$syn[[1]]
# 0806
sds_bagsample0806_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0806_sample_syn.RData")
sds_bagsample0806_tryout <- object
sds_bagsample0806_tryout$syn[[1]][,2:54] <- sapply(sds_bagsample0806_tryout$syn[[1]][,2:54],as.integer)
sds_bagsample0806_tryout$syn <- sds_bagsample0806_tryout$syn[[1]]
# 0807
sds_bagsample0807_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0807_sample_syn.RData")
sds_bagsample0807_tryout <- object
sds_bagsample0807_tryout$syn[[1]][,2:54] <- sapply(sds_bagsample0807_tryout$syn[[1]][,2:54],as.integer)
sds_bagsample0807_tryout$syn <- sds_bagsample0807_tryout$syn[[1]]
# 0808
sds_bagsample0808_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0808_sample_syn.RData")
sds_bagsample0808_tryout <- object
sds_bagsample0808_tryout$syn[[1]][,2:54] <- sapply(sds_bagsample0808_tryout$syn[[1]][,2:54],as.integer)
sds_bagsample0808_tryout$syn <- sds_bagsample0808_tryout$syn[[1]]

# concatenate the bagsample synthetic datasets
list_sds_bagsample_tryout <- list(sds_bagsample0802_tryout$syn, sds_bagsample0803_tryout$syn, 
                    sds_bagsample0804_tryout$syn, sds_bagsample0805_tryout$syn, 
                    sds_bagsample0806_tryout$syn, sds_bagsample0807_tryout$syn, sds_bagsample0808_tryout$syn)

# Concatenate the dataframes vertically
sds_bagsample_tryout <- sds_bagsample0802_tryout
sds_bagsample_tryout$syn <- do.call(rbind, list_sds_bagsample_tryout)
# encode as integer
bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_bagsample_tryout$syn[,2:54] <- sapply(sds_bagsample_tryout$syn[,2:54],as.integer)

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_bagsample_model1 <- lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                               data = sds_bagsample_tryout)
summary(lm_bagsample_model1)
compare_bagsample_model1 <- compare(lm_bagsample_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the bagsample object
saveRDS(lm_bagsample_model1, "./SyntheticData/Yue/lm_bagsample_model1.rds")
saveRDS(compare_bagsample_model1, "./SyntheticData/Yue/compare_bagsample_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_bagsample_model2 <- lm.synds(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                               data = sds_bagsample_tryout)
summary(lm_bagsample_model2)
compare_bagsample_model2 <- compare(lm_bagsample_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the bagsample object
saveRDS(lm_bagsample_model2, "./SyntheticData/Yue/lm_bagsample_model2.rds")
saveRDS(compare_bagsample_model2, "./SyntheticData/Yue/compare_bagsample_model2.rds")

# ##########################################################
# ------------------ bagnorm ----------------------------
# ##########################################################
# 0802
sds_bagnorm0802_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0802_norm_syn.RData")
sds_bagnorm0802_tryout <- object
sds_bagnorm0802_tryout$syn[[1]][,2:54] <- sapply(sds_bagnorm0802_tryout$syn[[1]][,2:54],as.integer)
sds_bagnorm0802_tryout$syn <- sds_bagnorm0802_tryout$syn[[1]]
# 0803
sds_bagnorm0803_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0803_norm_syn.RData")
sds_bagnorm0803_tryout <- object
sds_bagnorm0803_tryout$syn[[1]][,2:54] <- sapply(sds_bagnorm0803_tryout$syn[[1]][,2:54],as.integer)
sds_bagnorm0803_tryout$syn <- sds_bagnorm0803_tryout$syn[[1]]
# 0804
sds_bagnorm0804_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0804_norm_syn.RData")
sds_bagnorm0804_tryout <- object
sds_bagnorm0804_tryout$syn[[1]][,2:54] <- sapply(sds_bagnorm0804_tryout$syn[[1]][,2:54],as.integer)
sds_bagnorm0804_tryout$syn <- sds_bagnorm0804_tryout$syn[[1]]
# 0805
sds_bagnorm0805_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0805_norm_syn.RData")
sds_bagnorm0805_tryout <- object
sds_bagnorm0805_tryout$syn[[1]][,2:54] <- sapply(sds_bagnorm0805_tryout$syn[[1]][,2:54],as.integer)
sds_bagnorm0805_tryout$syn <- sds_bagnorm0805_tryout$syn[[1]]
# 0806
sds_bagnorm0806_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0806_norm_syn.RData")
sds_bagnorm0806_tryout <- object
sds_bagnorm0806_tryout$syn[[1]][,2:54] <- sapply(sds_bagnorm0806_tryout$syn[[1]][,2:54],as.integer)
sds_bagnorm0806_tryout$syn <- sds_bagnorm0806_tryout$syn[[1]]
# 0807
sds_bagnorm0807_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0807_norm_syn.RData")
sds_bagnorm0807_tryout <- object
sds_bagnorm0807_tryout$syn[[1]][,2:54] <- sapply(sds_bagnorm0807_tryout$syn[[1]][,2:54],as.integer)
sds_bagnorm0807_tryout$syn <- sds_bagnorm0807_tryout$syn[[1]]
# 0808
sds_bagnorm0808_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0808_norm_syn.RData")
sds_bagnorm0808_tryout <- object
sds_bagnorm0808_tryout$syn[[1]][,2:54] <- sapply(sds_bagnorm0808_tryout$syn[[1]][,2:54],as.integer)
sds_bagnorm0808_tryout$syn <- sds_bagnorm0808_tryout$syn[[1]]

# concatenate the bagnorm synthetic datasets
list_sds_bagnorm_tryout <- list(sds_bagnorm0802_tryout$syn, sds_bagnorm0803_tryout$syn, 
                                  sds_bagnorm0804_tryout$syn, sds_bagnorm0805_tryout$syn, 
                                  sds_bagnorm0806_tryout$syn, sds_bagnorm0807_tryout$syn, sds_bagnorm0808_tryout$syn)

# Concatenate the dataframes vertically
sds_bagnorm_tryout <- sds_bagnorm0802_tryout
sds_bagnorm_tryout$syn <- do.call(rbind, list_sds_bagnorm_tryout)
# encode as integer
# bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_bagnorm_tryout$syn[,2:54] <- sapply(sds_bagnorm_tryout$syn[,2:54],as.integer)

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_bagnorm_model1 <- lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                                data = sds_bagnorm_tryout)
summary(lm_bagnorm_model1)
compare_bagnorm_model1 <- compare(lm_bagnorm_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the bagnorm object
saveRDS(lm_bagnorm_model1, "./SyntheticData/Yue/lm_bagnorm_model1.rds")
saveRDS(compare_bagnorm_model1, "./SyntheticData/Yue/compare_bagnorm_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_bagnorm_model2 <- lm.synds(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                                data = sds_bagnorm_tryout)
summary(lm_bagnorm_model2)
compare_bagnorm_model2 <- compare(lm_bagnorm_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the bagnorm object
saveRDS(lm_bagnorm_model2, "./SyntheticData/Yue/lm_bagnorm_model2.rds")
saveRDS(compare_bagnorm_model2, "./SyntheticData/Yue/compare_bagnorm_model2.rds")


# ##########################################################
# ------------------ bagnormrank ----------------------------
# ##########################################################
# 0802
sds_bagnormrank0802_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0802_normrank_syn.RData")
sds_bagnormrank0802_tryout <- object
sds_bagnormrank0802_tryout$syn[[1]][,2:54] <- sapply(sds_bagnormrank0802_tryout$syn[[1]][,2:54],as.integer)
sds_bagnormrank0802_tryout$syn <- sds_bagnormrank0802_tryout$syn[[1]]
# 0803
sds_bagnormrank0803_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0803_normrank_syn.RData")
sds_bagnormrank0803_tryout <- object
sds_bagnormrank0803_tryout$syn[[1]][,2:54] <- sapply(sds_bagnormrank0803_tryout$syn[[1]][,2:54],as.integer)
sds_bagnormrank0803_tryout$syn <- sds_bagnormrank0803_tryout$syn[[1]]
# 0804
sds_bagnormrank0804_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0804_normrank_syn.RData")
sds_bagnormrank0804_tryout <- object
sds_bagnormrank0804_tryout$syn[[1]][,2:54] <- sapply(sds_bagnormrank0804_tryout$syn[[1]][,2:54],as.integer)
sds_bagnormrank0804_tryout$syn <- sds_bagnormrank0804_tryout$syn[[1]]
# 0805
sds_bagnormrank0805_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0805_normrank_syn.RData")
sds_bagnormrank0805_tryout <- object
sds_bagnormrank0805_tryout$syn[[1]][,2:54] <- sapply(sds_bagnormrank0805_tryout$syn[[1]][,2:54],as.integer)
sds_bagnormrank0805_tryout$syn <- sds_bagnormrank0805_tryout$syn[[1]]
# 0806
sds_bagnormrank0806_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0806_normrank_syn.RData")
sds_bagnormrank0806_tryout <- object
sds_bagnormrank0806_tryout$syn[[1]][,2:54] <- sapply(sds_bagnormrank0806_tryout$syn[[1]][,2:54],as.integer)
sds_bagnormrank0806_tryout$syn <- sds_bagnormrank0806_tryout$syn[[1]]
# 0807
sds_bagnormrank0807_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0807_normrank_syn.RData")
sds_bagnormrank0807_tryout <- object
sds_bagnormrank0807_tryout$syn[[1]][,2:54] <- sapply(sds_bagnormrank0807_tryout$syn[[1]][,2:54],as.integer)
sds_bagnormrank0807_tryout$syn <- sds_bagnormrank0807_tryout$syn[[1]]
# 0808
sds_bagnormrank0808_tryout <- load("../../Master-Thesis-DifferentialPrivacy/synobject_bag0808_normrank_syn.RData")
sds_bagnormrank0808_tryout <- object
sds_bagnormrank0808_tryout$syn[[1]][,2:54] <- sapply(sds_bagnormrank0808_tryout$syn[[1]][,2:54],as.integer)
sds_bagnormrank0808_tryout$syn <- sds_bagnormrank0808_tryout$syn[[1]]

# concatenate the bagnormrank synthetic datasets
list_sds_bagnormrank_tryout <- list(sds_bagnormrank0802_tryout$syn, sds_bagnormrank0803_tryout$syn, 
                                sds_bagnormrank0804_tryout$syn, sds_bagnormrank0805_tryout$syn, 
                                sds_bagnormrank0806_tryout$syn, sds_bagnormrank0807_tryout$syn, sds_bagnormrank0808_tryout$syn)

# Concatenate the dataframes vertically
sds_bagnormrank_tryout <- sds_bagnormrank0802_tryout
sds_bagnormrank_tryout$syn <- do.call(rbind, list_sds_bagnormrank_tryout)
# encode as integer
# bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_bagnormrank_tryout$syn[,2:54] <- sapply(sds_bagnormrank_tryout$syn[,2:54],as.integer)

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_bagnormrank_model1 <- lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                              data = sds_bagnormrank_tryout)
summary(lm_bagnormrank_model1)
compare_bagnormrank_model1 <- compare(lm_bagnormrank_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the bagnormrank object
saveRDS(lm_bagnormrank_model1, "./SyntheticData/Yue/lm_bagnormrank_model1.rds")
saveRDS(compare_bagnormrank_model1, "./SyntheticData/Yue/compare_bagnormrank_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_bagnormrank_model2 <- lm.synds(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                              data = sds_bagnormrank_tryout)
summary(lm_bagnormrank_model2)
compare_bagnormrank_model2 <- compare(lm_bagnormrank_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
# Save the bagnormrank object
saveRDS(lm_bagnormrank_model2, "./SyntheticData/Yue/lm_bagnormrank_model2.rds")
saveRDS(compare_bagnormrank_model2, "./SyntheticData/Yue/compare_bagnormrank_model2.rds")

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
wd <- "/dss/dsshome1/0C/ru27req2/Master-Thesis-DifferentialPrivacy"
# wd <- "/Volumes/ru27req/MasterThesisRoxy/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# load the preprocessed original data
load("ods_preprocess_0802.rda")
str(ods_threshold_0802)
# we have the dataframe here named as "ods_threshold_0802"
# Encoding, var B2, 
# -1    -99 [0, 1) [1, 3) 
#  3 151336   2405 106555
#  1      2      3      4 
#  3 151336   2405 106555 
ods_threshold_0802$B2 <- as.integer(ods_threshold_0802$B2)
# var B4,
#    -99 [0, 1) [1, 5)
# 249422    290  10587
#      1      2      3 
# 249422    290  10587 
ods_threshold_0802$B4 <- as.integer(ods_threshold_0802$B4)
# var E5,
#   -99 [0, 1) [1, 2) 
# 45595   8766 205938 
#     1      2      3 
# 45595   8766 205938
ods_threshold_0802$E5 <- as.integer(ods_threshold_0802$E5)
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
ods_threshold_0802 <- ods_threshold_0802 %>% select(-all_of(cols_remove))
# also for those B1b_x like vars and D10, we try exclude them from the synthesis
cols_rm_bd <- c("B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
                "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11","B1b_x12", "B1b_x13", "D10",
                "C0_1", "C0_2", "C0_3", "C0_4", "C0_5", "C0_6")

ods_threshold_0802 <- ods_threshold_0802 %>% select(-all_of(cols_rm_bd))
ncol(ods_threshold_0802)==54
nrow(ods_threshold_0802)

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
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:54,1)], visit.sequence = arg_col[c(2:54, 1)], bag.ntree=3)
  
  write.syn(syn_dataset, filename = paste("bag0802", para_weight_list[index], "syn", sep="_"), filetype = "rda")
  message("syn done!")
}

# tryout for bag_sample
sds_bagsample_tryout <- syn_bag_experiment(para_weight_list, index=1, ods_threshold_0802, arg_method, arg_col)
# tryout for bag_norm
sds_bagnorm_tryout <- syn_bag_experiment(para_weight_list, index=2, ods_threshold_0802, arg_method, arg_col)
# tryout for bag_normrank
sds_bagnormrank_tryout <- syn_bag_experiment(para_weight_list, index=3, ods_threshold_0802, arg_method, arg_col)

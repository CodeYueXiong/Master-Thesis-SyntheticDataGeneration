##########################################################################
#####---------------weight with parametric=="norm"-------------------#####
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

# library(beepr)
# library(svMisc) # install.packages("svMisc")

# source(here::here("./SyntheticData/Yue/data_preprocess.R"))

# set the working directory
# wd <- "F:/Master-Thesis-DifferentialPrivacy"
# wd <- "/dss/dsshome1/0C/ru27req2/Master-Thesis-DifferentialPrivacy"
wd <- "/Volumes/ru27req/MasterThesisRoxy/Master-Thesis-DifferentialPrivacy"
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
bindori_dataset_threshold_chr$B2 <- as.integer(bindori_dataset_threshold_chr$B2)
# var B4,
#    -99 [0, 1) [1, 5)
# 249422    290  10587
#      1      2      3 
# 249422    290  10587 
bindori_dataset_threshold_chr$B4 <- as.integer(bindori_dataset_threshold_chr$B4)
# var E5,
#   -99 [0, 1) [1, 2) 
# 45595   8766 205938 
#     1      2      3 
# 45595   8766 205938
bindori_dataset_threshold_chr$E5 <- as.integer(bindori_dataset_threshold_chr$E5)
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
# cols_syn <- colnames(ds_col_syn)
# also for those B1b_x like vars and D10, we try exclude them from the synthesis
cols_rm_bd <- c("B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
                "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11","B1b_x12", "B1b_x13", "D10")

bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(-all_of(cols_rm_bd))
ncol(bindori_dataset_threshold_chr)==60
##########################################################################
######---------------synthetic data with synthpop-------------------######
##########################################################################

# first of all, we try extract all the methods with m=0 set in cart
settings_default <- syn(bindori_dataset_threshold_chr, method = "cart", m = 0)
# now we take a look at the extracted settings with $method and $visit.sequence
arg_method <- settings_default$method
arg_col <- settings_default$visit.sequence


# for var weight, we should always try "parametric", sample, norm, normrank, pmm
# ----------------------------try norm for weight-------------------------------

arg_method[['weight']] <- "norm"
# # demographics vars E2,E3,E4,E5,E7, we try finding the column index first
# E2_index <- match("E2",names(bindori_dataset_threshold_chr))
E3_index <- match("E3",names(bindori_dataset_threshold_chr)) # always try "sample"
# E4_index <- match("E4",names(bindori_dataset_threshold_chr))
# E5_index <- match("E5",names(bindori_dataset_threshold_chr))
# E7_index <- match("E7",names(bindori_dataset_threshold_chr))

# for E3, we always try sample
arg_method[['E3']] <- "sample"
# then we display all the combinations of 4 method strings
m1 = c("polyreg", "polyreg", "polyreg", "polyreg") # suggest rerunning with polyreg.maxit increased (default 1000)
m2 = c("cart", "cart", "cart", "cart")
m3 = c("rf", "rf", "rf", "rf")
m4 = c("bag", "bag", "bag", "bag")

method_list <- bind_rows(data.frame(t(m1)), data.frame(t(m2)), data.frame(t(m3)), data.frame(t(m4)))
# rename the columns of the method list
colnames(method_list) <- c('E2','E4','E5','E7')
# as.character(method_list[['E2']][2])

# # change as integer for B2, B4, E5, E6
# # !!! pay attention to the reverse!!!
# bindori_dataset_threshold_chr$B2 <- as.integer(bindori_dataset_threshold_chr$B2)
# bindori_dataset_threshold_chr$B4 <- as.integer(bindori_dataset_threshold_chr$B4)
# bindori_dataset_threshold_chr$E5 <- as.integer(bindori_dataset_threshold_chr$E5)
# bindori_dataset_threshold_chr$E6 <- as.integer(bindori_dataset_threshold_chr$E6)

syn_experiment <- function(method, index_round, method_list, bindori_dataset_threshold_chr, arg_method, arg_col) {
  # start from cart index_round=2
  arg_method[['E2']] <- as.character(method_list[['E2']][index_round])
  arg_method[['E4']] <- as.character(method_list[['E4']][index_round])
  arg_method[['E5']] <- as.character(method_list[['E5']][index_round])
  arg_method[['E7']] <- as.character(method_list[['E7']][index_round])
  
  syn_dataset <- NULL
  arg_method[['E3']] <- 'sample'
  arg_method[['weight']] <- 'norm'
  # arg_method[['E6']] <- 'cart'
  
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:60,1)], visit.sequence = arg_col[c(2:60, 1)])
  
  write.syn(syn_dataset, filename = paste("norm", method, "syn", sep="_"), filetype = "rda")
  message("syn done!")
}

# tryout for norm_cart
sds_normcart_tryout <- syn_experiment(method="cart", index_round=2, method_list, bindori_dataset_threshold_chr, arg_method=arg_method, arg_col=arg_col)
# tryout for norm_rf
sds_normrf_tryout <- syn_experiment(method="rf", index_round=3, method_list, bindori_dataset_threshold_chr, arg_method = arg_method, arg_col=arg_col)
# tryout for norm_bag
sds_normbag_tryout <- syn_experiment(method="bag", index_round=4, method_list, bindori_dataset_threshold_chr, arg_method = arg_method, arg_col=arg_col)
# tryout for norm_polyreg
sds_normpolyreg_tryout <- syn_experiment(method="polyreg", index_round=1, method_list, bindori_dataset_threshold_chr, arg_method = arg_method)

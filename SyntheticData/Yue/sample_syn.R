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
wd <- "/dss/dsshome1/0C/ru27req2/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# load the preprocessed original data
load("bindori_dataset_preprocessed_factor.rda")
# # dummify the data, first we change them to factor
# col_names <- names(bindori_dataset_threshold_chr)[2:90]
# bindori_dataset_threshold_chr[col_names] <- lapply(bindori_dataset_threshold_chr[col_names] , factor)

str(bindori_dataset_threshold_chr)
# !!! for var B2, B4, E5, E6, we change them to integer
# var B2, 
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
# # export_path <- "/dss/dsshome1/0C/ru27req2/Master-Thesis-DifferentialPrivacy"
# # bindori_data_name <- "bindori_dataset_preprocessed_factor.rda"
# # 
# # save(bindori_dataset_threshold_chr, file=paste(c(export_path, bindori_data_name), 
#                                                collapse="/"))
# we have the dataframe here named as "bindori_dataset_threshold_chr"
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
ncol(bindori_dataset_threshold_chr) == 60
##########################################################################
######---------------synthetic data with synthpop-------------------######
##########################################################################

# first of all, we try extract all the methods with m=0 set in cart
settings_default <- syn(bindori_dataset_threshold_chr, method = "cart", m = 0)
# now we take a look at the extracted settings with $method and $visit.sequence
arg_method <- settings_default$method
arg_col <- settings_default$visit.sequence


# for var weight, we should always try "parametric", sample, norm, normrank, pmm
# ------------------------------------------------------------------------------
# Error: The following functions were not found: syn.syn.norm, syn.syn.polyreg
# weight for sample
arg_method[['weight']] <- "sample"
E3_index <- match("E3",names(bindori_dataset_threshold_chr))
# for E3, we always try sample
# arg_method[[E3_index]] <- "sample"
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

# #========== tryout for sample_cart ============
# arg_method[['E2']] <- as.character(method_list[['E2']][2])
# arg_method[['E4']] <- as.character(method_list[['E4']][2])
# arg_method[['E5']] <- as.character(method_list[['E5']][2])
# arg_method[['E7']] <- as.character(method_list[['E7']][2])
# 
# syn_dataset <- syn(bindori_dataset_threshold_chr, method = c("sample", arg_method[-c(1, E3_index)], "sample"), visit.sequence = c(E3_index, arg_col[-c(1, E3_index)], 1))
# 
# write.syn(syn_dataset, filename = "sample_cart_syn", filetype = "rda")

syn_experiment <- function(method, index_round, method_list, bindori_dataset_threshold_chr, arg_method, arg_col) {
  # start from cart index_round=2
  arg_method[['E2']] <- as.character(method_list[['E2']][index_round])
  arg_method[['E4']] <- as.character(method_list[['E4']][index_round])
  arg_method[['E5']] <- as.character(method_list[['E5']][index_round])
  arg_method[['E7']] <- as.character(method_list[['E7']][index_round])

  syn_dataset <- NULL
  arg_method[['E3']] <- 'sample'
  arg_method[['weight']] <- 'sample'
  arg_method[['E6']] <- 'cart'
  
  # syn_dataset <- syn(bindori_dataset_threshold_chr, method = c(E3 = "sample", arg_method[-c(1, E3_index)], weight = "sample"), visit.sequence = c(E3=1, arg_col[-c(1, E3_index)], weight=1))
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:60,1)], visit.sequence = arg_col[c(2:60, 1)])
  
  write.syn(syn_dataset, filename = paste("sample", method, "syn", sep="_"), filetype = "rda")
  message("syn done!")
}

# tryout for sample_cart
sds_samplecart_tryout <- syn_experiment(method="cart", index_round=2, method_list, bindori_dataset_threshold_chr, arg_method=arg_method, arg_col=arg_col)
# tryout for sample_rf
sds_samplerf_tryout <- syn_experiment(method="rf", index_round=3, method_list, bindori_dataset_threshold_chr, arg_method = arg_method, arg_col=arg_col)
# tryout for sample_bag
sds_samplebag_tryout <- syn_experiment(method="bag", index_round=4, method_list, bindori_dataset_threshold_chr, arg_method = arg_method, arg_col=arg_col)
# tryout for sample_polyreg
sds_samplepolyreg_tryout <- syn_experiment(method="polyreg", index_round=1, method_list, bindori_dataset_threshold_chr, arg_method = arg_method, arg_col=arg_col)
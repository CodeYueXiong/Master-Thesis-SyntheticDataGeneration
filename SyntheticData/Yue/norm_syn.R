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
library(svMisc) # install.packages("svMisc")

# source(here::here("./SyntheticData/Yue/data_preprocess.R"))

# set the working directory
# wd <- "F:/Master-Thesis-DifferentialPrivacy"
wd <- "/dss/dsshome1/0C/ru27req2/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# load the preprocessed original data
load("bindori_dataset_preprocessed_new.rda")
# we have the dataframe here named as "bindori_dataset_threshold_chr"

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

syn_experiment <- function(method, index_round, method_list, bindori_dataset_threshold_chr, arg_method, arg_col) {
  # start from cart index_round=2
  arg_method[['E2']] <- as.character(method_list[['E2']][index_round])
  arg_method[['E4']] <- as.character(method_list[['E4']][index_round])
  arg_method[['E5']] <- as.character(method_list[['E5']][index_round])
  arg_method[['E7']] <- as.character(method_list[['E7']][index_round])
  
  syn_dataset <- NULL
  arg_method[['E3']] <- 'sample'
  arg_method[['weight']] <- 'norm'
  arg_method[['E6']] <- 'cart'
  
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:90,1)], visit.sequence = arg_col[c(2:90, 1)])
  
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

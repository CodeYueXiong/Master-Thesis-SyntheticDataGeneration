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
# # also, we can probably subset those columns with constant inputs
# cols_remove <- c("B13_1", "B13_2", "B13_3", "B13_4",
#                  "B13_5", "B13_6", "B13_7",
#                  "B14_1", "B14_2", "B14_3", "B14_4", "B14_5",
#                  "D6_1", "D6_2", "D6_3", "F3_de")
# ds_col_syn <- bindori_dataset_threshold_chr %>% select(-cols_remove)
# cols_syn <- colnames(ds_col_syn)


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
# arg_method[['E3']] <- "sample"
# then we display all the combinations of 4 method strings
m1 = c("polyreg", "polyreg", "polyreg", "polyreg") # suggest rerunning with polyreg.maxit increased (default 1000)
m2 = c("cart", "cart", "cart", "cart")
m3 = c("rf", "rf", "rf", "rf")
m4 = c("bag", "bag", "bag", "bag")

method_list <- bind_rows(data.frame(t(m1)), data.frame(t(m2)), data.frame(t(m3)), data.frame(t(m4)))
# rename the columns of the method list
colnames(method_list) <- c('E2','E4','E5','E7')
# as.character(method_list[['E2']][2])

#========== tryout for sample_cart ============
arg_method[['E2']] <- as.character(method_list[['E2']][2])
arg_method[['E4']] <- as.character(method_list[['E4']][2])
arg_method[['E5']] <- as.character(method_list[['E5']][2])
arg_method[['E7']] <- as.character(method_list[['E7']][2])

syn_dataset <- syn(bindori_dataset_threshold_chr, method = c("sample", arg_method[-c(1, E3_index)], "sample"), visit.sequence = c(E3_index, arg_col[-c(1, E3_index)], 1))

write.syn(syn_dataset, filename = "sample_cart_syn", filetype = "rda")

syn_experiment <- function(method, index_round, method_list, bindori_dataset_threshold_chr, arg_method, arg_col) {
  # start from cart index_round=2
  arg_method[['E2']] <- as.character(method_list[['E2']][index_round])
  arg_method[['E4']] <- as.character(method_list[['E4']][index_round])
  arg_method[['E5']] <- as.character(method_list[['E5']][index_round])
  arg_method[['E7']] <- as.character(method_list[['E7']][index_round])
  
  syn_dataset <- NULL
  
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = c("sample", arg_method[-c(1, E3_index)], "sample"), visit.sequence = c(E3_index, arg_col[-c(1, E3_index)], 1))
  
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
sds_samplepolyreg_tryout <- syn_experiment(method_list, bindori_dataset_threshold_chr, arg_method = arg_method)

# design a for loop
# syn_experiment <- function(method_list, bindori_dataset_threshold_chr, arg_method) {
#   for (index_round in 1:4) {
#     arg_method[['E2']] <- as.character(method_list[['E2']][index_round])
#     arg_method[['E4']] <- as.character(method_list[['E4']][index_round])
#     arg_method[['E5']] <- as.character(method_list[['E5']][index_round])
#     arg_method[['E7']] <- as.character(method_list[['E7']][index_round])
#     
#     syn_dataset <- NULL
#     if (index_round == 1) {
#       syn_dataset[[index_round]] <- syn(bindori_dataset_threshold_chr, method = c(arg_method[-1], "sample"), visit.sequence = c(2:90, 1), polyreg.maxit = 10000)
#     } else {
#       syn_dataset[[index_round]] <- syn(bindori_dataset_threshold_chr, method = c(arg_method[-1], "sample"), visit.sequence = c(2:90, 1))
#     }
#     
#     # make the progress bar working
#     progress(index_round, 4)
#     Sys.sleep(0.02)
#     if (index_round == 4) {
#       save(as.data.frame(syn_dataset), file = "sample_syn.rda")
#       message("Done!")}
#   }
#   
#   return(as.data.frame(syn_dataset))
# }

# # for loop combination 1~4
# sds_sample_tryout <- syn_experiment(method_list, bindori_dataset_threshold_chr, arg_method = arg_method)
# 
# # weight for norm
# arg_method[['weight']] <- "norm"
# # demographics vars E2,E3,E4,E5,E7, we try finding the column index first
# E2_index <- match("E2",names(bindori_dataset_threshold_chr))
# E3_index <- match("E3",names(bindori_dataset_threshold_chr)) # always try "sample"
# E4_index <- match("E4",names(bindori_dataset_threshold_chr))
# E5_index <- match("E5",names(bindori_dataset_threshold_chr))
# E7_index <- match("E7",names(bindori_dataset_threshold_chr))
# 
# # for E3, we always try sample
# # arg_method[[E3_index]] <- "sample"
# arg_method[['E3']] <- "sample"
# # then we display all the combinations of 4 method strings
# m1 = c("polyreg", "cart", "syn.rf", "syn.bag")
# m2 = c("polyreg", "cart", "syn.rf", "syn.bag")
# m3 = c("polyreg", "cart", "syn.rf", "syn.bag")
# m4 = c("polyreg", "cart", "syn.rf", "syn.bag")
# method_list = expand.grid(m1,m2,m3,m4)
# # rename the columns of the method list
# colnames(method_list) <- c('E2','E4','E5','E7')
# # as.character(method_list[['E2']][2])
# 
# # i=1
# syn_experiment <- function(index_round, method_list, bindori_dataset_threshold_chr, arg_method, arg_col) {
#   arg_method[['E2']] <- as.character(method_list[['E2']][index_round])
#   arg_method[['E4']] <- as.character(method_list[['E4']][index_round])
#   arg_method[['E5']] <- as.character(method_list[['E5']][index_round])
#   arg_method[['E7']] <- as.character(method_list[['E7']][index_round])
#   
#   syn_dataset <- NULL
#   syn_dataset <- syn(bindori_dataset_threshold_chr, method = c(arg_method[-1], "norm"), visit.sequence = c(2:90, 1))
#   beep(sound = 3)
#   beep(sound = 3)
#   beep(sound = 3)
#   
#   return(data.frame(syn_dataset))
# }
# 
# # combination 1~256
# sds_norm_round1 <- syn_experiment(1, method_list, bindori_dataset_threshold_chr = bindori_dataset_threshold_chr, arg_method = arg_method, arg_col = arg_col)
# 
# 
# # -------------------------------------------------------------------------------
# # weight for syn.normrank
# 
# # -------------------------------------------------------------------------------
# # weight for syn.pmm
# 
# 
# 
# # utility evaluation
# # one-way marginals using compare()
# compare_plots <- c()
# 
# for (i in 1:85) {
#   cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
#   
#   compare_plots[[i]] <- compare(object = data.frame(Pdata = syn_select_vars[i]),
#                                 data = data.frame(Pdata = bindori_select_vars[i]),
#                                 vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
#                                 msel = NULL, stat = "percents", breaks = 10,
#                                 nrow = 2, ncol = 2, rel.size.x = 1,
#                                 utility.stats = c("pMSE", "S_pMSE"),
#                                 cols = c("#1A3C5A","#4187BF"),
#                                 plot = TRUE, table = TRUE)
#   
# }
# 
# pMSE_list <- c()
# S_pMSE_list <- c()
# for (i in 1:85) {
#   pMSE_list <- append(pMSE_list, compare_plots[[i]]$tab.utility[1])
#   S_pMSE_list <- append(S_pMSE_list, compare_plots[[i]]$tab.utility[2])
# }
# 
# vars2show <- df_utility[df_utility[, "S_pMSE"]<10, ][1]
# 
# # two-way marginals with utility.tables()
# utility.twoway <- utility.tables(object = data.frame(syn_select_vars), 
#                                  data = data.frame(bindori_select_vars),
#                                  tables = "twoway",
#                                  tab.stats = c("pMSE", "S_pMSE"),
#                                  plot.stat = "S_pMSE", plot = TRUE,
#                                  print.tabs = TRUE)
# 
# # sdc using
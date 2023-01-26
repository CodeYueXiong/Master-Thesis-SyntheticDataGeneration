################################################################################
# ----------------------- MachineLearning Evaluation --------------------------#
# This script is meant for the comparison of coef plots based on the synthetic #
# datasets compared to the original dataset                                    #

packages_list <- c("readr", "vroom", "tidyverse", "arsenal", "reshape2",
                   "synthpop", "ggplot2", "dbplyr", "data.table", "caret",
                   "mlr3", "mlr3learners", "mlr3filters", "mlr3pipelines", 
                   "mlr3tuning", "mlr3viz", "mlr3verse", "mlr3benchmark",
                   "e1071", "MASS")
install.packages(packages_list)

# load the libraries
library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(synthpop)
library(ggplot2)
library(dbplyr)

library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3viz)
library(mlr3verse)
library(mlr3benchmark)

library(e1071)
library(MASS)
library(stats)

# set the working directory
wd <- "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
setwd(wd)

#*******Step 1: load original and synthetic datasets
#*
# then we load the original preprocessed datasets
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)

# load synthetic datasets
# ------ cart group -------
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder_cart <- "./SyntheticData/Yue/syn1_cart"
folder_rf <- "./SyntheticData/Yue/syn2_rf"
folder_bag <- "./SyntheticData/Yue/syn3_bag"
folder_polyreg <- "./SyntheticData/Yue/syn4_polyreg"
folder_norm <- "./SyntheticData/Yue/syn5_norm"
folder_normrank <- "./SyntheticData/Yue/syn6_normrank"

files_cart <- list.files(folder_cart, pattern = ".rda$")
files_rf <- list.files(folder_rf, pattern = ".rda$")
files_bag <- list.files(folder_bag, pattern = ".rda$")
files_polyreg <- list.files(folder_polyreg, pattern = ".rda$")
files_norm <- list.files(folder_norm, pattern = ".rda$")
files_normrank <- list.files(folder_normrank, pattern = ".rda$")

syn_cart_models <- Map(rda2list, file.path(folder_cart, files_cart))
names(syn_cart_models) <- tools::file_path_sans_ext(files_cart)

# we dataframe the lists
cart_sample_sds <- data.frame(syn_cart_models$cart_sample_syn)
cart_norm_sds <- data.frame(syn_cart_models$cart_norm_syn)
cart_normrank_sds <- data.frame(syn_cart_models$cart_normrank_syn)

# delete the prefix in variable naming
names(cart_sample_sds) <- sub('^syn.', '', names(cart_sample_sds))
names(cart_norm_sds) <- sub('^syn.', '', names(cart_norm_sds))
names(cart_normrank_sds) <- sub('^syn.', '', names(cart_normrank_sds))

# ------ rf group -------
syn_rf_models <- Map(rda2list, file.path(folder_rf, files_rf))
names(syn_rf_models) <- tools::file_path_sans_ext(files_rf)

# we dataframe the lists
rf_sample_sds <- data.frame(syn_rf_models$rf_sample_syn)
rf_norm_sds <- data.frame(syn_rf_models$rf_norm_syn)
rf_normrank_sds <- data.frame(syn_rf_models$rf_normrank_syn)

# delete the prefix
names(rf_sample_sds) <- sub('^rf_sample_sds.', '', names(rf_sample_sds))
names(rf_norm_sds) <- sub('^rf_norm_sds.', '', names(rf_norm_sds))
names(rf_normrank_sds) <- sub('^rf_normrank_sds.', '', names(rf_normrank_sds))

str(rf_sample_sds)
str(rf_norm_sds)
str(rf_normrank_sds)

# ------ bag group -------
syn_bag_models <- Map(rda2list, file.path(folder_bag, files_bag))
names(syn_bag_models) <- tools::file_path_sans_ext(files_bag)
# we dataframe the lists
bag_sample_sds <- data.frame(syn_bag_models$bag_sample_syn)
bag_norm_sds <- data.frame(syn_bag_models$bag_norm_syn)
bag_normrank_sds <- data.frame(syn_bag_models$bag_normrank_syn)
# delete the prefix
names(bag_sample_sds) <- sub('^bag_sample_sds.', '', names(bag_sample_sds))
names(bag_norm_sds) <- sub('^bag_norm_sds.', '', names(bag_norm_sds))
names(bag_normrank_sds) <- sub('^bag_normrank_sds.', '', names(bag_normrank_sds))
str(bag_sample_sds)
str(bag_norm_sds)
str(bag_normrank_sds)

# ------ polyreg group ------
syn_polyreg_models <- Map(rda2list, file.path(folder_polyreg, files_polyreg))
names(syn_polyreg_models) <- tools::file_path_sans_ext(files_polyreg)
# we dataframe the lists
polyreg_sample_sds <- data.frame(syn_polyreg_models$polyreg_sample_syn)
polyreg_norm_sds <- data.frame(syn_polyreg_models$polyreg_norm_syn)
polyreg_normrank_sds <- data.frame(syn_polyreg_models$polyreg_normrank_syn)
# delete the prefix in variable naming
names(polyreg_sample_sds) <- sub('^syn.', '', names(polyreg_sample_sds))
names(polyreg_norm_sds) <- sub('^syn.', '', names(polyreg_norm_sds))
names(polyreg_normrank_sds) <- sub('^syn.', '', names(polyreg_normrank_sds))
str(polyreg_sample_sds)
str(polyreg_norm_sds)
str(polyreg_normrank_sds)
# only the E6 variable needs to be reversed back to factor
polyreg_sample_sds$E6[polyreg_sample_sds$E6 == "1"] <- "-99"
polyreg_sample_sds$E6[polyreg_sample_sds$E6 == "2"] <- "[0, 9)"
polyreg_norm_sds$E6[polyreg_norm_sds$E6 == "1"] <- "-99"
polyreg_norm_sds$E6[polyreg_norm_sds$E6 == "2"] <- "[0, 9)"
polyreg_normrank_sds$E6[polyreg_normrank_sds$E6 == "1"] <- "-99"
polyreg_normrank_sds$E6[polyreg_normrank_sds$E6 == "2"] <- "[0, 9)"

cols_factor <- c("E6")

polyreg_sample_sds[cols_factor] <- lapply(polyreg_sample_sds[cols_factor], factor)
polyreg_norm_sds[cols_factor] <- lapply(polyreg_norm_sds[cols_factor], factor)
polyreg_normrank_sds[cols_factor] <- lapply(polyreg_normrank_sds[cols_factor], factor)

str(polyreg_sample_sds)
str(polyreg_norm_sds)
str(polyreg_normrank_sds)

# ------- norm group ------
syn_norm_models <- Map(rda2list, file.path(folder_norm, files_norm))
names(syn_norm_models) <- tools::file_path_sans_ext(files_norm)
# we dataframe the lists
norm_sample_sds <- data.frame(syn_norm_models$norm_sample_syn)
norm_norm_sds <- data.frame(syn_norm_models$norm_norm_syn)
norm_normrank_sds <- data.frame(syn_norm_models$norm_normrank_syn)
# delete the prefix in variable naming
names(norm_sample_sds) <- sub('^syn.', '', names(norm_sample_sds))
names(norm_norm_sds) <- sub('^syn.', '', names(norm_norm_sds))
names(norm_normrank_sds) <- sub('^syn.', '', names(norm_normrank_sds))
str(norm_sample_sds)
str(norm_norm_sds)
str(norm_normrank_sds)
# var C1_m
table(norm_sample_sds$C1_m)  # -1      0      1      2      3      4      5 
table(bindori_dataset_threshold_chr$C1_m)  # -99      1      2
norm_sample_sds$C1_m[norm_sample_sds$C1_m == "1"] <- "-99"
norm_sample_sds$C1_m[norm_sample_sds$C1_m == "2"] <- "1"
norm_sample_sds$C1_m[norm_sample_sds$C1_m == "3"] <- "2"
norm_norm_sds$C1_m[norm_norm_sds$C1_m == "1"] <- "-99"
norm_norm_sds$C1_m[norm_norm_sds$C1_m == "2"] <- "1"
norm_norm_sds$C1_m[norm_norm_sds$C1_m == "3"] <- "2"
norm_normrank_sds$C1_m[norm_normrank_sds$C1_m == "1"] <- "-99"
norm_normrank_sds$C1_m[norm_normrank_sds$C1_m == "2"] <- "1"
norm_normrank_sds$C1_m[norm_normrank_sds$C1_m == "3"] <- "2"
# var C3
table(norm_sample_sds$C3) # 0      1      2      3      4 
table(bindori_dataset_threshold_chr$C3) # 1      2
norm_sample_sds$C3[norm_sample_sds$C3 == "1"] <- "1"
norm_sample_sds$C3[norm_sample_sds$C3 == "2"] <- "2"
norm_norm_sds$C3[norm_norm_sds$C3 == "1"] <- "1"
norm_norm_sds$C3[norm_norm_sds$C3 == "2"] <- "2"
norm_normrank_sds$C3[norm_normrank_sds$C3 == "1"] <- "1"
norm_normrank_sds$C3[norm_normrank_sds$C3 == "2"] <- "2"
# var C5
table(norm_sample_sds$C5) # -6    -5    -4    -3    -2    -1     0     1     2     3     4     5     6     7     8     9    10    11
table(bindori_dataset_threshold_chr$C5) # -99      1      2      3      4      5      6 
norm_sample_sds$C5[norm_sample_sds$C5 == "1"] <- "-99"
norm_sample_sds$C5[norm_sample_sds$C5 == "2"] <- "1"
norm_sample_sds$C5[norm_sample_sds$C5 == "3"] <- "2"
norm_sample_sds$C5[norm_sample_sds$C5 == "4"] <- "3"
norm_sample_sds$C5[norm_sample_sds$C5 == "5"] <- "4"
norm_sample_sds$C5[norm_sample_sds$C5 == "6"] <- "5"
norm_sample_sds$C5[norm_sample_sds$C5 == "7"] <- "6"

norm_norm_sds$C5[norm_norm_sds$C5 == "1"] <- "-99"
norm_norm_sds$C5[norm_norm_sds$C5 == "2"] <- "1"
norm_norm_sds$C5[norm_norm_sds$C5 == "3"] <- "2"
norm_norm_sds$C5[norm_norm_sds$C5 == "4"] <- "3"
norm_norm_sds$C5[norm_norm_sds$C5 == "5"] <- "4"
norm_norm_sds$C5[norm_norm_sds$C5 == "6"] <- "5"
norm_norm_sds$C5[norm_norm_sds$C5 == "7"] <- "6"

norm_normrank_sds$C5[norm_normrank_sds$C5 == "1"] <- "-99"
norm_normrank_sds$C5[norm_normrank_sds$C5 == "2"] <- "1"
norm_normrank_sds$C5[norm_normrank_sds$C5 == "3"] <- "2"
norm_normrank_sds$C5[norm_normrank_sds$C5 == "4"] <- "3"
norm_normrank_sds$C5[norm_normrank_sds$C5 == "5"] <- "4"
norm_normrank_sds$C5[norm_normrank_sds$C5 == "6"] <- "5"
norm_normrank_sds$C5[norm_normrank_sds$C5 == "7"] <- "6"
# var C8
table(norm_sample_sds$C8, norm_normrank_sds$C8) # 1      2      3
table(bindori_dataset_threshold_chr$C8)  # -99      1      2 
norm_sample_sds$C8[norm_sample_sds$C8 == "1"] <- "-99"
norm_sample_sds$C8[norm_sample_sds$C8 == "2"] <- "1"
norm_sample_sds$C8[norm_sample_sds$C8 == "3"] <- "2"
norm_norm_sds$C8[norm_norm_sds$C8 == "1"] <- "-99"
norm_norm_sds$C8[norm_norm_sds$C8 == "2"] <- "1"
norm_norm_sds$C8[norm_norm_sds$C8 == "3"] <- "2"
norm_normrank_sds$C8[norm_normrank_sds$C8 == "1"] <- "-99"
norm_normrank_sds$C8[norm_normrank_sds$C8 == "2"] <- "1"
norm_normrank_sds$C8[norm_normrank_sds$C8 == "3"] <- "2"
# var D1
table(norm_sample_sds$D1, norm_normrank_sds$D1) # too many
table(bindori_dataset_threshold_chr$D1) # -99      1      2      3      4      5
norm_sample_sds$D1[norm_sample_sds$D1 == "1"] <- "-99"
norm_sample_sds$D1[norm_sample_sds$D1 == "2"] <- "1"
norm_sample_sds$D1[norm_sample_sds$D1 == "3"] <- "2"
norm_sample_sds$D1[norm_sample_sds$D1 == "4"] <- "3"
norm_sample_sds$D1[norm_sample_sds$D1 == "5"] <- "4"
norm_sample_sds$D1[norm_sample_sds$D1 == "6"] <- "5"

norm_norm_sds$D1[norm_norm_sds$D1 == "1"] <- "-99"
norm_norm_sds$D1[norm_norm_sds$D1 == "2"] <- "1"
norm_norm_sds$D1[norm_norm_sds$D1 == "3"] <- "2"
norm_norm_sds$D1[norm_norm_sds$D1 == "4"] <- "3"
norm_norm_sds$D1[norm_norm_sds$D1 == "5"] <- "4"
norm_norm_sds$D1[norm_norm_sds$D1 == "6"] <- "5"

norm_normrank_sds$D1[norm_normrank_sds$D1 == "1"] <- "-99"
norm_normrank_sds$D1[norm_normrank_sds$D1 == "2"] <- "1"
norm_normrank_sds$D1[norm_normrank_sds$D1 == "3"] <- "2"
norm_normrank_sds$D1[norm_normrank_sds$D1 == "4"] <- "3"
norm_normrank_sds$D1[norm_normrank_sds$D1 == "5"] <- "4"
norm_normrank_sds$D1[norm_normrank_sds$D1 == "6"] <- "5"
# var D3
table(norm_sample_sds$D3, norm_normrank_sds$D3)
table(bindori_dataset_threshold_chr$D3)  # -99      1      2      3      4
norm_sample_sds$D3[norm_sample_sds$D3 == "1"] <- "-99"
norm_sample_sds$D3[norm_sample_sds$D3 == "2"] <- "1"
norm_sample_sds$D3[norm_sample_sds$D3 == "3"] <- "2"
norm_sample_sds$D3[norm_sample_sds$D3 == "4"] <- "3"
norm_sample_sds$D3[norm_sample_sds$D3 == "5"] <- "4"

norm_norm_sds$D3[norm_norm_sds$D3 == "1"] <- "-99"
norm_norm_sds$D3[norm_norm_sds$D3 == "2"] <- "1"
norm_norm_sds$D3[norm_norm_sds$D3 == "3"] <- "2"
norm_norm_sds$D3[norm_norm_sds$D3 == "4"] <- "3"
norm_norm_sds$D3[norm_norm_sds$D3 == "5"] <- "4"

norm_normrank_sds$D3[norm_normrank_sds$D3 == "1"] <- "-99"
norm_normrank_sds$D3[norm_normrank_sds$D3 == "2"] <- "1"
norm_normrank_sds$D3[norm_normrank_sds$D3 == "3"] <- "2"
norm_normrank_sds$D3[norm_normrank_sds$D3 == "4"] <- "3"
norm_normrank_sds$D3[norm_normrank_sds$D3 == "5"] <- "4"
# var D4
table(norm_sample_sds$D4, norm_normrank_sds$D4)
table(bindori_dataset_threshold_chr$D4)  # -99      1      2      3      4
norm_sample_sds$D4[norm_sample_sds$D4 == "1"] <- "-99"
norm_sample_sds$D4[norm_sample_sds$D4 == "2"] <- "1"
norm_sample_sds$D4[norm_sample_sds$D4 == "3"] <- "2"
norm_sample_sds$D4[norm_sample_sds$D4 == "4"] <- "3"
norm_sample_sds$D4[norm_sample_sds$D4 == "5"] <- "4"

norm_norm_sds$D4[norm_norm_sds$D4 == "1"] <- "-99"
norm_norm_sds$D4[norm_norm_sds$D4 == "2"] <- "1"
norm_norm_sds$D4[norm_norm_sds$D4 == "3"] <- "2"
norm_norm_sds$D4[norm_norm_sds$D4 == "4"] <- "3"
norm_norm_sds$D4[norm_norm_sds$D4 == "5"] <- "4"

norm_normrank_sds$D4[norm_normrank_sds$D4 == "1"] <- "-99"
norm_normrank_sds$D4[norm_normrank_sds$D4 == "2"] <- "1"
norm_normrank_sds$D4[norm_normrank_sds$D4 == "3"] <- "2"
norm_normrank_sds$D4[norm_normrank_sds$D4 == "4"] <- "3"
norm_normrank_sds$D4[norm_normrank_sds$D4 == "5"] <- "4"
# var E3
table(norm_sample_sds$E3, norm_normrank_sds$E3)
table(bindori_dataset_threshold_chr$E3)  # -99      1      2      3      4
norm_sample_sds$E3[norm_sample_sds$E3 == "1"] <- "-99"
norm_sample_sds$E3[norm_sample_sds$E3 == "2"] <- "1"
norm_sample_sds$E3[norm_sample_sds$E3 == "3"] <- "2"
norm_sample_sds$E3[norm_sample_sds$E3 == "4"] <- "3"
norm_sample_sds$E3[norm_sample_sds$E3 == "5"] <- "4"

norm_norm_sds$E3[norm_norm_sds$E3 == "1"] <- "-99"
norm_norm_sds$E3[norm_norm_sds$E3 == "2"] <- "1"
norm_norm_sds$E3[norm_norm_sds$E3 == "3"] <- "2"
norm_norm_sds$E3[norm_norm_sds$E3 == "4"] <- "3"
norm_norm_sds$E3[norm_norm_sds$E3 == "5"] <- "4"

norm_normrank_sds$E3[norm_normrank_sds$E3 == "1"] <- "-99"
norm_normrank_sds$E3[norm_normrank_sds$E3 == "2"] <- "1"
norm_normrank_sds$E3[norm_normrank_sds$E3 == "3"] <- "2"
norm_normrank_sds$E3[norm_normrank_sds$E3 == "4"] <- "3"
norm_normrank_sds$E3[norm_normrank_sds$E3 == "5"] <- "4"
# var E4
table(norm_sample_sds$E4, norm_normrank_sds$E4)
table(bindori_dataset_threshold_chr$E4)
norm_sample_sds$E4[norm_sample_sds$E4 == "1"] <- "-99"
norm_sample_sds$E4[norm_sample_sds$E4 == "2"] <- "1"
norm_sample_sds$E4[norm_sample_sds$E4 == "3"] <- "2"
norm_sample_sds$E4[norm_sample_sds$E4 == "4"] <- "3"
norm_sample_sds$E4[norm_sample_sds$E4 == "5"] <- "4"
norm_sample_sds$E4[norm_sample_sds$E4 == "6"] <- "5"
norm_sample_sds$E4[norm_sample_sds$E4 == "7"] <- "6"
norm_sample_sds$E4[norm_sample_sds$E4 == "8"] <- "7"

norm_norm_sds$E4[norm_norm_sds$E4 == "1"] <- "-99"
norm_norm_sds$E4[norm_norm_sds$E4 == "2"] <- "1"
norm_norm_sds$E4[norm_norm_sds$E4 == "3"] <- "2"
norm_norm_sds$E4[norm_norm_sds$E4 == "4"] <- "3"
norm_norm_sds$E4[norm_norm_sds$E4 == "5"] <- "4"
norm_norm_sds$E4[norm_norm_sds$E4 == "6"] <- "5"
norm_norm_sds$E4[norm_norm_sds$E4 == "7"] <- "6"
norm_norm_sds$E4[norm_norm_sds$E4 == "8"] <- "7"

norm_normrank_sds$E4[norm_normrank_sds$E4 == "1"] <- "-99"
norm_normrank_sds$E4[norm_normrank_sds$E4 == "2"] <- "1"
norm_normrank_sds$E4[norm_normrank_sds$E4 == "3"] <- "2"
norm_normrank_sds$E4[norm_normrank_sds$E4 == "4"] <- "3"
norm_normrank_sds$E4[norm_normrank_sds$E4 == "5"] <- "4"
norm_normrank_sds$E4[norm_normrank_sds$E4 == "6"] <- "5"
norm_normrank_sds$E4[norm_normrank_sds$E4 == "7"] <- "6"
norm_normrank_sds$E4[norm_normrank_sds$E4 == "8"] <- "7"
# var B4
table(norm_sample_sds$B4, norm_normrank_sds$B4)
table(bindori_dataset_threshold_chr$B4)  # -99 [0, 1) [1, 5)
norm_sample_sds$B4[norm_sample_sds$B4 == "1"] <- "-99"
norm_sample_sds$B4[norm_sample_sds$B4 == "2"] <- "[0, 1)"
norm_sample_sds$B4[norm_sample_sds$B4 == "3"] <- "[1, 5)"

norm_norm_sds$B4[norm_norm_sds$B4 == "1"] <- "-99"
norm_norm_sds$B4[norm_norm_sds$B4 == "2"] <- "[0, 1)"
norm_norm_sds$B4[norm_norm_sds$B4 == "3"] <- "[1, 5)"

norm_normrank_sds$B4[norm_normrank_sds$B4 == "1"] <- "-99"
norm_normrank_sds$B4[norm_normrank_sds$B4 == "2"] <- "[0, 1)"
norm_normrank_sds$B4[norm_normrank_sds$B4 == "3"] <- "[1, 5)"
# var E6
table(norm_sample_sds$E6, norm_normrank_sds$E6)
table(bindori_dataset_threshold_chr$E6)  # -99 [0, 9)
norm_sample_sds$E6[norm_sample_sds$E6 == "1"] <- "-99"
norm_sample_sds$E6[norm_sample_sds$E6 == "2"] <- "[0, 9)"

norm_norm_sds$E6[norm_norm_sds$E6 == "1"] <- "-99"
norm_norm_sds$E6[norm_norm_sds$E6 == "2"] <- "[0, 9)"

norm_normrank_sds$E6[norm_normrank_sds$E6 == "1"] <- "-99"
norm_normrank_sds$E6[norm_normrank_sds$E6 == "2"] <- "[0, 9)"
# for norm_sample, we try change all the character type back to factor except "weight"
col_names <- names(norm_sample_sds)[2:54]
norm_sample_sds[col_names] <- lapply(norm_sample_sds[col_names], factor)
norm_norm_sds[col_names] <- lapply(norm_norm_sds[col_names], factor)
norm_normrank_sds[col_names] <- lapply(norm_normrank_sds[col_names], factor)
str(norm_sample_sds)
str(norm_norm_sds)
str(norm_normrank_sds)

# ------- normrank group ------
syn_normrank_models <- Map(rda2list, file.path(folder_normrank, files_normrank))
names(syn_normrank_models) <- tools::file_path_sans_ext(files_normrank)
# we dataframe the lists
normrank_sample_sds <- data.frame(syn_normrank_models$normrank_sample_syn)
normrank_norm_sds <- data.frame(syn_normrank_models$normrank_norm_syn)
normrank_normrank_sds <- data.frame(syn_normrank_models$normrank_normrank_syn)
# delete the prefix in variable naming
names(normrank_sample_sds) <- sub('^syn.', '', names(normrank_sample_sds))
names(normrank_norm_sds) <- sub('^syn.', '', names(normrank_norm_sds))
names(normrank_normrank_sds) <- sub('^syn.', '', names(normrank_normrank_sds))
str(normrank_sample_sds)
str(normrank_norm_sds)
str(normrank_normrank_sds)
# var C1_m
table(normrank_sample_sds$C1_m, normrank_norm_sds$C1_m)  # 1 2 3
table(bindori_dataset_threshold_chr$C1_m)  # -99      1      2
normrank_sample_sds$C1_m[normrank_sample_sds$C1_m == "1"] <- "-99"
normrank_sample_sds$C1_m[normrank_sample_sds$C1_m == "2"] <- "1"
normrank_sample_sds$C1_m[normrank_sample_sds$C1_m == "3"] <- "2"
normrank_norm_sds$C1_m[normrank_norm_sds$C1_m == "1"] <- "-99"
normrank_norm_sds$C1_m[normrank_norm_sds$C1_m == "2"] <- "1"
normrank_norm_sds$C1_m[normrank_norm_sds$C1_m == "3"] <- "2"
normrank_normrank_sds$C1_m[normrank_normrank_sds$C1_m == "1"] <- "-99"
normrank_normrank_sds$C1_m[normrank_normrank_sds$C1_m == "2"] <- "1"
normrank_normrank_sds$C1_m[normrank_normrank_sds$C1_m == "3"] <- "2"
# var C3
table(normrank_sample_sds$C3, normrank_norm_sds$C3) # 1 2 
table(bindori_dataset_threshold_chr$C3) # 1      2
normrank_sample_sds$C3[normrank_sample_sds$C3 == "1"] <- "1"
normrank_sample_sds$C3[normrank_sample_sds$C3 == "2"] <- "2"
normrank_norm_sds$C3[normrank_norm_sds$C3 == "1"] <- "1"
normrank_norm_sds$C3[normrank_norm_sds$C3 == "2"] <- "2"
normrank_normrank_sds$C3[normrank_normrank_sds$C3 == "1"] <- "1"
normrank_normrank_sds$C3[normrank_normrank_sds$C3 == "2"] <- "2"
# var C5
table(normrank_sample_sds$C5) # 1 to 7
table(bindori_dataset_threshold_chr$C5) # -99      1      2      3      4      5      6 
normrank_sample_sds$C5[normrank_sample_sds$C5 == "1"] <- "-99"
normrank_sample_sds$C5[normrank_sample_sds$C5 == "2"] <- "1"
normrank_sample_sds$C5[normrank_sample_sds$C5 == "3"] <- "2"
normrank_sample_sds$C5[normrank_sample_sds$C5 == "4"] <- "3"
normrank_sample_sds$C5[normrank_sample_sds$C5 == "5"] <- "4"
normrank_sample_sds$C5[normrank_sample_sds$C5 == "6"] <- "5"
normrank_sample_sds$C5[normrank_sample_sds$C5 == "7"] <- "6"

normrank_norm_sds$C5[normrank_norm_sds$C5 == "1"] <- "-99"
normrank_norm_sds$C5[normrank_norm_sds$C5 == "2"] <- "1"
normrank_norm_sds$C5[normrank_norm_sds$C5 == "3"] <- "2"
normrank_norm_sds$C5[normrank_norm_sds$C5 == "4"] <- "3"
normrank_norm_sds$C5[normrank_norm_sds$C5 == "5"] <- "4"
normrank_norm_sds$C5[normrank_norm_sds$C5 == "6"] <- "5"
normrank_norm_sds$C5[normrank_norm_sds$C5 == "7"] <- "6"

normrank_normrank_sds$C5[normrank_normrank_sds$C5 == "1"] <- "-99"
normrank_normrank_sds$C5[normrank_normrank_sds$C5 == "2"] <- "1"
normrank_normrank_sds$C5[normrank_normrank_sds$C5 == "3"] <- "2"
normrank_normrank_sds$C5[normrank_normrank_sds$C5 == "4"] <- "3"
normrank_normrank_sds$C5[normrank_normrank_sds$C5 == "5"] <- "4"
normrank_normrank_sds$C5[normrank_normrank_sds$C5 == "6"] <- "5"
normrank_normrank_sds$C5[normrank_normrank_sds$C5 == "7"] <- "6"
# var C8
table(normrank_sample_sds$C8, normrank_normrank_sds$C8) # 1      2 (only has 1 and 2, C8 needs to be treated separately)
table(bindori_dataset_threshold_chr$C8)  # -99      1      2 
normrank_sample_sds$C8[normrank_sample_sds$C8 == "1"] <- "-99"
normrank_sample_sds$C8[normrank_sample_sds$C8 == "2"] <- "1"
normrank_sample_sds$C8[normrank_sample_sds$C8 == "3"] <- "2"
normrank_norm_sds$C8[normrank_norm_sds$C8 == "1"] <- "-99"
normrank_norm_sds$C8[normrank_norm_sds$C8 == "2"] <- "1"
normrank_norm_sds$C8[normrank_norm_sds$C8 == "3"] <- "2"
normrank_normrank_sds$C8[normrank_normrank_sds$C8 == "1"] <- "-99"
normrank_normrank_sds$C8[normrank_normrank_sds$C8 == "2"] <- "1"
normrank_normrank_sds$C8[normrank_normrank_sds$C8 == "3"] <- "2"
# var D1
table(normrank_sample_sds$D1, normrank_normrank_sds$D1) #
table(bindori_dataset_threshold_chr$D1) # -99      1      2      3      4      5
normrank_sample_sds$D1[normrank_sample_sds$D1 == "1"] <- "-99"
normrank_sample_sds$D1[normrank_sample_sds$D1 == "2"] <- "1"
normrank_sample_sds$D1[normrank_sample_sds$D1 == "3"] <- "2"
normrank_sample_sds$D1[normrank_sample_sds$D1 == "4"] <- "3"
normrank_sample_sds$D1[normrank_sample_sds$D1 == "5"] <- "4"
normrank_sample_sds$D1[normrank_sample_sds$D1 == "6"] <- "5"

normrank_norm_sds$D1[normrank_norm_sds$D1 == "1"] <- "-99"
normrank_norm_sds$D1[normrank_norm_sds$D1 == "2"] <- "1"
normrank_norm_sds$D1[normrank_norm_sds$D1 == "3"] <- "2"
normrank_norm_sds$D1[normrank_norm_sds$D1 == "4"] <- "3"
normrank_norm_sds$D1[normrank_norm_sds$D1 == "5"] <- "4"
normrank_norm_sds$D1[normrank_norm_sds$D1 == "6"] <- "5"

normrank_normrank_sds$D1[normrank_normrank_sds$D1 == "1"] <- "-99"
normrank_normrank_sds$D1[normrank_normrank_sds$D1 == "2"] <- "1"
normrank_normrank_sds$D1[normrank_normrank_sds$D1 == "3"] <- "2"
normrank_normrank_sds$D1[normrank_normrank_sds$D1 == "4"] <- "3"
normrank_normrank_sds$D1[normrank_normrank_sds$D1 == "5"] <- "4"
normrank_normrank_sds$D1[normrank_normrank_sds$D1 == "6"] <- "5"
# var D3
table(normrank_sample_sds$D3, normrank_normrank_sds$D3)
table(bindori_dataset_threshold_chr$D3)  # -99      1      2      3      4
normrank_sample_sds$D3[normrank_sample_sds$D3 == "1"] <- "-99"
normrank_sample_sds$D3[normrank_sample_sds$D3 == "2"] <- "1"
normrank_sample_sds$D3[normrank_sample_sds$D3 == "3"] <- "2"
normrank_sample_sds$D3[normrank_sample_sds$D3 == "4"] <- "3"
normrank_sample_sds$D3[normrank_sample_sds$D3 == "5"] <- "4"

normrank_norm_sds$D3[normrank_norm_sds$D3 == "1"] <- "-99"
normrank_norm_sds$D3[normrank_norm_sds$D3 == "2"] <- "1"
normrank_norm_sds$D3[normrank_norm_sds$D3 == "3"] <- "2"
normrank_norm_sds$D3[normrank_norm_sds$D3 == "4"] <- "3"
normrank_norm_sds$D3[normrank_norm_sds$D3 == "5"] <- "4"

normrank_normrank_sds$D3[normrank_normrank_sds$D3 == "1"] <- "-99"
normrank_normrank_sds$D3[normrank_normrank_sds$D3 == "2"] <- "1"
normrank_normrank_sds$D3[normrank_normrank_sds$D3 == "3"] <- "2"
normrank_normrank_sds$D3[normrank_normrank_sds$D3 == "4"] <- "3"
normrank_normrank_sds$D3[normrank_normrank_sds$D3 == "5"] <- "4"
# var D4
table(normrank_sample_sds$D4, normrank_normrank_sds$D4)
table(bindori_dataset_threshold_chr$D4)  # -99      1      2      3      4
normrank_sample_sds$D4[normrank_sample_sds$D4 == "1"] <- "-99"
normrank_sample_sds$D4[normrank_sample_sds$D4 == "2"] <- "1"
normrank_sample_sds$D4[normrank_sample_sds$D4 == "3"] <- "2"
normrank_sample_sds$D4[normrank_sample_sds$D4 == "4"] <- "3"
normrank_sample_sds$D4[normrank_sample_sds$D4 == "5"] <- "4"

normrank_norm_sds$D4[normrank_norm_sds$D4 == "1"] <- "-99"
normrank_norm_sds$D4[normrank_norm_sds$D4 == "2"] <- "1"
normrank_norm_sds$D4[normrank_norm_sds$D4 == "3"] <- "2"
normrank_norm_sds$D4[normrank_norm_sds$D4 == "4"] <- "3"
normrank_norm_sds$D4[normrank_norm_sds$D4 == "5"] <- "4"

normrank_normrank_sds$D4[normrank_normrank_sds$D4 == "1"] <- "-99"
normrank_normrank_sds$D4[normrank_normrank_sds$D4 == "2"] <- "1"
normrank_normrank_sds$D4[normrank_normrank_sds$D4 == "3"] <- "2"
normrank_normrank_sds$D4[normrank_normrank_sds$D4 == "4"] <- "3"
normrank_normrank_sds$D4[normrank_normrank_sds$D4 == "5"] <- "4"
# var E3
table(normrank_sample_sds$E3, normrank_normrank_sds$E3)
table(bindori_dataset_threshold_chr$E3)  # -99      1      2      3      4
normrank_sample_sds$E3[normrank_sample_sds$E3 == "1"] <- "-99"
normrank_sample_sds$E3[normrank_sample_sds$E3 == "2"] <- "1"
normrank_sample_sds$E3[normrank_sample_sds$E3 == "3"] <- "2"
normrank_sample_sds$E3[normrank_sample_sds$E3 == "4"] <- "3"
normrank_sample_sds$E3[normrank_sample_sds$E3 == "5"] <- "4"

normrank_norm_sds$E3[normrank_norm_sds$E3 == "1"] <- "-99"
normrank_norm_sds$E3[normrank_norm_sds$E3 == "2"] <- "1"
normrank_norm_sds$E3[normrank_norm_sds$E3 == "3"] <- "2"
normrank_norm_sds$E3[normrank_norm_sds$E3 == "4"] <- "3"
normrank_norm_sds$E3[normrank_norm_sds$E3 == "5"] <- "4"

normrank_normrank_sds$E3[normrank_normrank_sds$E3 == "1"] <- "-99"
normrank_normrank_sds$E3[normrank_normrank_sds$E3 == "2"] <- "1"
normrank_normrank_sds$E3[normrank_normrank_sds$E3 == "3"] <- "2"
normrank_normrank_sds$E3[normrank_normrank_sds$E3 == "4"] <- "3"
normrank_normrank_sds$E3[normrank_normrank_sds$E3 == "5"] <- "4"
# var E4
table(normrank_sample_sds$E4, normrank_normrank_sds$E4)
table(bindori_dataset_threshold_chr$E4)
normrank_sample_sds$E4[normrank_sample_sds$E4 == "1"] <- "-99"
normrank_sample_sds$E4[normrank_sample_sds$E4 == "2"] <- "1"
normrank_sample_sds$E4[normrank_sample_sds$E4 == "3"] <- "2"
normrank_sample_sds$E4[normrank_sample_sds$E4 == "4"] <- "3"
normrank_sample_sds$E4[normrank_sample_sds$E4 == "5"] <- "4"
normrank_sample_sds$E4[normrank_sample_sds$E4 == "6"] <- "5"
normrank_sample_sds$E4[normrank_sample_sds$E4 == "7"] <- "6"
normrank_sample_sds$E4[normrank_sample_sds$E4 == "8"] <- "7"

normrank_norm_sds$E4[normrank_norm_sds$E4 == "1"] <- "-99"
normrank_norm_sds$E4[normrank_norm_sds$E4 == "2"] <- "1"
normrank_norm_sds$E4[normrank_norm_sds$E4 == "3"] <- "2"
normrank_norm_sds$E4[normrank_norm_sds$E4 == "4"] <- "3"
normrank_norm_sds$E4[normrank_norm_sds$E4 == "5"] <- "4"
normrank_norm_sds$E4[normrank_norm_sds$E4 == "6"] <- "5"
normrank_norm_sds$E4[normrank_norm_sds$E4 == "7"] <- "6"
normrank_norm_sds$E4[normrank_norm_sds$E4 == "8"] <- "7"

normrank_normrank_sds$E4[normrank_normrank_sds$E4 == "1"] <- "-99"
normrank_normrank_sds$E4[normrank_normrank_sds$E4 == "2"] <- "1"
normrank_normrank_sds$E4[normrank_normrank_sds$E4 == "3"] <- "2"
normrank_normrank_sds$E4[normrank_normrank_sds$E4 == "4"] <- "3"
normrank_normrank_sds$E4[normrank_normrank_sds$E4 == "5"] <- "4"
normrank_normrank_sds$E4[normrank_normrank_sds$E4 == "6"] <- "5"
normrank_normrank_sds$E4[normrank_normrank_sds$E4 == "7"] <- "6"
normrank_normrank_sds$E4[normrank_normrank_sds$E4 == "8"] <- "7"
# var B4
table(normrank_sample_sds$B4, normrank_norm_sds$B4)
table(bindori_dataset_threshold_chr$B4)  # -99 [0, 1) [1, 5)
normrank_sample_sds$B4[normrank_sample_sds$B4 == "1"] <- "-99"
normrank_sample_sds$B4[normrank_sample_sds$B4 == "2"] <- "[0, 1)"
normrank_sample_sds$B4[normrank_sample_sds$B4 == "3"] <- "[1, 5)"

normrank_norm_sds$B4[normrank_norm_sds$B4 == "1"] <- "-99"
normrank_norm_sds$B4[normrank_norm_sds$B4 == "2"] <- "[0, 1)"
normrank_norm_sds$B4[normrank_norm_sds$B4 == "3"] <- "[1, 5)"

normrank_normrank_sds$B4[normrank_normrank_sds$B4 == "1"] <- "-99"
normrank_normrank_sds$B4[normrank_normrank_sds$B4 == "2"] <- "[0, 1)"
normrank_normrank_sds$B4[normrank_normrank_sds$B4 == "3"] <- "[1, 5)"
# E6
table(normrank_sample_sds$E6, normrank_normrank_sds$E6)
table(bindori_dataset_threshold_chr$E6)  # -99 [0, 9)
normrank_sample_sds$E6[normrank_sample_sds$E6 == "1"] <- "-99"
normrank_sample_sds$E6[normrank_sample_sds$E6 == "2"] <- "[0, 9)"

normrank_norm_sds$E6[normrank_norm_sds$E6 == "1"] <- "-99"
normrank_norm_sds$E6[normrank_norm_sds$E6 == "2"] <- "[0, 9)"

normrank_normrank_sds$E6[normrank_normrank_sds$E6 == "1"] <- "-99"
normrank_normrank_sds$E6[normrank_normrank_sds$E6 == "2"] <- "[0, 9)"
# for cart_sample, we try change all the character type back to factor except "weight"
col_names <- names(normrank_sample_sds)[2:54]
normrank_sample_sds[col_names] <- lapply(normrank_sample_sds[col_names], factor)
normrank_norm_sds[col_names] <- lapply(normrank_norm_sds[col_names], factor)
normrank_normrank_sds[col_names] <- lapply(normrank_normrank_sds[col_names], factor)
str(normrank_sample_sds)
str(normrank_norm_sds)
str(normrank_normrank_sds)

# also load in Terrance's version1 and version2
terrance_v1_filepath <- "./SyntheticData/Terrance/version_1/syn_k2_2020-08-02_2020-08-08-version1.csv"
terrance_v2_filepath <- "./SyntheticData/Terrance/version_2/syn_k2_2020-08-02_2020-08-08-version2.csv"
terrance_v1_sds <- read.csv(file = terrance_v1_filepath, sep = ",")
terrance_v2_sds <- read.csv(file = terrance_v2_filepath, sep = ",")

colnames(cart_sample_sds)

colnames(terrance_v1_sds)[colnames(terrance_v1_sds)=="sample_weight"] <- "weight"
colnames(terrance_v2_sds)[colnames(terrance_v2_sds)=="sample_weight"] <- "weight"
# align the variables
terrance_v1_sds <- drop_na(terrance_v1_sds[colnames(cart_sample_sds)])
terrance_v2_sds <- drop_na(terrance_v2_sds[colnames(cart_sample_sds)])
col_names <- names(terrance_v1_sds)[2:54]
terrance_v1_sds[col_names] <- lapply(terrance_v1_sds[col_names], factor)
terrance_v2_sds[col_names] <- lapply(terrance_v2_sds[col_names], factor)
str(terrance_v1_sds)
str(terrance_v2_sds)
# encode var 2-54 as integer
cart_sample_sds[,2:54] <- sapply(cart_sample_sds[,2:54],as.integer)
cart_norm_sds[,2:54] <- sapply(cart_norm_sds[,2:54],as.integer)
cart_normrank_sds[,2:54] <- sapply(cart_normrank_sds[,2:54],as.integer)
rf_sample_sds[,2:54] <- sapply(rf_sample_sds[,2:54],as.integer)
rf_norm_sds[,2:54] <- sapply(rf_norm_sds[,2:54],as.integer)
rf_normrank_sds[,2:54] <- sapply(rf_normrank_sds[,2:54],as.integer)
bag_sample_sds[,2:54] <- sapply(bag_sample_sds[,2:54],as.integer)
bag_norm_sds[,2:54] <- sapply(bag_norm_sds[,2:54],as.integer)
bag_normrank_sds[,2:54] <- sapply(bag_normrank_sds[,2:54],as.integer)
polyreg_sample_sds[,2:54] <- sapply(polyreg_sample_sds[,2:54],as.integer)
polyreg_norm_sds[,2:54] <- sapply(polyreg_norm_sds[,2:54],as.integer)
polyreg_normrank_sds[,2:54] <- sapply(polyreg_normrank_sds[,2:54],as.integer)
norm_sample_sds[,2:54] <- sapply(norm_sample_sds[,2:54],as.integer)
norm_norm_sds[,2:54] <- sapply(norm_norm_sds[,2:54],as.integer)
norm_normrank_sds[,2:54] <- sapply(norm_normrank_sds[,2:54],as.integer)
normrank_sample_sds[,2:54] <- sapply(normrank_sample_sds[,2:54],as.integer)
normrank_norm_sds[,2:54] <- sapply(normrank_norm_sds[,2:54],as.integer)
normrank_normrank_sds[,2:54] <- sapply(normrank_normrank_sds[,2:54],as.integer)
terrance_v1_sds[,2:54] <- sapply(terrance_v1_sds[,2:54],as.integer)
terrance_v2_sds[,2:54] <- sapply(terrance_v2_sds[,2:54],as.integer)
str(terrance_v2_sds)

table(terrance_v1_sds$F2_1, terrance_v1_sds$B8)
table(cart_norm_sds$F2_1, cart_norm_sds$B8)
# -----------------------------------------------------------------------------
################################# Machine Learning #############################
# -----------------------------------------------------------------------------
# ******** Step 2: fit linear regression to ori and syn datasets

set.seed(2023) # make sure the results are reproducible

#*****************************************************
# Model 1: contact tracing app -- F2_1


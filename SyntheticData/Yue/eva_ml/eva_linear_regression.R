################################################################################
# ----------------------- MachineLearning Evaluation --------------------------#
# This script is meant for the comparison of coef plots based on the synthetic #
# datasets compared to the original dataset                                    #
packages_list <- c("readr", "vroom", "tidyverse", "arsenal", "reshape2",
                   "synthpop", "ggplot2", "dbplyr", "data.table", "caret")
                   # "mlr3", "mlr3learners", "mlr3filters", "mlr3pipelines", 
                   # "mlr3tuning", "mlr3viz", "mlr3verse", "mlr3benchmark",
                   # "e1071", "MASS")
install.packages(packages_list)

# load the libraries
library(readr)
library(vroom)
library(tidyverse, quietly = TRUE)
# install.packages("tidyverse", dependencies = TRUE, type = "source")

library(broom)
library(arsenal)
library(reshape2)
library(synthpop)
library(ggplot2)
library(dbplyr)

# library(mlr3)
# library(mlr3learners)
# library(mlr3filters)
# library(mlr3pipelines)
# library(mlr3tuning)
# library(mlr3viz)
# library(mlr3verse)
# library(mlr3benchmark)
# library(e1071)
# library(MASS)
library(stats)

# set the working directory
wd <- "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
# wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
setwd(wd)

#*******Step 1: load original and synthetic datasets
#*
# then we load the original preprocessed datasets
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)
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

colnames(terrance_v1_sds)[colnames(terrance_v1_sds)=="sample_weight"] <- "weight"
colnames(terrance_v2_sds)[colnames(terrance_v2_sds)=="sample_weight"] <- "weight"
names(cart_sample_sds)
# align the variables
terrance_v1_sds <- drop_na(terrance_v1_sds[colnames(cart_sample_sds)])
terrance_v2_sds <- drop_na(terrance_v2_sds[colnames(cart_sample_sds)])
# ncol(terrance_v1_sds)
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
str(cart_norm_sds)


# -----------------------------------------------------------------------------
################################# lm.synds #############################
# -----------------------------------------------------------------------------
# ******** Step 2: fit linear regression to ori and syn datasets

set.seed(2023) # make sure the results are reproducible

#*****************************************************
# Model 1: contact tracing app -- F2_1
# Step1: prepare the datasets
### Linear model
vars_inc_m1 <- c("D1","D2","D3","D4","D5","D7","D8","D9","E2","E3","E4","E7","E5","E6","F2_1")
ods_m1 <- bindori_dataset_threshold_chr[vars_inc_m1]
sds_cartsample_m1 <- cart_sample_sds[vars_inc_m1]
sds_cartnorm_m1 <- cart_norm_sds[vars_inc_m1]
sds_cartnormrank_m1 <- cart_normrank_sds[vars_inc_m1]
sds_rfsample_m1 <- rf_sample_sds[vars_inc_m1]
sds_rfnorm_m1 <- rf_norm_sds[vars_inc_m1]
sds_rfnormrank_m1 <- rf_normrank_sds[vars_inc_m1]
sds_bagsample_m1 <- bag_sample_sds[vars_inc_m1]
sds_bagnorm_m1 <- bag_norm_sds[vars_inc_m1]
sds_bagnormrank_m1 <- bag_normrank_sds[vars_inc_m1]
sds_polyregsample_m1 <- polyreg_sample_sds[vars_inc_m1]
sds_polyregnorm_m1 <- polyreg_norm_sds[vars_inc_m1]
sds_polyregnormrank_m1 <- polyreg_normrank_sds[vars_inc_m1]
sds_normsample_m1 <- norm_sample_sds[vars_inc_m1]
sds_normnorm_m1 <- norm_norm_sds[vars_inc_m1]
sds_normnormrank_m1 <- norm_normrank_sds[vars_inc_m1]
sds_normranksample_m1 <- normrank_sample_sds[vars_inc_m1]
sds_normranknorm_m1 <- normrank_norm_sds[vars_inc_m1]
sds_normranknormrank_m1 <- normrank_normrank_sds[vars_inc_m1]
sds_terrance_v1_m1 <- terrance_v1_sds[vars_inc_m1]
sds_terrance_v2_m1 <- terrance_v2_sds[vars_inc_m1]

# encode F2_1 as numeric
ods_m1["F2_1"] <- lapply(ods_m1["F2_1"], as.numeric)
sds_cartsample_m1["F2_1"] <- lapply(sds_cartsample_m1["F2_1"], as.numeric)
sds_cartnorm_m1["F2_1"] <- lapply(sds_cartnorm_m1["F2_1"], as.numeric)
sds_cartnormrank_m1["F2_1"] <- lapply(sds_cartnormrank_m1["F2_1"], as.numeric)
sds_rfsample_m1["F2_1"] <- lapply(sds_rfsample_m1["F2_1"], as.numeric)
sds_rfnorm_m1["F2_1"] <- lapply(sds_rfnorm_m1["F2_1"], as.numeric)
sds_rfnormrank_m1["F2_1"] <- lapply(sds_rfnormrank_m1["F2_1"], as.numeric)
sds_bagsample_m1["F2_1"] <- lapply(sds_bagsample_m1["F2_1"], as.numeric)
sds_bagnorm_m1["F2_1"] <- lapply(sds_bagnorm_m1["F2_1"], as.numeric)
sds_bagnormrank_m1["F2_1"] <- lapply(sds_bagnormrank_m1["F2_1"], as.numeric)
sds_polyregsample_m1["F2_1"] <- lapply(sds_polyregsample_m1["F2_1"], as.numeric)
sds_polyregnorm_m1["F2_1"] <- lapply(sds_polyregnorm_m1["F2_1"], as.numeric)
sds_polyregnormrank_m1["F2_1"] <- lapply(sds_polyregnormrank_m1["F2_1"], as.numeric)
sds_normsample_m1["F2_1"] <- lapply(sds_normsample_m1["F2_1"], as.numeric)
sds_normnorm_m1["F2_1"] <- lapply(sds_normnorm_m1["F2_1"], as.numeric)
sds_normnormrank_m1["F2_1"] <- lapply(sds_normnormrank_m1["F2_1"], as.numeric)
sds_normranksample_m1["F2_1"] <- lapply(sds_normranksample_m1["F2_1"], as.numeric)
sds_normranknorm_m1["F2_1"] <- lapply(sds_normranknorm_m1["F2_1"], as.numeric)
sds_normranknormrank_m1["F2_1"] <- lapply(sds_normranknormrank_m1["F2_1"], as.numeric)
sds_terrance_v1_m1["F2_1"] <- lapply(sds_terrance_v1_m1["F2_1"], as.numeric)
sds_terrance_v2_m1["F2_1"] <- lapply(sds_terrance_v2_m1["F2_1"], as.numeric)

# Step 2: Fit a linear regression model ("D1","D2","D3","D4","D5","D7","D8","D9","E2","E3","E4","E7","E5","E6","F2_1")
# test_syn <- syn(sds_cartsample_m1, m=0)
# 
# test_syn$obs.vars
# lm.synds(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
#          data = test_syn)

lm_m1_cartsample <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                       data = sds_cartsample_m1)
lm_m1_cartnorm <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                     data = sds_cartnorm_m1)
lm_m1_cartnormrank <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                         data = sds_cartnormrank_m1)
lm_m1_rfsample <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                       data = sds_rfsample_m1)
lm_m1_rfnorm <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                     data = sds_rfnorm_m1)
lm_m1_rfnormrank <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                         data = sds_rfnormrank_m1)
lm_m1_bagsample <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                     data = sds_bagsample_m1)
lm_m1_bagnorm <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                   data = sds_bagnorm_m1)
lm_m1_bagnormrank <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                       data = sds_bagnormrank_m1)
lm_m1_polyregsample <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                      data = sds_polyregsample_m1)
lm_m1_polyregnorm <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                    data = sds_polyregnorm_m1)
lm_m1_polyregnormrank <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                        data = sds_polyregnormrank_m1)
lm_m1_normsample <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                          data = sds_normsample_m1)
lm_m1_normnorm <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                        data = sds_normnorm_m1)
lm_m1_normnormrank <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                            data = sds_normnormrank_m1)
lm_m1_normranksample <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                       data = sds_normranksample_m1)
lm_m1_normranknorm <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                     data = sds_normranknorm_m1)
lm_m1_normranknormrank <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                         data = sds_normranknormrank_m1)
lm_m1_terranceV1 <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                    data = sds_terrance_v1_m1)
lm_m1_terrnaceV2 <- lm(F2_1 ~ D1 + D2 + D3 + D4 + D5 + D7 + D8 + D9 + E2 + E3 + E4 + E7 + E5 + E6,
                        data = sds_terrance_v2_m1)

compare_rfsample_model2 <- compare(lm_m1_terrnaceV2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")

# Step 3: Extract the coefficients, standard errors, and confidence intervals from the fitted models
coefs_m1_cartsample <- tidy(lm_m1_cartsample)
coefs_m1_cartnorm <- tidy(lm_m1_cartnorm)
coefs_m1_cartnormrank <- tidy(lm_m1_cartnormrank)
coefs_m1_rfsample <- tidy(lm_m1_rfsample)
coefs_m1_rfnorm <- tidy(lm_m1_rfnorm)
coefs_m1_rfnormrank <- tidy(lm_m1_rfnormrank)
coefs_m1_bagsample <- tidy(lm_m1_bagsample)
coefs_m1_bagnorm <- tidy(lm_m1_bagnorm)
coefs_m1_bagnormrank <- tidy(lm_m1_bagnormrank)
coefs_m1_polyregsample <- tidy(lm_m1_polyregsample)
coefs_m1_polyregnorm <- tidy(lm_m1_polyregnorm)
coefs_m1_polyregnormrank <- tidy(lm_m1_polyregnormrank)
coefs_m1_normsample <- tidy(lm_m1_normsample)
coefs_m1_normnorm <- tidy(lm_m1_normnorm)
coefs_m1_normnormrank <- tidy(lm_m1_normnormrank)
coefs_m1_normranksample <- tidy(lm_m1_normranksample)
coefs_m1_normranknorm <- tidy(lm_m1_normranknorm)
coefs_m1_normranknormrank <- tidy(lm_m1_normranknormrank)
coefs_m1_terranceV1 <- tidy(lm_m1_terranceV1)
coefs_m1_terranceV2 <- tidy(lm_m1_terrnaceV2)

# Step 4: Calculate the lower and upper bounds of the confidence intervals for each model
coefs_m1_cartsample$ci_lower <- coefs_m1_cartsample$estimate - 1.96 * coefs_m1_cartsample$std.error
coefs_m1_cartsample$ci_upper <- coefs_m1_cartsample$estimate + 1.96 * coefs_m1_cartsample$std.error
coefs_m1_cartnorm$ci_lower <- coefs_m1_cartnorm$estimate - 1.96 * coefs_m1_cartnorm$std.error
coefs_m1_cartnorm$ci_upper <- coefs_m1_cartnorm$estimate + 1.96 * coefs_m1_cartnorm$std.error
coefs_m1_cartnormrank$ci_lower <- coefs_m1_cartnormrank$estimate - 1.96 * coefs_m1_cartnormrank$std.error
coefs_m1_cartnormrank$ci_upper <- coefs_m1_cartnormrank$estimate + 1.96 * coefs_m1_cartnormrank$std.error
coefs_m1_rfsample$ci_lower <- coefs_m1_rfsample$estimate - 1.96 * coefs_m1_rfsample$std.error
coefs_m1_rfsample$ci_upper <- coefs_m1_rfsample$estimate + 1.96 * coefs_m1_rfsample$std.error
coefs_m1_rfnorm$ci_lower <- coefs_m1_rfnorm$estimate - 1.96 * coefs_m1_rfnorm$std.error
coefs_m1_rfnorm$ci_upper <- coefs_m1_rfnorm$estimate + 1.96 * coefs_m1_rfnorm$std.error
coefs_m1_rfnormrank$ci_lower <- coefs_m1_rfnormrank$estimate - 1.96 * coefs_m1_rfnormrank$std.error
coefs_m1_rfnormrank$ci_upper <- coefs_m1_rfnormrank$estimate + 1.96 * coefs_m1_rfnormrank$std.error
coefs_m1_bagsample$ci_lower <- coefs_m1_bagsample$estimate - 1.96 * coefs_m1_bagsample$std.error
coefs_m1_bagsample$ci_upper <- coefs_m1_bagsample$estimate + 1.96 * coefs_m1_bagsample$std.error
coefs_m1_bagnorm$ci_lower <- coefs_m1_bagnorm$estimate - 1.96 * coefs_m1_bagnorm$std.error
coefs_m1_bagnorm$ci_upper <- coefs_m1_bagnorm$estimate + 1.96 * coefs_m1_bagnorm$std.error
coefs_m1_bagnormrank$ci_lower <- coefs_m1_bagnormrank$estimate - 1.96 * coefs_m1_bagnormrank$std.error
coefs_m1_bagnormrank$ci_upper <- coefs_m1_bagnormrank$estimate + 1.96 * coefs_m1_bagnormrank$std.error
coefs_m1_polyregsample$ci_lower <- coefs_m1_polyregsample$estimate - 1.96 * coefs_m1_polyregsample$std.error
coefs_m1_polyregsample$ci_upper <- coefs_m1_polyregsample$estimate + 1.96 * coefs_m1_polyregsample$std.error
coefs_m1_polyregnorm$ci_lower <- coefs_m1_polyregnorm$estimate - 1.96 * coefs_m1_polyregnorm$std.error
coefs_m1_polyregnorm$ci_upper <- coefs_m1_polyregnorm$estimate + 1.96 * coefs_m1_polyregnorm$std.error
coefs_m1_polyregnormrank$ci_lower <- coefs_m1_polyregnormrank$estimate - 1.96 * coefs_m1_polyregnormrank$std.error
coefs_m1_polyregnormrank$ci_upper <- coefs_m1_polyregnormrank$estimate + 1.96 * coefs_m1_polyregnormrank$std.error
coefs_m1_normsample$ci_lower <- coefs_m1_normsample$estimate - 1.96 * coefs_m1_normsample$std.error
coefs_m1_normsample$ci_upper <- coefs_m1_normsample$estimate + 1.96 * coefs_m1_normsample$std.error
coefs_m1_normnorm$ci_lower <- coefs_m1_normnorm$estimate - 1.96 * coefs_m1_normnorm$std.error
coefs_m1_normnorm$ci_upper <- coefs_m1_normnorm$estimate + 1.96 * coefs_m1_normnorm$std.error
coefs_m1_normnormrank$ci_lower <- coefs_m1_normnormrank$estimate - 1.96 * coefs_m1_normnormrank$std.error
coefs_m1_normnormrank$ci_upper <- coefs_m1_normnormrank$estimate + 1.96 * coefs_m1_normnormrank$std.error
coefs_m1_normranksample$ci_lower <- coefs_m1_normranksample$estimate - 1.96 * coefs_m1_normranksample$std.error
coefs_m1_normranksample$ci_upper <- coefs_m1_normranksample$estimate + 1.96 * coefs_m1_normranksample$std.error
coefs_m1_normranknorm$ci_lower <- coefs_m1_normranknorm$estimate - 1.96 * coefs_m1_normranknorm$std.error
coefs_m1_normranknorm$ci_upper <- coefs_m1_normranknorm$estimate + 1.96 * coefs_m1_normranknorm$std.error
coefs_m1_normranknormrank$ci_lower <- coefs_m1_normranknormrank$estimate - 1.96 * coefs_m1_normranknormrank$std.error
coefs_m1_normranknormrank$ci_upper <- coefs_m1_normranknormrank$estimate + 1.96 * coefs_m1_normranknormrank$std.error
coefs_m1_terranceV1$ci_lower <- coefs_m1_terranceV1$estimate - 1.96 * coefs_m1_terranceV1$std.error
coefs_m1_terranceV1$ci_upper <- coefs_m1_terranceV1$estimate + 1.96 * coefs_m1_terranceV1$std.error
coefs_m1_terranceV2$ci_lower <- coefs_m1_terranceV2$estimate - 1.96 * coefs_m1_terranceV2$std.error
coefs_m1_terranceV2$ci_upper <- coefs_m1_terranceV2$estimate + 1.96 * coefs_m1_terranceV2$std.error

# Add a column to each dataframe to indicate the model
coefs_m1_cartsample$model <- "model_m1_cartsample"
coefs_m1_cartnorm$model <- "model_m1_cartnorm"
coefs_m1_cartnormrank$model <- "model_m1_cartnormrank"
coefs_m1_rfsample$model <- "model_m1_rfsample"
coefs_m1_rfnorm$model <- "model_m1_rfnorm"
coefs_m1_rfnormrank$model <- "model_m1_rfnormrank"
coefs_m1_bagsample$model <- "model_m1_bagsample"
coefs_m1_bagnorm$model <- "model_m1_bagnorm"
coefs_m1_bagnormrank$model <- "model_m1_bagnormrank"
coefs_m1_polyregsample$model <- "model_m1_polyregsample"
coefs_m1_polyregnorm$model <- "model_m1_polyregnorm"
coefs_m1_polyregnormrank$model <- "model_m1_polyregnormrank"
coefs_m1_normsample$model <- "model_m1_normsample"
coefs_m1_normnorm$model <- "model_m1_normnorm"
coefs_m1_normnormrank$model <- "model_m1_normnormrank"
coefs_m1_normranksample$model <- "model_m1_normranksample"
coefs_m1_normranknorm$model <- "model_m1_normranknorm"
coefs_m1_normranknormrank$model <- "model_m1_normranknormrank"
coefs_m1_terranceV1$model <- "model_m1_terranceV1"
coefs_m1_terranceV2$model <- "model_m1_terranceV2"

# Bind the coefficient data for each model
coefs_m1 <- bind_rows(coefs_m1_cartsample, coefs_m1_cartnorm, coefs_m1_cartnormrank,
                      coefs_m1_rfsample, coefs_m1_rfnorm, coefs_m1_rfnormrank,
                      coefs_m1_bagsample, coefs_m1_bagnorm, coefs_m1_bagnormrank,
                      coefs_m1_polyregsample, coefs_m1_polyregnorm, coefs_m1_polyregnormrank,
                      coefs_m1_normsample, coefs_m1_normnorm, coefs_m1_normnormrank,
                      coefs_m1_normranksample, coefs_m1_normranknorm, coefs_m1_normranknormrank,
                      coefs_m1_terranceV1, coefs_m1_terranceV2)

# Create a new column indicating which model the coefficients belong to
coefs_m1$model <- rep(c("cartsample_m1", "cartnorm_m1", "cartnormrank_m1",
                        "rfsample_m1", "rfnorm_m1", "rfnormrank_m1",
                        "bagsample_m1", "bagnorm_m1", "bagnormrank_m1",
                        "polyregsample_m1", "polyregnorm_m1", "polyregnormrank_m1",
                        "normsample_m1", "normnorm_m1", "normnormrank_m1",
                        "normranksample_m1", "normranknorm_m1", "normranknormrank_m1",
                        "terranceV1_m1", "terranceV2_m1"), each = nrow(coefs_m1)/20)

# Step 5: Create the coefficient plot

# Define shapes for each group
shapes <- c(1, 2, 3, 4, 5, 6, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 9, 10)

# Plot the coefficients
ggplot(coefs_m1, aes(x = estimate, y = term, color = model)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, shape = model), width = 0.6) +
  geom_point(aes(shape = model), size = 3) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  ggtitle("Coefficients of Linear Regression Models: Contact Tracing App (F2_1)") +
  scale_shape_manual(values = shapes) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 10),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

# # Step2: new machine learning tasks for ods and sds with Model 1
# tsk_ods_m1 <- TaskRegr$new(id="tsk_ods_m1",
#                            backend=ods_m1, target="F2_1")
# 
# tsk_cartsample_m1 <- TaskRegr$new(id="tsk_cartsample_m1", 
#                                   backend=sds_cartsample_m1, target="F2_1")

#*****************************************************
# Model 2: covid positive -- B8
vars_inc_m2 <- c("E2","E3","E4","E7","E5","E6","C1_m","C2","C3","C5","C6","C7","C8","B8")
ods_m2 <- bindori_dataset_threshold_chr[vars_inc_m2]
sds_cartsample_m2 <- cart_sample_sds[vars_inc_m2]
sds_cartnorm_m2 <- cart_norm_sds[vars_inc_m2]
sds_cartnormrank_m2 <- cart_normrank_sds[vars_inc_m2]
sds_rfsample_m2 <- rf_sample_sds[vars_inc_m2]
sds_rfnorm_m2 <- rf_norm_sds[vars_inc_m2]
sds_rfnormrank_m2 <- rf_normrank_sds[vars_inc_m2]
sds_bagsample_m2 <- bag_sample_sds[vars_inc_m2]
sds_bagnorm_m2 <- bag_norm_sds[vars_inc_m2]
sds_bagnormrank_m2 <- bag_normrank_sds[vars_inc_m2]
sds_polyregsample_m2 <- polyreg_sample_sds[vars_inc_m2]
sds_polyregnorm_m2 <- polyreg_norm_sds[vars_inc_m2]
sds_polyregnormrank_m2 <- polyreg_normrank_sds[vars_inc_m2]
sds_normsample_m2 <- norm_sample_sds[vars_inc_m2]
sds_normnorm_m2 <- norm_norm_sds[vars_inc_m2]
sds_normnormrank_m2 <- norm_normrank_sds[vars_inc_m2]
sds_normranksample_m2 <- normrank_sample_sds[vars_inc_m2]
sds_normranknorm_m2 <- normrank_norm_sds[vars_inc_m2]
sds_normranknormrank_m2 <- normrank_normrank_sds[vars_inc_m2]
sds_terrance_v1_m2 <- terrance_v1_sds[vars_inc_m2]
sds_terrance_v2_m2 <- terrance_v2_sds[vars_inc_m2]

# encode B8 as numeric
ods_m2["B8"] <- lapply(ods_m2["B8"], as.numeric)
sds_cartsample_m2["B8"] <- lapply(sds_cartsample_m2["B8"], as.numeric)
sds_cartnorm_m2["B8"] <- lapply(sds_cartnorm_m2["B8"], as.numeric)
sds_cartnormrank_m2["B8"] <- lapply(sds_cartnormrank_m2["B8"], as.numeric)
sds_rfsample_m2["B8"] <- lapply(sds_rfsample_m2["B8"], as.numeric)
sds_rfnorm_m2["B8"] <- lapply(sds_rfnorm_m2["B8"], as.numeric)
sds_rfnormrank_m2["B8"] <- lapply(sds_rfnormrank_m2["B8"], as.numeric)
sds_bagsample_m2["B8"] <- lapply(sds_bagsample_m2["B8"], as.numeric)
sds_bagnorm_m2["B8"] <- lapply(sds_bagnorm_m2["B8"], as.numeric)
sds_bagnormrank_m2["B8"] <- lapply(sds_bagnormrank_m2["B8"], as.numeric)
sds_polyregsample_m2["B8"] <- lapply(sds_polyregsample_m2["B8"], as.numeric)
sds_polyregnorm_m2["B8"] <- lapply(sds_polyregnorm_m2["B8"], as.numeric)
sds_polyregnormrank_m2["B8"] <- lapply(sds_polyregnormrank_m2["B8"], as.numeric)
sds_normsample_m2["B8"] <- lapply(sds_normsample_m2["B8"], as.numeric)
sds_normnorm_m2["B8"] <- lapply(sds_normnorm_m2["B8"], as.numeric)
sds_normnormrank_m2["B8"] <- lapply(sds_normnormrank_m2["B8"], as.numeric)
sds_normranksample_m2["B8"] <- lapply(sds_normranksample_m2["B8"], as.numeric)
sds_normranknorm_m2["B8"] <- lapply(sds_normranknorm_m2["B8"], as.numeric)
sds_normranknormrank_m2["B8"] <- lapply(sds_normranknormrank_m2["B8"], as.numeric)
sds_terrance_v1_m2["B8"] <- lapply(sds_terrance_v1_m2["B8"], as.numeric)
sds_terrance_v2_m2["B8"] <- lapply(sds_terrance_v2_m2["B8"], as.numeric)

# Step 2: Fit a linear regression model "E2","E3","E4","E7","E5","E6","C1_m","C2","C3","C5","C6","C7","C8","B8"
lm_m2_cartsample <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                       data = sds_cartsample_m2)
lm_m2_cartnorm <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                     data = sds_cartnorm_m2)
lm_m2_cartnormrank <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                         data = sds_cartnormrank_m2)
lm_m2_rfsample <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                     data = sds_rfsample_m2)
lm_m2_rfnorm <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                   data = sds_rfnorm_m2)
lm_m2_rfnormrank <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                       data = sds_rfnormrank_m2)
lm_m2_bagsample <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                      data = sds_bagsample_m2)
lm_m2_bagnorm <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                    data = sds_bagnorm_m2)
lm_m2_bagnormrank <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                        data = sds_bagnormrank_m2)
lm_m2_polyregsample <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                          data = sds_polyregsample_m2)
lm_m2_polyregnorm <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                        data = sds_polyregnorm_m2)
lm_m2_polyregnormrank <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                            data = sds_polyregnormrank_m2)
lm_m2_normsample <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                       data = sds_normsample_m2)
lm_m2_normnorm <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                     data = sds_normnorm_m2)
lm_m2_normnormrank <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                         data = sds_normnormrank_m2)
lm_m2_normranksample <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                           data = sds_normranksample_m2)
lm_m2_normranknorm <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                         data = sds_normranknorm_m2)
lm_m2_normranknormrank <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                             data = sds_normranknormrank_m2)
lm_m2_terranceV1 <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                       data = sds_terrance_v1_m2)
lm_m2_terrnaceV2 <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                       data = sds_terrance_v2_m2)


# Step 3: Extract the coefficients, standard errors, and confidence intervals from the fitted models
coefs_m2_cartsample <- tidy(lm_m2_cartsample)
coefs_m2_cartnorm <- tidy(lm_m2_cartnorm)
coefs_m2_cartnormrank <- tidy(lm_m2_cartnormrank)
coefs_m2_rfsample <- tidy(lm_m2_rfsample)
coefs_m2_rfnorm <- tidy(lm_m2_rfnorm)
coefs_m2_rfnormrank <- tidy(lm_m2_rfnormrank)
coefs_m2_bagsample <- tidy(lm_m2_bagsample)
coefs_m2_bagnorm <- tidy(lm_m2_bagnorm)
coefs_m2_bagnormrank <- tidy(lm_m2_bagnormrank)
coefs_m2_polyregsample <- tidy(lm_m2_polyregsample)
coefs_m2_polyregnorm <- tidy(lm_m2_polyregnorm)
coefs_m2_polyregnormrank <- tidy(lm_m2_polyregnormrank)
coefs_m2_normsample <- tidy(lm_m2_normsample)
coefs_m2_normnorm <- tidy(lm_m2_normnorm)
coefs_m2_normnormrank <- tidy(lm_m2_normnormrank)
coefs_m2_normranksample <- tidy(lm_m2_normranksample)
coefs_m2_normranknorm <- tidy(lm_m2_normranknorm)
coefs_m2_normranknormrank <- tidy(lm_m2_normranknormrank)
coefs_m2_terranceV1 <- tidy(lm_m2_terranceV1)
coefs_m2_terranceV2 <- tidy(lm_m2_terrnaceV2)

# Step 4: Calculate the lower and upper bounds of the confidence intervals for each model
coefs_m2_cartsample$ci_lower <- coefs_m2_cartsample$estimate - 1.96 * coefs_m2_cartsample$std.error
coefs_m2_cartsample$ci_upper <- coefs_m2_cartsample$estimate + 1.96 * coefs_m2_cartsample$std.error
coefs_m2_cartnorm$ci_lower <- coefs_m2_cartnorm$estimate - 1.96 * coefs_m2_cartnorm$std.error
coefs_m2_cartnorm$ci_upper <- coefs_m2_cartnorm$estimate + 1.96 * coefs_m2_cartnorm$std.error
coefs_m2_cartnormrank$ci_lower <- coefs_m2_cartnormrank$estimate - 1.96 * coefs_m2_cartnormrank$std.error
coefs_m2_cartnormrank$ci_upper <- coefs_m2_cartnormrank$estimate + 1.96 * coefs_m2_cartnormrank$std.error
coefs_m2_rfsample$ci_lower <- coefs_m2_rfsample$estimate - 1.96 * coefs_m2_rfsample$std.error
coefs_m2_rfsample$ci_upper <- coefs_m2_rfsample$estimate + 1.96 * coefs_m2_rfsample$std.error
coefs_m2_rfnorm$ci_lower <- coefs_m2_rfnorm$estimate - 1.96 * coefs_m2_rfnorm$std.error
coefs_m2_rfnorm$ci_upper <- coefs_m2_rfnorm$estimate + 1.96 * coefs_m2_rfnorm$std.error
coefs_m2_rfnormrank$ci_lower <- coefs_m2_rfnormrank$estimate - 1.96 * coefs_m2_rfnormrank$std.error
coefs_m2_rfnormrank$ci_upper <- coefs_m2_rfnormrank$estimate + 1.96 * coefs_m2_rfnormrank$std.error
coefs_m2_bagsample$ci_lower <- coefs_m2_bagsample$estimate - 1.96 * coefs_m2_bagsample$std.error
coefs_m2_bagsample$ci_upper <- coefs_m2_bagsample$estimate + 1.96 * coefs_m2_bagsample$std.error
coefs_m2_bagnorm$ci_lower <- coefs_m2_bagnorm$estimate - 1.96 * coefs_m2_bagnorm$std.error
coefs_m2_bagnorm$ci_upper <- coefs_m2_bagnorm$estimate + 1.96 * coefs_m2_bagnorm$std.error
coefs_m2_bagnormrank$ci_lower <- coefs_m2_bagnormrank$estimate - 1.96 * coefs_m2_bagnormrank$std.error
coefs_m2_bagnormrank$ci_upper <- coefs_m2_bagnormrank$estimate + 1.96 * coefs_m2_bagnormrank$std.error
coefs_m2_polyregsample$ci_lower <- coefs_m2_polyregsample$estimate - 1.96 * coefs_m2_polyregsample$std.error
coefs_m2_polyregsample$ci_upper <- coefs_m2_polyregsample$estimate + 1.96 * coefs_m2_polyregsample$std.error
coefs_m2_polyregnorm$ci_lower <- coefs_m2_polyregnorm$estimate - 1.96 * coefs_m2_polyregnorm$std.error
coefs_m2_polyregnorm$ci_upper <- coefs_m2_polyregnorm$estimate + 1.96 * coefs_m2_polyregnorm$std.error
coefs_m2_polyregnormrank$ci_lower <- coefs_m2_polyregnormrank$estimate - 1.96 * coefs_m2_polyregnormrank$std.error
coefs_m2_polyregnormrank$ci_upper <- coefs_m2_polyregnormrank$estimate + 1.96 * coefs_m2_polyregnormrank$std.error
coefs_m2_normsample$ci_lower <- coefs_m2_normsample$estimate - 1.96 * coefs_m2_normsample$std.error
coefs_m2_normsample$ci_upper <- coefs_m2_normsample$estimate + 1.96 * coefs_m2_normsample$std.error
coefs_m2_normnorm$ci_lower <- coefs_m2_normnorm$estimate - 1.96 * coefs_m2_normnorm$std.error
coefs_m2_normnorm$ci_upper <- coefs_m2_normnorm$estimate + 1.96 * coefs_m2_normnorm$std.error
coefs_m2_normnormrank$ci_lower <- coefs_m2_normnormrank$estimate - 1.96 * coefs_m2_normnormrank$std.error
coefs_m2_normnormrank$ci_upper <- coefs_m2_normnormrank$estimate + 1.96 * coefs_m2_normnormrank$std.error
coefs_m2_normranksample$ci_lower <- coefs_m2_normranksample$estimate - 1.96 * coefs_m2_normranksample$std.error
coefs_m2_normranksample$ci_upper <- coefs_m2_normranksample$estimate + 1.96 * coefs_m2_normranksample$std.error
coefs_m2_normranknorm$ci_lower <- coefs_m2_normranknorm$estimate - 1.96 * coefs_m2_normranknorm$std.error
coefs_m2_normranknorm$ci_upper <- coefs_m2_normranknorm$estimate + 1.96 * coefs_m2_normranknorm$std.error
coefs_m2_normranknormrank$ci_lower <- coefs_m2_normranknormrank$estimate - 1.96 * coefs_m2_normranknormrank$std.error
coefs_m2_normranknormrank$ci_upper <- coefs_m2_normranknormrank$estimate + 1.96 * coefs_m2_normranknormrank$std.error
coefs_m2_terranceV1$ci_lower <- coefs_m2_terranceV1$estimate - 1.96 * coefs_m2_terranceV1$std.error
coefs_m2_terranceV1$ci_upper <- coefs_m2_terranceV1$estimate + 1.96 * coefs_m2_terranceV1$std.error
coefs_m2_terranceV2$ci_lower <- coefs_m2_terranceV2$estimate - 1.96 * coefs_m2_terranceV2$std.error
coefs_m2_terranceV2$ci_upper <- coefs_m2_terranceV2$estimate + 1.96 * coefs_m2_terranceV2$std.error

# Add a column to each dataframe to indicate the model
coefs_m2_cartsample$model <- "model_m2_cartsample"
coefs_m2_cartnorm$model <- "model_m2_cartnorm"
coefs_m2_cartnormrank$model <- "model_m2_cartnormrank"
coefs_m2_rfsample$model <- "model_m2_rfsample"
coefs_m2_rfnorm$model <- "model_m2_rfnorm"
coefs_m2_rfnormrank$model <- "model_m2_rfnormrank"
coefs_m2_bagsample$model <- "model_m2_bagsample"
coefs_m2_bagnorm$model <- "model_m2_bagnorm"
coefs_m2_bagnormrank$model <- "model_m2_bagnormrank"
coefs_m2_polyregsample$model <- "model_m2_polyregsample"
coefs_m2_polyregnorm$model <- "model_m2_polyregnorm"
coefs_m2_polyregnormrank$model <- "model_m2_polyregnormrank"
coefs_m2_normsample$model <- "model_m2_normsample"
coefs_m2_normnorm$model <- "model_m2_normnorm"
coefs_m2_normnormrank$model <- "model_m2_normnormrank"
coefs_m2_normranksample$model <- "model_m2_normranksample"
coefs_m2_normranknorm$model <- "model_m2_normranknorm"
coefs_m2_normranknormrank$model <- "model_m2_normranknormrank"
coefs_m2_terranceV1$model <- "model_m2_terranceV1"
coefs_m2_terranceV2$model <- "model_m2_terranceV2"

# Bind the coefficient data for each model
coefs_m2 <- bind_rows(coefs_m2_cartsample, coefs_m2_cartnorm, coefs_m2_cartnormrank,
                      coefs_m2_rfsample, coefs_m2_rfnorm, coefs_m2_rfnormrank,
                      coefs_m2_bagsample, coefs_m2_bagnorm, coefs_m2_bagnormrank,
                      coefs_m2_polyregsample, coefs_m2_polyregnorm, coefs_m2_polyregnormrank,
                      coefs_m2_normsample, coefs_m2_normnorm, coefs_m2_normnormrank,
                      coefs_m2_normranksample, coefs_m2_normranknorm, coefs_m2_normranknormrank,
                      coefs_m2_terranceV1, coefs_m2_terranceV2)

# Create a new column indicating which model the coefficients belong to
coefs_m2$model <- rep(c("cartsample_m2", "cartnorm_m2", "cartnormrank_m2",
                        "rfsample_m2", "rfnorm_m2", "rfnormrank_m2",
                        "bagsample_m2", "bagnorm_m2", "bagnormrank_m2",
                        "polyregsample_m2", "polyregnorm_m2", "polyregnormrank_m2",
                        "normsample_m2", "normnorm_m2", "normnormrank_m2",
                        "normranksample_m2", "normranknorm_m2", "normranknormrank_m2",
                        "terranceV1_m2", "terranceV2_m2"), each = nrow(coefs_m2)/20)

# Step 5: Create the coefficient plot

# Define shapes for each group
shapes <- c(1, 2, 3, 4, 5, 6, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 9, 10)

# Plot the coefficients
ggplot(coefs_m2, aes(x = estimate, y = term, color = model)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, shape = model), width = 0.6) +
  geom_point(aes(shape = model), size = 3) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  ggtitle("Coefficients of Linear Regression Models: Covid Positive (B8)") +
  scale_shape_manual(values = shapes) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 10),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))










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
library(tidyverse, quietly = TRUE)
# install.packages("tidyverse", dependencies = TRUE, type = "source")
library(broom)
library(arsenal)
library(reshape2)
library(synthpop)
library(ggplot2)
library(dbplyr)
library(broom)

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
# wd <- "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
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

colnames(terrance_v1_sds)[colnames(terrance_v1_sds)=="sample_weight"] <- "weight"
colnames(terrance_v2_sds)[colnames(terrance_v2_sds)=="sample_weight"] <- "weight"
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
################################# Machine Learning #############################
# -----------------------------------------------------------------------------
# ******** Step 2: fit linear regression to ori and syn datasets

set.seed(2023) # make sure the results are reproducible

#*****************************************************
# Model 1: contact tracing app -- F2_1

# Step1: prepare the datasets
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
# Step2: new machine learning tasks for ods and sds with Model 1
tsk_ods_m1 <- TaskRegr$new(id="tsk_ods_m1",
                           backend=ods_m1, target="F2_1")

tsk_cartsample_m1 <- TaskRegr$new(id="tsk_cartsample_m1", 
                                  backend=sds_cartsample_m1, target="F2_1")

tsk_cartnorm_m1 <- TaskRegr$new(id="tsk_cartnorm_m1", 
                                backend=sds_cartnorm_m1, target="F2_1")

tsk_cartnormrank_m1 <- TaskRegr$new(id="tsk_cartnormrank_m1",
                                    backend=sds_cartnormrank_m1, target="F2_1")

tsk_rfsample_m1 <- TaskRegr$new(id="tsk_rfsample_m1", 
                                  backend=sds_rfsample_m1, target="F2_1")

tsk_rfnorm_m1 <- TaskRegr$new(id="tsk_rfnorm_m1", 
                                backend=sds_rfnorm_m1, target="F2_1")

tsk_rfnormrank_m1 <- TaskRegr$new(id="tsk_rfnormrank_m1",
                                    backend=sds_rfnormrank_m1, target="F2_1")

tsk_bagsample_m1 <- TaskRegr$new(id="tsk_bagsample_m1", 
                                backend=sds_bagsample_m1, target="F2_1")

tsk_bagnorm_m1 <- TaskRegr$new(id="tsk_bagnorm_m1", 
                              backend=sds_bagnorm_m1, target="F2_1")

tsk_bagnormrank_m1 <- TaskRegr$new(id="tsk_bagnormrank_m1",
                                  backend=sds_bagnormrank_m1, target="F2_1")

tsk_polyregsample_m1 <- TaskRegr$new(id="tsk_polyregsample_m1", 
                                 backend=sds_polyregsample_m1, target="F2_1")

tsk_polyregnorm_m1 <- TaskRegr$new(id="tsk_polyregnorm_m1", 
                               backend=sds_polyregnorm_m1, target="F2_1")

tsk_polyregnormrank_m1 <- TaskRegr$new(id="tsk_polyregnormrank_m1",
                                   backend=sds_polyregnormrank_m1, target="F2_1")

tsk_normsample_m1 <- TaskRegr$new(id="tsk_normsample_m1", 
                                 backend=sds_normsample_m1, target="F2_1")

tsk_normnorm_m1 <- TaskRegr$new(id="tsk_normnorm_m1", 
                               backend=sds_normnorm_m1, target="F2_1")

tsk_normnormrank_m1 <- TaskRegr$new(id="tsk_normnormrank_m1",
                                   backend=sds_normnormrank_m1, target="F2_1")

tsk_normranksample_m1 <- TaskRegr$new(id="tsk_normranksample_m1", 
                                  backend=sds_normranksample_m1, target="F2_1")

tsk_normranknorm_m1 <- TaskRegr$new(id="tsk_normranknorm_m1", 
                                backend=sds_normranknorm_m1, target="F2_1")

tsk_normranknormrank_m1 <- TaskRegr$new(id="tsk_normranknormrank_m1",
                                    backend=sds_normranknormrank_m1, target="F2_1")

tsk_terranceV1_m1 <- TaskRegr$new(id="tsk_terranceV1_m1",
                                  backend=sds_terrance_v1_m1, target="F2_1")

tsk_terranceV2_m1 <- TaskRegr$new(id="tsk_terranceV2_m1",
                                  backend=sds_terrance_v2_m1, target="F2_1")

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

# Step2: new machine learning tasks for ods and sds
tsk_ods_m2 <- TaskRegr$new(id="tsk_ods_m2",
                           backend=ods_m2, target="B8")

tsk_cartsample_m2 <- TaskRegr$new(id="tsk_cartsample_m2", 
                                  backend=sds_cartsample_m2, target="B8")

tsk_cartnorm_m2 <- TaskRegr$new(id="tsk_cartnorm_m2", 
                                backend=sds_cartnorm_m2, target="B8")

tsk_cartnormrank_m2 <- TaskRegr$new(id="tsk_cartnormrank_m2",
                                    backend=sds_cartnormrank_m2, target="B8")

tsk_rfsample_m2 <- TaskRegr$new(id="tsk_rfsample_m2", 
                                backend=sds_rfsample_m2, target="B8")

tsk_rfnorm_m2 <- TaskRegr$new(id="tsk_rfnorm_m2", 
                              backend=sds_rfnorm_m2, target="B8")

tsk_rfnormrank_m2 <- TaskRegr$new(id="tsk_rfnormrank_m2",
                                  backend=sds_rfnormrank_m2, target="B8")

tsk_bagsample_m2 <- TaskRegr$new(id="tsk_bagsample_m2", 
                                 backend=sds_bagsample_m2, target="B8")

tsk_bagnorm_m2 <- TaskRegr$new(id="tsk_bagnorm_m2", 
                               backend=sds_bagnorm_m2, target="B8")

tsk_bagnormrank_m2 <- TaskRegr$new(id="tsk_bagnormrank_m2",
                                   backend=sds_bagnormrank_m2, target="B8")

tsk_polyregsample_m2 <- TaskRegr$new(id="tsk_polyregsample_m2", 
                                     backend=sds_polyregsample_m2, target="B8")

tsk_polyregnorm_m2 <- TaskRegr$new(id="tsk_polyregnorm_m2", 
                                   backend=sds_polyregnorm_m2, target="B8")

tsk_polyregnormrank_m2 <- TaskRegr$new(id="tsk_polyregnormrank_m2",
                                       backend=sds_polyregnormrank_m2, target="B8")

tsk_normsample_m2 <- TaskRegr$new(id="tsk_normsample_m2", 
                                  backend=sds_normsample_m2, target="B8")

tsk_normnorm_m2 <- TaskRegr$new(id="tsk_normnorm_m2", 
                                backend=sds_normnorm_m2, target="B8")

tsk_normnormrank_m2 <- TaskRegr$new(id="tsk_normnormrank_m2",
                                    backend=sds_normnormrank_m2, target="B8")

tsk_normranksample_m2 <- TaskRegr$new(id="tsk_normranksample_m2", 
                                      backend=sds_normranksample_m2, target="B8")

tsk_normranknorm_m2 <- TaskRegr$new(id="tsk_normranknorm_m2", 
                                    backend=sds_normranknorm_m2, target="B8")

tsk_normranknormrank_m2 <- TaskRegr$new(id="tsk_normranknormrank_m2",
                                        backend=sds_normranknormrank_m2, target="B8")

tsk_terranceV1_m2 <- TaskRegr$new(id="tsk_terranceV1_m2",
                                  backend=sds_terrance_v1_m2, target="B8")

tsk_terranceV2_m2 <- TaskRegr$new(id="tsk_terranceV2_m2",
                                  backend=sds_terrance_v2_m2, target="B8")

tasks_list <- list(tsk_ods_m1, tsk_cartsample_m1,tsk_cartnorm_m1, tsk_cartnormrank_m1,
                   tsk_rfsample_m1, tsk_rfnorm_m1, tsk_rfnormrank_m1,
                   tsk_bagsample_m1, tsk_bagnorm_m1, tsk_bagnormrank_m1,
                   tsk_polyregsample_m1, tsk_polyregnorm_m1, tsk_polyregnormrank_m1,
                   tsk_normsample_m1, tsk_normnorm_m1, tsk_normnormrank_m1,
                   tsk_normranksample_m1, tsk_normranknorm_m1, tsk_normranknormrank_m1,
                   tsk_terranceV1_m1, tsk_terranceV2_m1,
                   tsk_ods_m2, tsk_cartsample_m2,tsk_cartnorm_m2, tsk_cartnormrank_m2,
                   tsk_rfsample_m2, tsk_rfnorm_m2, tsk_rfnormrank_m2,
                   tsk_bagsample_m2, tsk_bagnorm_m2, tsk_bagnormrank_m2,
                   tsk_polyregsample_m2, tsk_polyregnorm_m2, tsk_polyregnormrank_m2,
                   tsk_normsample_m2, tsk_normnorm_m2, tsk_normnormrank_m2,
                   tsk_normranksample_m2, tsk_normranknorm_m2, tsk_normranknormrank_m2,
                   tsk_terranceV1_m2, tsk_terranceV2_m2)

# Step3: prepare the required learner
learner_regr <- lrn("regr.lm")

# Step4: benchmark the tasks and learner with resampling
# benchmark_grid is the design
bm_models <- benchmark(benchmark_grid(tasks = tasks_list,
                                      learners = learner_regr,
                                      resamplings = rsmp("holdout", ratio=0.8)),
                       
                       store_models = TRUE)
# Step5: validate the accuracy of the model
#****** Measure to compare true observed 
#****** labels with predicted labels in 
#****** multiclass classification tasks.
bm_models$aggregate(msr("regr.rmse"))[learner_id == "regr.lm",]

# step7: save bm_model as rds
saveRDS(bm_models, './SyntheticData/Yue/bm_models_lm.rds')

# step6: extract the coefficients of the trained instances
mlr3misc::map(as.data.table(bm_models)$learner, "model")[[40]][1]$coefficients

as.data.table(bm_models)$learner

# Fit a linear regression model
model_v2_cartsample <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
            data = sds_cartsample_m2)
model_v2_cartnorm <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                          data = sds_cartnorm_m2)
model_v2_cartnormrank <- lm(B8 ~ E2 + E3 + E4 + E7 + E5 + E6 + C1_m + C2 + C3 + C5 + C6 + C7 + C8,
                        data = sds_cartnormrank_m2)
# Extract the coefficients, standard errors, and confidence intervals from the models
coefs_v2_cartsample <- tidy(model_v2_cartsample)
coefs_v2_cartnorm <- tidy(model_v2_cartnorm)
coefs_v2_cartnormrank <- tidy(model_v2_cartnormrank)

# Calculate the lower and upper bounds of the confidence intervals for each model
coefs_v2_cartsample$ci_lower <- coefs_v2_cartsample$estimate - 1.96 * coefs_v2_cartsample$std.error
coefs_v2_cartsample$ci_upper <- coefs_v2_cartsample$estimate + 1.96 * coefs_v2_cartsample$std.error
coefs_v2_cartnorm$ci_lower <- coefs_v2_cartnorm$estimate - 1.96 * coefs_v2_cartnorm$std.error
coefs_v2_cartnorm$ci_upper <- coefs_v2_cartnorm$estimate + 1.96 * coefs_v2_cartnorm$std.error
coefs_v2_cartnormrank$ci_lower <- coefs_v2_cartnormrank$estimate - 1.96 * coefs_v2_cartnormrank$std.error
coefs_v2_cartnormrank$ci_upper <- coefs_v2_cartnormrank$estimate + 1.96 * coefs_v2_cartnormrank$std.error

# Add a column to each dataframe to indicate the model
coefs_v2_cartsample$model <- "model_v2_cartsample"
coefs_v2_cartnorm$model <- "model_v2_cartnorm"
coefs_v2_cartnormrank$model <- "model_v2_cartnormrank"

# Bind the coefficient data for each model
coefs_v2 <- bind_rows(coefs_v2_cartsample, coefs_v2_cartnorm, coefs_v2_cartnormrank)

# Create the plot
ggplot(coefs_v2, aes(x = term, y = estimate, color = model)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  theme_classic() +
  ggtitle("Coefficients of Linear Regression Model") +
  xlab("parameter") +
  ylab("coefficient")

# Create a new column indicating which model the coefficients belong to
coefs_v2$model <- rep(c("cartsample_m2", "cartnorm_m2", "cartnormrank_m2"), each = nrow(coefs_v2)/3)

# Create the coefficient plot
ggplot(coefs_v2, aes(x = term, y = estimate, color = model)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.6) +
  geom_hline(yintercept = 0) +
  geom_point() +
  theme_classic() +
  ggtitle("Coefficients of Linear Regression Model with Confidence Interval") +
  xlab("Parameter") +
  ylab("Estimate") +
  theme(legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"))










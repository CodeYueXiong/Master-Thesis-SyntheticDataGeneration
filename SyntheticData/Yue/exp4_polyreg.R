##########################################################################
########---------------experiment with polyreg-------------------#########
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
wd <- "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
# wd <- "/Volumes/ru27req/MasterThesisRoxy/Master-Thesis-DifferentialPrivacy"
setwd(wd)
wd <- "/Users/roxy/Desktop/Master-Thesis-SyntheticDataGeneration"
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
bindori_dataset_threshold_chr$B2 <- as.factor(bindori_dataset_threshold_chr$B2)
# var B4,
#    -99 [0, 1) [1, 5)
# 249422    290  10587
#      1      2      3 
# 249422    290  10587 
# bindori_dataset_threshold_chr$B4 <- as.integer(bindori_dataset_threshold_chr$B4)
# var E5,
#   -99 [0, 1) [1, 2) 
# 45595   8766 205938 
#     1      2      3 
# 45595   8766 205938
# bindori_dataset_threshold_chr$E5 <- as.integer(bindori_dataset_threshold_chr$E5)
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
# also for those B1b_x like vars and D10, we try exclude them from the synthesis
cols_rm_bd <- c("B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
                "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11","B1b_x12", "B1b_x13", "D10",
                "C0_1", "C0_2", "C0_3", "C0_4", "C0_5", "C0_6")

bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(-all_of(cols_rm_bd))
ncol(bindori_dataset_threshold_chr)==54

# Calculate the percentage of -99 across all variables in the dataframe
missing_percent <- as.vector(apply(bindori_dataset_threshold_chr == -99, 2, function(x) sum(x)/length(x) * 100))
# Create empty vectors to store the variable names and values
var_names <- names(bindori_dataset_threshold_chr)
length(var_names)
var_values <- round(missing_percent, 2)
length(var_values)

# Create a dataframe to store the results
df <- data.frame(variable = var_names, value = var_values)
write.csv(df, "./SyntheticData/missingPercent.csv", row.names=FALSE)
# Loop through each element of the list
for (i in seq_along(mylist)) {
  
  # Check if the element is a numeric value
  if (is.numeric(mylist[[i]])) {
    # If it is, add the value to the var_values vector
    var_values <- c(var_values, mylist[[i]])
    
    # Add the corresponding variable name to the var_names vector
    var_names <- c(var_names, names(mylist)[i])
  } else {
    # If it is not, skip to the next element
    next
  }
}



##########################################################################
######---------------synthetic data with synthpop-------------------######
##########################################################################

# first of all, we try extract all the methods with m=0 set in cart
settings_default <- syn(bindori_dataset_threshold_chr, method = "cart", m = 0)
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

syn_polyreg_experiment <- function(para_weight_list, index, bindori_dataset_threshold_chr, arg_method, arg_col) {
  ### specify the method to use for group of vars ###
  # E3 and weight
  arg_method[['E3']] <- "sample"
  arg_method[['weight']] <- para_weight_list[index]  # sample(1), norm(2), normrank(3) to choose
  # E2, E4, E5, E7, ranger for exp_polyreg
  arg_method[['E2']] <- "polyreg"
  arg_method[['E4']] <- "polyreg"
  arg_method[['E5']] <- "polyreg"
  arg_method[['E7']] <- "polyreg"
  # B3 to B11, ranger
  arg_method[['B3']] <- "polyreg"
  arg_method[['B4']] <- "polyreg"
  arg_method[['B5']] <- "polyreg"
  arg_method[['B6']] <- "polyreg"
  arg_method[['B7']] <- "polyreg"
  arg_method[['B8']] <- "polyreg"
  arg_method[['B9']] <- "polyreg"
  arg_method[['B10']] <- "polyreg"
  arg_method[['B11']] <- "polyreg"
  # C3, C8
  arg_method[['C3']] <- "polyreg"
  arg_method[['C8']] <- "polyreg"
  # C2, C4, C5, C6
  arg_method[['C2']] <- "polyreg"
  arg_method[['C4']] <- "polyreg"
  arg_method[['C5']] <- "polyreg"
  arg_method[['C6']] <- "polyreg"
  # D1, D2, D4, D5
  arg_method[['D1']] <- "polyreg"
  arg_method[['D2']] <- "polyreg"
  arg_method[['D4']] <- "polyreg"
  arg_method[['D5']] <- "polyreg"
  
  syn_dataset <- NULL
  syn_dataset <- syn(bindori_dataset_threshold_chr, method = arg_method[c(2:54,1)], visit.sequence = arg_col[c(2:54, 1)], polyreg.maxit=10000)
  
  write.syn(syn_dataset, filename = paste("polyreg", para_weight_list[index], "syn", sep="_"), filetype = "rda")
  message("syn done!")
}

# tryout for polyreg_sample
sds_polyregsample_tryout <- syn_polyreg_experiment(para_weight_list, index=1, bindori_dataset_threshold_chr, arg_method, arg_col)
# tryout for polyreg_norm
sds_polyregnorm_tryout <- syn_polyreg_experiment(para_weight_list, index=2, bindori_dataset_threshold_chr, arg_method, arg_col)
# tryout for polyreg_normrank
sds_polyregnormrank_tryout <- syn_polyreg_experiment(para_weight_list, index=3, bindori_dataset_threshold_chr, arg_method, arg_col)
# ##########################################################
# ------------------ terrancev1 ----------------------------
# ##########################################################
# load in Terrance's version1 and version2
terrance_v1_filepath <- "./SyntheticData/Terrance/version_1/syn_k2_2020-08-02_2020-08-08-version1.csv"
terrance_v2_filepath <- "./SyntheticData/Terrance/version_2/syn_k2_2020-08-02_2020-08-08-version2.csv"
terrance_v1_sds <- read.csv(file = terrance_v1_filepath, sep = ",")
terrance_v2_sds <- read.csv(file = terrance_v2_filepath, sep = ",")

colnames(terrance_v1_sds)[colnames(terrance_v1_sds)=="sample_weight"] <- "weight"
colnames(terrance_v2_sds)[colnames(terrance_v2_sds)=="sample_weight"] <- "weight"
nrow(terrance_v1_sds)
nrow(terrance_v2_sds)
# align the variables
terrance_v1_sds <- drop_na(terrance_v1_sds[colnames(bindori_dataset_threshold_chr)])
terrance_v2_sds <- drop_na(terrance_v2_sds[colnames(bindori_dataset_threshold_chr)])
# ncol(terrance_v1_sds)
col_names <- names(terrance_v1_sds)[2:54]
terrance_v1_sds[col_names] <- lapply(terrance_v1_sds[col_names], factor)
terrance_v2_sds[col_names] <- lapply(terrance_v2_sds[col_names], factor)
str(terrance_v1_sds)
str(terrance_v2_sds)
# encode variables as integers for ods and polyregsample
bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
terrance_v1_sds[,2:54] <- sapply(terrance_v1_sds[,2:54],as.integer)
terrance_v2_sds[,2:54] <- sapply(terrance_v2_sds[,2:54],as.integer)

utility.gen.tv1 <- utility.gen(terrance_v1_sds, bindori_dataset_threshold_chr, 
                               not.synthesised = NULL, cont.na = NULL, 
                               print.stats = c("pMSE"))
utility.gen.tv2 <- utility.gen(terrance_v2_sds, bindori_dataset_threshold_chr, 
                               not.synthesised = NULL, cont.na = NULL, 
                               print.stats = c("pMSE"))

sds_terrancev1_tryout <- sds_polyregsample_tryout
sds_terrancev1_tryout$syn <- terrance_v1_sds
sds_terrancev2_tryout <- sds_polyregsample_tryout
sds_terrancev2_tryout$syn <- terrance_v2_sds
#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_terrancev1_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                   B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                   B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                   B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                   C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                   D1 + D2 + D3 + D4 + D5 + 
                                   E2 + E3 + E4 + E7 + E5 + E6,
                                    data = sds_terrancev1_tryout)
summary(lm_terrancev1_model1)
compare_terrancev1_model1 <- compare(lm_terrancev1_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_terrancev1_model1
# Save the polyregsample object
saveRDS(lm_polyregsample_model1, "./SyntheticData/Yue/lm_polyregsample_model1.rds")
saveRDS(compare_polyregsample_model1, "./SyntheticData/Yue/compare_polyregsample_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_terrancev1_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                   B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                   B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                   B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                   C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                   E2 + E3 + E4 + E7 + E5 + E6,
                                    data = sds_terrancev1_tryout)
summary(lm_terrancev1_model2)
compare_terrancev1_model2 <- compare(lm_terrancev1_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_terrancev1_model2
# Save the polyregsample object
saveRDS(lm_polyregsample_model2, "./SyntheticData/Yue/lm_polyregsample_model2.rds")
saveRDS(compare_polyregsample_model2, "./SyntheticData/Yue/compare_polyregsample_model2.rds")

rep_polyregsample <- replicated.uniques(sds_polyregsample_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_polyregsample, "./SyntheticData/Yue/rep_polyregsample.rds")

# ##########################################################
# ------------------ terrancev2 ----------------------------
# ##########################################################
#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_terrancev2_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                   B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                   B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                   B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                   C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                   D1 + D2 + D3 + D4 + D5 + 
                                   E2 + E3 + E4 + E7 + E5 + E6,
                                 data = sds_terrancev2_tryout)
summary(lm_terrancev2_model1)
compare_terrancev2_model1 <- compare(lm_terrancev2_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_terrancev2_model1
# Save the polyregsample object
saveRDS(lm_polyregsample_model1, "./SyntheticData/Yue/lm_polyregsample_model1.rds")
saveRDS(compare_polyregsample_model1, "./SyntheticData/Yue/compare_polyregsample_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_terrancev2_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                   B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                   B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                   B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                   C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                   E2 + E3 + E4 + E7 + E5 + E6,
                                 data = sds_terrancev2_tryout)
summary(lm_terrancev2_model2)
compare_terrancev2_model2 <- compare(lm_terrancev2_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_terrancev2_model2
# Save the polyregsample object
saveRDS(lm_polyregsample_model2, "./SyntheticData/Yue/lm_polyregsample_model2.rds")
saveRDS(compare_polyregsample_model2, "./SyntheticData/Yue/compare_polyregsample_model2.rds")

rep_polyregsample <- replicated.uniques(sds_polyregsample_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_polyregsample, "./SyntheticData/Yue/rep_polyregsample.rds")
# ##########################################################
# ------------------ polyregsample ----------------------------
# ##########################################################
sds_polyregsample_tryout <- load("./SyntheticData/Yue/synobject_polyreg_sample_syn.RData")
sds_polyregsample_tryout <- object
# encode variables as integers for ods and polyregsample
bindori_dataset_threshold_chr[,2:54] <- sapply(bindori_dataset_threshold_chr[,2:54],as.integer)
sds_polyregsample_tryout$syn[[1]][,2:54] <- sapply(sds_polyregsample_tryout$syn[[1]][,2:54],as.integer)

sds_polyregsample_tryout$syn <- sds_polyregsample_tryout$syn[[1]]

u1 <- utility.gen(sds_polyregsample_tryout, bindori_dataset_threshold_chr, print.stats = c("pMSE"))
#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_polyregsample_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                      B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                      B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                      B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                      C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                      D1 + D2 + D3 + D4 + D5 + 
                                      E2 + E3 + E4 + E7 + E5 + E6,
                                 data = sds_polyregsample_tryout)
summary(lm_polyregsample_model1)
compare_polyregsample_model1 <- compare(lm_polyregsample_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_polyregsample_model1
18.# Save the polyregsample object
saveRDS(lm_polyregsample_model1, "./SyntheticData/Yue/lm_polyregsample_model1.rds")
saveRDS(compare_polyregsample_model1, "./SyntheticData/Yue/compare_polyregsample_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_polyregsample_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                      B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                      B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                      B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                      C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                      E2 + E3 + E4 + E7 + E5 + E6,
                                 data = sds_polyregsample_tryout)
summary(lm_polyregsample_model2)
compare_polyregsample_model2 <- compare(lm_polyregsample_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_polyregsample_model2
# Save the polyregsample object
saveRDS(lm_polyregsample_model2, "./SyntheticData/Yue/lm_polyregsample_model2.rds")
saveRDS(compare_polyregsample_model2, "./SyntheticData/Yue/compare_polyregsample_model2.rds")

rep_polyregsample <- replicated.uniques(sds_polyregsample_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_polyregsample, "./SyntheticData/Yue/rep_polyregsample.rds")

# ##########################################################
# ------------------ polyregnorm ----------------------------
# ##########################################################

# load the syn object for polyreg norm
sds_polyregnorm_tryout <- load("./SyntheticData/Yue/synobject_polyreg_norm_syn.RData")
sds_polyregnorm_tryout <- object
# encode as integer
sds_polyregnorm_tryout$syn[[1]][,2:54] <- sapply(sds_polyregnorm_tryout$syn[[1]][,2:54],as.integer)
sds_polyregnorm_tryout$syn <- sds_polyregnorm_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_polyregnorm_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                    B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                    B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                    B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                    C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                    D1 + D2 + D3 + D4 + D5 + 
                                    E2 + E3 + E4 + E7 + E5 + E6,
                               data = sds_polyregnorm_tryout)
summary(lm_polyregnorm_model1)
compare_polyregnorm_model1 <- compare(lm_polyregnorm_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_polyregnorm_model1
# Save the polyregnorm object
saveRDS(lm_polyregnorm_model1, "./SyntheticData/Yue/lm_polyregnorm_model1.rds")
saveRDS(compare_polyregnorm_model1, "./SyntheticData/Yue/compare_polyregnorm_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_polyregnorm_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                    B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                    B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                    B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                    C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                    E2 + E3 + E4 + E7 + E5 + E6,
                               data = sds_polyregnorm_tryout)
summary(lm_polyregnorm_model2)
compare_polyregnorm_model2 <- compare(lm_polyregnorm_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_polyregnorm_model2
# Save the polyregnorm object
saveRDS(lm_polyregnorm_model2, "./SyntheticData/Yue/lm_polyregnorm_model2.rds")
saveRDS(compare_polyregnorm_model2, "./SyntheticData/Yue/compare_polyregnorm_model2.rds")

rep_polyregnorm <- replicated.uniques(sds_polyregnorm_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_polyregnorm, "./SyntheticData/Yue/rep_polyregnorm.rds")

# ##########################################################
# ------------------ polyregnormrank ----------------------------
# ##########################################################
# sds_polyregnormrank_tryout <- syn_polyreg_experiment(para_weight_list, index=3, bindori_dataset_threshold_chr, arg_method, arg_col)
# load the syn object for polyreg normrank
sds_polyregnormrank_tryout <- load("./SyntheticData/Yue/synobject_polyreg_normrank_syn.RData")
sds_polyregnormrank_tryout <- object
# encode as integer
sds_polyregnormrank_tryout$syn[[1]][,2:54] <- sapply(sds_polyregnormrank_tryout$syn[[1]][,2:54],as.integer)
sds_polyregnormrank_tryout$syn <- sds_polyregnormrank_tryout$syn[[1]]

#*****************************************************
# Model 1: contact tracing app -- F2_1
# formula prepared
lm_polyregnormrank_model1 <- lm.synds(F2_1 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                        B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                        B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + #
                                        B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                        C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                        D1 + D2 + D3 + D4 + D5 + 
                                        E2 + E3 + E4 + E7 + E5 + E6,
                                   data = sds_polyregnormrank_tryout)
summary(lm_polyregnormrank_model1)
compare_polyregnormrank_model1 <- compare(lm_polyregnormrank_model1, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_polyregnormrank_model1
# Save the polyregnormrank object
saveRDS(lm_polyregnormrank_model1, "./SyntheticData/Yue/lm_polyregnormrank_model1.rds")
saveRDS(compare_polyregnormrank_model1, "./SyntheticData/Yue/compare_polyregnormrank_model1.rds")

#*****************************************************
# Model 2: covid positive -- B8
lm_polyregnormrank_model2 <- lm.synds(B8 ~ B1_1 + B1_2 + B1_3 + B1_4 + B1_5 + B1_6 + 
                                        B1_7 + B1_8 + B1_9 + B1_10 + B1_11 + B1_12 + B1_13 +
                                        B2 + B3 + B4 + B5 + B6 + B7 + B9 + B10 + B11 + #
                                        B12_1 + B12_2 + B12_3 + B12_4 + B12_5 + B12_6 +
                                        C1_m + C2 + C3 + C5 + C6 + C7 + C8 + 
                                        E2 + E3 + E4 + E7 + E5 + E6,
                                   data = sds_polyregnormrank_tryout)
summary(lm_polyregnormrank_model2)
compare_polyregnormrank_model2 <- compare(lm_polyregnormrank_model2, bindori_dataset_threshold_chr, plot.intercept = TRUE, plot = "coef")
compare_polyregnormrank_model2
# Save the polyregnormrank object
saveRDS(lm_polyregnormrank_model2, "./SyntheticData/Yue/lm_polyregnormrank_model2.rds")
saveRDS(compare_polyregnormrank_model2, "./SyntheticData/Yue/compare_polyregnormrank_model2.rds")

rep_polyregnormrank <- replicated.uniques(sds_polyregnormrank_tryout,bindori_dataset_threshold_chr)
saveRDS(rep_polyregnormrank, "./SyntheticData/Yue/rep_polyregnormrank.rds")

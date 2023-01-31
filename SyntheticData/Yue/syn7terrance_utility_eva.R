################################################################################
# -------------------------- Utility Evaluation -------------------------------#
# This script is meant for the utility of evaluation based on the synthetic    #
# datasets compared to the original dataset                                    #

# firstly, we load the required libraries
library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(synthpop)
library(ggplot2)
library(dbplyr)
library(here)

# set the working directory
# wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
wd = "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# then we load the required preprocessed datasets
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)
# load the cart synthetic dataset for referrance
# now we start with the cart group by looping the saved .rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder <- "./SyntheticData/Yue/syn1_cart"
files <- list.files(folder, pattern = ".rda$")

syn_cart_models <- Map(rda2list, file.path(folder, files))
names(syn_cart_models) <- tools::file_path_sans_ext(files)

# we dataframe the lists
cart_sample_sds <- data.frame(syn_cart_models$cart_sample_syn)
cart_norm_sds <- data.frame(syn_cart_models$cart_norm_syn)
cart_normrank_sds <- data.frame(syn_cart_models$cart_normrank_syn)

# delete the prefix in variable naming
names(cart_sample_sds) <- sub('^syn.', '', names(cart_sample_sds))
names(cart_norm_sds) <- sub('^syn.', '', names(cart_norm_sds))
names(cart_normrank_sds) <- sub('^syn.', '', names(cart_normrank_sds))

str(cart_sample_sds)
str(cart_norm_sds)
str(cart_normrank_sds)
table(cart_norm_sds$E6)
table(bindori_dataset_threshold_chr$E6)


ncol(cart_norm_sds)==54  # there are 54 variables in total

# # for cart_sample, we try change all the character type back to factor except "weight"
# col_names <- names(cart_sample_sds)[2:59]
# cart_sample_sds[col_names] <- lapply(cart_sample_sds[col_names], factor)
# str(cart_sample_sds)

# for cart_sample, cart_norm and normrank, we will have to reverse the integer back to the factor

# additionally, for cart sample, norm and norm, we reverse the integer back to interval
table(cart_sample_sds$B2, cart_norm_sds$B2) # B2 is ok
cart_sample_sds$B2[cart_sample_sds$B2 == "1"] <- "-1"
cart_sample_sds$B2[cart_sample_sds$B2 == "2"] <- "-99"
cart_sample_sds$B2[cart_sample_sds$B2 == "3"] <- "[0, 1)"
cart_sample_sds$B2[cart_sample_sds$B2 == "4"] <- "[1, 3)"
cart_norm_sds$B2[cart_norm_sds$B2 == "1"] <- "-1"
cart_norm_sds$B2[cart_norm_sds$B2 == "2"] <- "-99"
cart_norm_sds$B2[cart_norm_sds$B2 == "3"] <- "[0, 1)"
cart_norm_sds$B2[cart_norm_sds$B2 == "4"] <- "[1, 3)"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "1"] <- "-1"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "2"] <- "-99"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "3"] <- "[0, 1)"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "4"] <- "[1, 3)"

table(cart_sample_sds$B4, cart_norm_sds$B4) # B4 is ok
cart_sample_sds$B4[cart_sample_sds$B4 == "1"] <- "-99"
cart_sample_sds$B4[cart_sample_sds$B4 == "2"] <- "[0, 1)"
cart_sample_sds$B4[cart_sample_sds$B4 == "3"] <- "[1, 5)"
cart_norm_sds$B4[cart_norm_sds$B4 == "1"] <- "-99"
cart_norm_sds$B4[cart_norm_sds$B4 == "2"] <- "[0, 1)"
cart_norm_sds$B4[cart_norm_sds$B4 == "3"] <- "[1, 5)"
cart_normrank_sds$B4[cart_normrank_sds$B4 == "1"] <- "-99"
cart_normrank_sds$B4[cart_normrank_sds$B4 == "2"] <- "[0, 1)"
cart_normrank_sds$B4[cart_normrank_sds$B4 == "3"] <- "[1, 5)"

table(cart_sample_sds$E5, cart_norm_sds$E5) # E5 is ok
cart_sample_sds$E5[cart_sample_sds$E5 == "1"] <- "-99"
cart_sample_sds$E5[cart_sample_sds$E5 == "2"] <- "[0, 1)"
cart_sample_sds$E5[cart_sample_sds$E5 == "3"] <- "[1, 2)"
cart_norm_sds$E5[cart_norm_sds$E5 == "1"] <- "-99"
cart_norm_sds$E5[cart_norm_sds$E5 == "2"] <- "[0, 1)"
cart_norm_sds$E5[cart_norm_sds$E5 == "3"] <- "[1, 2)"
cart_normrank_sds$E5[cart_normrank_sds$E5 == "1"] <- "-99"
cart_normrank_sds$E5[cart_normrank_sds$E5 == "2"] <- "[0, 1)"
cart_normrank_sds$E5[cart_normrank_sds$E5 == "3"] <- "[1, 2)"

table(cart_sample_sds$E6, cart_norm_sds$E6) # exclude E6, 54-1
cart_sample_sds$E6[cart_sample_sds$E6 == "1"] <- "-99"
cart_sample_sds$E6[cart_sample_sds$E6 == "2"] <- "[0, 9)"
cart_norm_sds$E6[cart_norm_sds$E6 == "1"] <- "-99"
cart_norm_sds$E6[cart_norm_sds$E6 == "2"] <- "[0, 9)"
cart_normrank_sds$E6[cart_normrank_sds$E6 == "1"] <- "-99"
cart_normrank_sds$E6[cart_normrank_sds$E6 == "2"] <- "[0, 9)"

# change var "B2", "B4", "E5", "E6" to factor type
cols_factor <- c("B2", "B4", "E5", "E6")
cart_sample_sds[cols_factor] <- lapply(cart_sample_sds[cols_factor], factor)
cart_norm_sds[cols_factor] <- lapply(cart_norm_sds[cols_factor], factor)
cart_normrank_sds[cols_factor] <- lapply(cart_normrank_sds[cols_factor], factor)

# filter the original dataset with 54 variables
ods_select_vars <- bindori_dataset_threshold_chr[colnames(cart_sample_sds)]
str(ods_select_vars)
#----------------------------------------------------------------#
#                           Exp-7
#--------------------- Terrance's sds ----------------------------#
#----------------------------------------------------------------#
sds_filepath_version1 <- "./SyntheticData/Terrance/version_1/syn_k2_2020-08-02_2020-08-08-version1.csv"
sds_filepath_version2 <- "./SyntheticData/Terrance/version_2/syn_k2_2020-08-02_2020-08-08-version2.csv"

sds_terrance_v1 <- read.csv(file = sds_filepath_version1)
sds_terrance_v2 <- read.csv(file = sds_filepath_version2)

# change from sample_weight to weight
colnames(sds_terrance_v1)[colnames(sds_terrance_v1)=="sample_weight"] <- "weight"
colnames(sds_terrance_v2)[colnames(sds_terrance_v2)=="sample_weight"] <- "weight"
sds_terrance_v1 <- sds_terrance_v1[names(cart_sample_sds)]
sds_terrance_v2 <- sds_terrance_v2[names(cart_sample_sds)]
str(sds_terrance_v1)
# change to factor type from var 2 to var 54
sds_terrance_v1[2:54] <- lapply(sds_terrance_v1[2:54], factor)
sds_terrance_v2[2:54] <- lapply(sds_terrance_v2[2:54], factor)
str(sds_terrance_v1)
str(sds_terrance_v2)
str(ods_select_vars)
# align the levels of C3, B2, B4, E5, E6
levels(ods_select_vars$C3) <- levels(sds_terrance_v1$C3)
levels(ods_select_vars$B2) <- levels(sds_terrance_v1$B2)
levels(ods_select_vars$B4) <- levels(sds_terrance_v1$B4)
levels(ods_select_vars$E5) <- levels(sds_terrance_v1$E5)
levels(ods_select_vars$E6) <- levels(sds_terrance_v1$E6)

#******************* for terrance sds v1
compare_plots_terrance_sds_v1 <- c()

for (i in 1:54) {
  cat(colnames(ods_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_terrance_sds_v1[[i]] <- compare(object = data.frame(Pdata = sds_terrance_v1[i]),
                                           data = data.frame(Pdata = ods_select_vars[i]),
                                           vars = c(colnames(ods_select_vars[i])), cont.na = NULL,
                                           msel = NULL, stat = "percents", breaks = 10,
                                           nrow = 2, ncol = 2, rel.size.x = 1,
                                           utility.stats = c("pMSE", "S_pMSE"),
                                           cols = c("#1A3C5A","#4187BF"),
                                           plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn7_terrance/oneway_compare_terrance_version1.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_terrance_sds_v1[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_terrance_sds_v1 <- c()
SpMSE_list_terrance_sds_v1 <- c()
for (i in 1:54) {
  pMSE_list_terrance_sds_v1 <- append(pMSE_list_terrance_sds_v1, compare_plots_terrance_sds_v1[[i]]$tab.utility[1])
  SpMSE_list_terrance_sds_v1 <- append(SpMSE_list_terrance_sds_v1, compare_plots_terrance_sds_v1[[i]]$tab.utility[2])
}

#create data frame
df_utility_terrance_sds_v1 <- data.frame(vars_list=colnames(sds_terrance_v1),
                                         pMSE=pMSE_list_terrance_sds_v1,
                                         S_pMSE=SpMSE_list_terrance_sds_v1)

write_utility_terrance_sds_v1 <- "./SyntheticData/Yue/syn7_terrance/oneway_utility_terrance_version1.csv"
write.csv(df_utility_terrance_sds_v1, write_utility_terrance_sds_v1, row.names=FALSE)

vars2show_terrance_sds_v1 <- df_utility_terrance_sds_v1[df_utility_terrance_sds_v1[, "S_pMSE"]<10, ][1]

nrow(vars2show_terrance_sds_v1)  # there are 19 in total for terrance_sds_v1


#******************* for terrance sds v2
compare_plots_terrance_sds_v2 <- c()

for (i in 1:54) {
  cat(colnames(ods_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_terrance_sds_v2[[i]] <- compare(object = data.frame(Pdata = sds_terrance_v2[i]),
                                                data = data.frame(Pdata = ods_select_vars[i]),
                                                vars = c(colnames(ods_select_vars[i])), cont.na = NULL,
                                                msel = NULL, stat = "percents", breaks = 10,
                                                nrow = 2, ncol = 2, rel.size.x = 1,
                                                utility.stats = c("pMSE", "S_pMSE"),
                                                cols = c("#1A3C5A","#4187BF"),
                                                plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn7_terrance/oneway_compare_terrance_version2.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_terrance_sds_v2[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_terrance_sds_v2 <- c()
SpMSE_list_terrance_sds_v2 <- c()
for (i in 1:54) {
  pMSE_list_terrance_sds_v2 <- append(pMSE_list_terrance_sds_v2, compare_plots_terrance_sds_v2[[i]]$tab.utility[1])
  SpMSE_list_terrance_sds_v2 <- append(SpMSE_list_terrance_sds_v2, compare_plots_terrance_sds_v2[[i]]$tab.utility[2])
}

#create data frame
df_utility_terrance_sds_v2 <- data.frame(vars_list=colnames(sds_terrance_v2),
                                         pMSE=pMSE_list_terrance_sds_v2,
                                         S_pMSE=SpMSE_list_terrance_sds_v2)

write_utility_terrance_sds_v2 <- "./SyntheticData/Yue/syn7_terrance/oneway_utility_terrance_version2.csv"
write.csv(df_utility_terrance_sds_v2, write_utility_terrance_sds_v2, row.names=FALSE)

vars2show_terrance_sds_v2 <- df_utility_terrance_sds_v2[df_utility_terrance_sds_v2[, "S_pMSE"]<10, ][1]

nrow(vars2show_terrance_sds_v2)  # there are 2 in total for terrance_sds_v2

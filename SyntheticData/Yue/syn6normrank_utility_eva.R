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
wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# then we load the required preprocessed datasets
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)


#----------------------------------------------------------------#
#                           Exp-6
#--------------------- method = normrank ----------------------------#
#----------------------------------------------------------------#

# now we start with the cart group by looping the saved .rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder <- "./SyntheticData/Yue/syn6_normrank"
files <- list.files(folder, pattern = ".rda$")

syn_normrank_models <- Map(rda2list, file.path(folder, files))
names(syn_normrank_models) <- tools::file_path_sans_ext(files)

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

# for normrank_sample, normrank_norm and normrank, we will have to reverse the integer back to the factor

# additionally, for cart sample, norm and norm, we reverse the integer back to factors
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

# is it necessary to add more levels to the dataset
table(bindori_dataset_threshold_chr$C8, normrank_sample_sds$C8)
levels(normrank_sample_sds$C8) <- c(levels(bindori_dataset_threshold_chr$C8))
levels(normrank_norm_sds$C8) <- c(levels(bindori_dataset_threshold_chr$C8))
levels(normrank_normrank_sds$C8) <- c(levels(bindori_dataset_threshold_chr$C8))

table(bindori_dataset_threshold_chr$E6, normrank_norm_sds$E6)
levels(bindori_dataset_threshold_chr$E6) <- c(levels(normrank_norm_sds$E6))
levels(normrank_sample_sds$E6) <- c(levels(normrank_norm_sds$E6))
levels(normrank_normrank_sds$E6) <- c(levels(normrank_norm_sds$E6))
#------------------Evaluating the utility of the norm_syn sds------------------

#=========(1). one-way marginals using compare()
# try with 54-1 variables firstly (exclude C8), we subset the original ods first
ncol(normrank_sample_sds)
bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(names(normrank_sample_sds))
# cuz the compare function cannot tackle with factor type variables, we delete them for evaluation
bindori_select_vars <- bindori_dataset_threshold_chr
normranksample_select_vars <- normrank_sample_sds
normranknorm_select_vars <- normrank_norm_sds
normranknormrank_select_vars <- normrank_normrank_sds

ncol(normranknorm_select_vars)

#******************* for norm sample
compare_plots_normranksample<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_normranksample[[i]] <- compare(object = data.frame(Pdata = normranksample_select_vars[i]),
                                data = data.frame(Pdata = bindori_select_vars[i]),
                                vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                msel = NULL, stat = "percents", breaks = 10,
                                nrow = 2, ncol = 2, rel.size.x = 1,
                                utility.stats = c("pMSE", "S_pMSE"),
                                cols = c("#1A3C5A","#4187BF"),
                                plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn6_normrank/oneway_compare_normranksample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_normranksample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_normranksample <- c()
SpMSE_list_normranksample <- c()
for (i in 1:54) {
  pMSE_list_normranksample <- append(pMSE_list_normranksample, compare_plots_normranksample[[i]]$tab.utility[1])
  SpMSE_list_normranksample <- append(SpMSE_list_normranksample, compare_plots_normranksample[[i]]$tab.utility[2])
}

#create data frame
df_utility_normranksample <- data.frame(vars_list=colnames(normranksample_select_vars),
                         pMSE=pMSE_list_normranksample,
                         S_pMSE=SpMSE_list_normranksample)

write_utility_normranksample <- "./SyntheticData/Yue/syn6_normrank/oneway_utility_normranksample.csv"
write.csv(df_utility_normranksample, write_utility_normranksample, row.names=FALSE)

vars2show_normranksample <- df_utility_normranksample[df_utility_normranksample[, "S_pMSE"]<10, ][1]

nrow(vars2show_normranksample)  # there are 27 in total for normranksample


#******************* for normrank norm
compare_plots_normranknorm<- c()
table(normranknorm_select_vars$E6, bindori_select_vars$E6) # there are 0 and 3 for norm please exclude them, 52 in total
for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_normranknorm[[i]] <- compare(object = data.frame(Pdata = normranknorm_select_vars[i]),
                                           data = data.frame(Pdata = bindori_select_vars[i]),
                                           vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                           msel = NULL, stat = "percents", breaks = 10,
                                           nrow = 2, ncol = 2, rel.size.x = 1,
                                           utility.stats = c("pMSE", "S_pMSE"),
                                           cols = c("#1A3C5A","#4187BF"),
                                           plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn6_normrank/oneway_compare_normranknorm.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_normranknorm[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_normranknorm <- c()
SpMSE_list_normranknorm <- c()
for (i in 1:54) {
  pMSE_list_normranknorm <- append(pMSE_list_normranknorm, compare_plots_normranknorm[[i]]$tab.utility[1])
  SpMSE_list_normranknorm <- append(SpMSE_list_normranknorm, compare_plots_normranknorm[[i]]$tab.utility[2])
}

#create data frame
df_utility_normranknorm <- data.frame(vars_list=colnames(normranknorm_select_vars[1:54]),
                                    pMSE=pMSE_list_normranknorm,
                                    S_pMSE=SpMSE_list_normranknorm)

write_utility_normranknorm <- "./SyntheticData/Yue/syn6_normrank/oneway_utility_normranknorm.csv"
write.csv(df_utility_normranknorm, write_utility_normranknorm, row.names=FALSE)

vars2show_normranknorm <- df_utility_normranknorm[df_utility_normranknorm[, "S_pMSE"]<10, ][1]

nrow(vars2show_normranknorm)  # there are 27 in total for normranknorm


#******************* for cart normrank
compare_plots_normranknormrank<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_normranknormrank[[i]] <- compare(object = data.frame(Pdata = normranknormrank_select_vars[i]),
                                         data = data.frame(Pdata = bindori_select_vars[i]),
                                         vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                         msel = NULL, stat = "percents", breaks = 10,
                                         nrow = 2, ncol = 2, rel.size.x = 1,
                                         utility.stats = c("pMSE", "S_pMSE"),
                                         cols = c("#1A3C5A","#4187BF"),
                                         plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn6_normrank/oneway_compare_normranknormrank.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_normranknormrank[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_normranknormrank <- c()
SpMSE_list_normranknormrank <- c()
for (i in 1:54) {
  pMSE_list_normranknormrank <- append(pMSE_list_normranknormrank, compare_plots_normranknormrank[[i]]$tab.utility[1])
  SpMSE_list_normranknormrank <- append(SpMSE_list_normranknormrank, compare_plots_normranknormrank[[i]]$tab.utility[2])
}

#create data frame
df_utility_normranknormrank <- data.frame(vars_list=colnames(normranknormrank_select_vars),
                                  pMSE=pMSE_list_normranknormrank,
                                  S_pMSE=SpMSE_list_normranknormrank)

write_utility_normranknormrank <- "./SyntheticData/Yue/syn6_normrank/oneway_utility_normranknormrank.csv"
write.csv(df_utility_normranknormrank, write_utility_normranknormrank, row.names=FALSE)

vars2show_normranknormrank <- df_utility_normranknormrank[df_utility_normranknormrank[, "S_pMSE"]<10, ][1]

nrow(vars2show_normranknormrank)  # there are 26 in total for normranknormrank


# -----------------------------------------------------------------------------
################################# Machine Learning #############################
# -----------------------------------------------------------------------------
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
library(glmnet)

set.seed(2023) # make sure the results are reproducible
#*****************************************************
# Model 1: contact tracing app -- F2_1

# step1: prepare the datasets
vars_inc_m1 <- c("D1","D2","D3","D4","D5","D7","D8","D9","E2","E3","E4","E7","E5","E6","F2_1")
ods_m1 <- bindori_dataset_threshold_chr[vars_inc_m1]
table(ods_m1$F2_1)

sds_normranksample_m1 <- normrank_sample_sds[vars_inc_m1]
table(sds_normranksample_m1$F2_1)

sds_normranknorm_m1 <- normrank_norm_sds[vars_inc_m1]
table(sds_normranknorm_m1$F2_1)

sds_normranknormrank_m1 <- normrank_normrank_sds[vars_inc_m1]
table(sds_normranknormrank_m1$F2_1)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m1 <- TaskClassif$new(id="tsk_ods_m1",
                              backend=ods_m1, target="F2_1")

tsk_normranksample_m1 <- TaskClassif$new(id="tsk_normranksample_m1", 
                                     backend=sds_normranksample_m1, target="F2_1")

tsk_normranknorm_m1 <- TaskClassif$new(id="tsk_normranknorm_m1", 
                                   backend=sds_normranknorm_m1, target="F2_1")

tsk_normranknormrank_m1 <- TaskClassif$new(id="tsk_normranknormrank_m1",
                                       backend=sds_normranknormrank_m1, target="F2_1")

tasks_list_m1 <- list(tsk_ods_m1, tsk_normranksample_m1, tsk_normranknorm_m1, tsk_normranknormrank_m1)

# step3: prepare the required learners
learners_list_model1 <- lrns(c("classif.naive_bayes", "classif.lda"))  # classif.lda

# step4: benchmark the task and learners with cross-validation
# benchmark_grid is the design
bm_model1 <- benchmark(benchmark_grid(tasks = tasks_list_m1,
                                      learners = learners_list_model1, resamplings = rsmp("cv", folds = 2)),
                       store_models = TRUE)

# step5: validate the accuracy of the model
#****** Measure to compare true observed 
#****** labels with predicted labels in 
#****** multiclass classification tasks.
bm_model1$aggregate(msr("classif.acc"))

# step6: extract the coefficients of the trained instances
coef_info_m1 <- mlr3misc::map(as.data.table(bm_model1)$learner, "model")

# step7: save bm_model as rds
saveRDS(bm_model1, './SyntheticData/Yue/syn6_normrank/bm_normrank_model1.rds')
saveRDS(coef_info_m1, './SyntheticData/Yue/syn6_normrank/coef_normrank_model1.rds')

#*****************************************************
# Model 2: covid positive -- B8 (multiclass)

# step1: prepare the datasets
vars_inc_m2 <- c("E2","E3","E4","E7","E5","E6","C1_m","C2","C3","C5","C6","C7","C8","B8")
ods_m2 <- bindori_dataset_threshold_chr[vars_inc_m2]
ods_m2$B8 = factor(ods_m2$B8)
table(ods_m2$B8)

sds_normranksample_m2 <- normrank_sample_sds[vars_inc_m2]
sds_normranksample_m2$B8 = factor(sds_normranksample_m2$B8)

sds_normranknorm_m2 <- normrank_norm_sds[vars_inc_m2]
sds_normranknorm_m2$B8 = factor(sds_normranknorm_m2$B8)

sds_normranknormrank_m2 <- normrank_normrank_sds[vars_inc_m2]
sds_normranknormrank_m2$B8 = factor(sds_normranknormrank_m2$B8)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m2 <- TaskClassif$new(id="tsk_ods_m2",
                              backend=ods_m2, target="B8")

tsk_normranksample_m2 <- TaskClassif$new(id="tsk_normranksample_m2", 
                                     backend=sds_normranksample_m2, target="B8")

tsk_normranknorm_m2 <- TaskClassif$new(id="tsk_normranknorm_m2", 
                                   backend=sds_normranknorm_m2, target="B8")

tsk_normranknormrank_m2 <- TaskClassif$new(id="tsk_normranknormrank_m2",
                                       backend=sds_normranknormrank_m1, target="B8")

tasks_list_m2 <- list(tsk_ods_m2, tsk_normranksample_m2, tsk_normranknorm_m2, tsk_normranknormrank_m2)

# step3: prepare the required learners
learners_list_model2 <- lrns(c("classif.naive_bayes", "classif.lda"))

# step4: benchmark the task and learners with cross-validation
# benchmark_grid is the design
bm_model2 <- benchmark(benchmark_grid(tasks = tasks_list_m2,
                                      learners = learners_list_model2,
                                      resamplings = rsmp("cv", folds = 2)),
                       store_models = TRUE)

# step5: validate the accuracy of the model
#****** Measure to compare true observed 
#****** labels with predicted labels in 
#****** multiclass classification tasks.
bm_model2$aggregate(msr("classif.acc"))

# step6: extract the coefficients of the trained instances
coef_info_m2 <- mlr3misc::map(as.data.table(bm_model2)$learner, "model")

# step7: save bm_model as rds
saveRDS(bm_model2, './SyntheticData/Yue/syn5_norm/bm_norm_model2.rds')
saveRDS(coef_info_m2, './SyntheticData/Yue/syn5_norm/coef_norm_model2.rds')
# score_multinom_m1_ods <- sum(data.frame(bm_model1$score(msr("classif.acc"))[learner_id == 'classif.multinom', ][task_id == "tsk_ods_m1", ])["classif.acc"])/3
# score_multinom_m1_ods <- sum(data.frame(bm_model1$score(msr("classif.acc"))[learner_id == 'classif.multinom', ][task_id == "tsk_ods_m1", ])["classif.acc"])/3



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

# set the working directory
wd <- "/Users/roxy/Desktop/Master-Thesis-SyntheticDataGeneration"
setwd(wd)

# then we load the required preprocessed datasets
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)
bindori_dataset_threshold_chr$F2_2

#----------------------------------------------------------------#
#                           Exp-5
#--------------------- method = norm ----------------------------#
#----------------------------------------------------------------#

# now we start with the cart group by looping the saved .rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder <- "./SyntheticData/Yue/syn5_norm"
files <- list.files(folder, pattern = ".rda$")

syn_norm_models <- Map(rda2list, file.path(folder, files))
names(syn_norm_models) <- tools::file_path_sans_ext(files)

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

# for norm_sample, norm_norm and normrank, we will have to reverse the integer back to the factor

# additionally, for cart sample, norm and norm, we reverse the integer back to factors
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

# is it necessary to add more levels to the dataset
table(bindori_dataset_threshold_chr$C1_m, norm_norm_sds$C1_m)
levels(bindori_dataset_threshold_chr$C1_m) <- c(levels(norm_norm_sds$C1_m))
levels(norm_sample_sds$C1_m) <- c(levels(norm_norm_sds$C1_m))
levels(norm_normrank_sds$C1_m) <- c(levels(norm_norm_sds$C1_m))

table(bindori_dataset_threshold_chr$C3, norm_norm_sds$C3)
levels(bindori_dataset_threshold_chr$C3) <- c(levels(norm_norm_sds$C3))
levels(norm_sample_sds$C3) <- c(levels(norm_norm_sds$C3))
levels(norm_normrank_sds$C3) <- c(levels(norm_norm_sds$C3))

table(bindori_dataset_threshold_chr$C5, norm_norm_sds$C5)
levels(bindori_dataset_threshold_chr$C5) <- c(levels(norm_sample_sds$C5))
levels(norm_norm_sds$C5) <- c(levels(norm_sample_sds$C5))
levels(norm_normrank_sds$C5) <- c(levels(norm_sample_sds$C5))

table(bindori_dataset_threshold_chr$C8, norm_normrank_sds$C8)
length(levels(norm_normrank_sds$C8))
levels(bindori_dataset_threshold_chr$C8) <- c(levels(norm_normrank_sds$C8))
levels(norm_sample_sds$C8) <- c(levels(norm_normrank_sds$C8))
levels(norm_norm_sds$C8) <- c(levels(norm_normrank_sds$C8))

table(bindori_dataset_threshold_chr$D1, norm_norm_sds$D1)
length(levels(norm_normrank_sds$D1))
levels(bindori_dataset_threshold_chr$D1) <- c(levels(norm_normrank_sds$D1))
levels(norm_sample_sds$D1) <- c(levels(norm_normrank_sds$D1))
levels(norm_norm_sds$D1) <- c(levels(norm_normrank_sds$D1))

table(bindori_dataset_threshold_chr$D3, norm_norm_sds$D3)
length(levels(norm_normrank_sds$D3))
levels(bindori_dataset_threshold_chr$D3) <- c(levels(norm_normrank_sds$D3))
levels(norm_sample_sds$D3) <- c(levels(norm_normrank_sds$D3))
levels(norm_norm_sds$D3) <- c(levels(norm_normrank_sds$D3))

table(norm_normrank_sds$D4, norm_norm_sds$D4)
length(levels(norm_norm_sds$D3))
levels(bindori_dataset_threshold_chr$D4) <- c(levels(norm_norm_sds$D4))
levels(norm_sample_sds$D4) <- c(levels(norm_norm_sds$D4))
levels(norm_normrank_sds$D4) <- c(levels(norm_norm_sds$D4))

table(norm_normrank_sds$E3, norm_norm_sds$E3)
length(levels(norm_normrank_sds$D3))
levels(bindori_dataset_threshold_chr$E3) <- c(levels(norm_norm_sds$E3))
levels(norm_sample_sds$E3) <- c(levels(norm_norm_sds$E3))
levels(norm_normrank_sds$E3) <- c(levels(norm_norm_sds$E3))

table(bindori_dataset_threshold_chr$E4, norm_norm_sds$E4)
length(levels(norm_norm_sds$E4))
levels(bindori_dataset_threshold_chr$E4) <- c(levels(norm_normrank_sds$E4))
levels(norm_sample_sds$E4) <- c(levels(norm_normrank_sds$E4))
levels(norm_norm_sds$E4) <- c(levels(norm_normrank_sds$E4))

table(norm_normrank_sds$B4, norm_norm_sds$B4)
length(levels(norm_normrank_sds$B4))
levels(bindori_dataset_threshold_chr$B4) <- c(levels(norm_norm_sds$B4))
levels(norm_sample_sds$B4) <- c(levels(norm_norm_sds$B4))
levels(norm_normrank_sds$B4) <- c(levels(norm_norm_sds$B4))

table(bindori_dataset_threshold_chr$E6, norm_norm_sds$E6)
length(levels(norm_norm_sds$E6))
levels(bindori_dataset_threshold_chr$E6) <- c(levels(norm_norm_sds$E6))
levels(norm_sample_sds$E6) <- c(levels(norm_norm_sds$E6))
levels(norm_normrank_sds$E6) <- c(levels(norm_norm_sds$E6))


#------------------Evaluating the utility of the norm_syn sds------------------

#=========(1). one-way marginals using compare()
# try with 54-4 variables firstly, we subset the original ods first
ncol(norm_sample_sds)
bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(names(norm_sample_sds))
# cuz the compare function cannot tackle with factor type variables, we delete them for evaluation
# bindori_select_vars <- subset(bindori_dataset_threshold_chr, select = -c(C1_m, C3, C5, C8, D1, D3, D4, E3, E4, B4, E6))
# normsample_select_vars <- subset(norm_sample_sds, select = -c(C1_m, C3, C5, C8, D1, D3, D4, E3, E4, B4, E6))
# normnorm_select_vars <- subset(norm_norm_sds, select = -c(C1_m, C3, C5, C8, D1, D3, D4, E3, E4, B4, E6))
# normnormrank_select_vars <- subset(norm_normrank_sds, select = -c(C1_m, C3, C5, C8, D1, D3, D4, E3, E4, B4, E6))

bindori_select_vars <- bindori_dataset_threshold_chr
normsample_select_vars <- norm_sample_sds
normnorm_select_vars <- norm_norm_sds
normnormrank_select_vars <- norm_normrank_sds

ncol(normnormrank_select_vars)

#******************* for norm sample
compare_plots_normsample<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_normsample[[i]] <- compare(object = data.frame(Pdata = normsample_select_vars[i]),
                                data = data.frame(Pdata = bindori_select_vars[i]),
                                vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                msel = NULL, stat = "percents", breaks = 10,
                                nrow = 2, ncol = 2, rel.size.x = 1,
                                utility.stats = c("pMSE", "S_pMSE"),
                                cols = c("#1A3C5A","#4187BF"),
                                plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn5_norm/oneway_compare_normsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_normsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_normsample <- c()
SpMSE_list_normsample <- c()
for (i in 1:54) {
  pMSE_list_normsample <- append(pMSE_list_normsample, compare_plots_normsample[[i]]$tab.utility[1])
  SpMSE_list_normsample <- append(SpMSE_list_normsample, compare_plots_normsample[[i]]$tab.utility[2])
}

#create data frame
df_utility_normsample <- data.frame(vars_list=colnames(normsample_select_vars),
                         pMSE=pMSE_list_normsample,
                         S_pMSE=SpMSE_list_normsample)

write_utility_normsample <- "./SyntheticData/Yue/syn5_norm/oneway_utility_normsample.csv"
write.csv(df_utility_normsample, write_utility_normsample, row.names=FALSE)

vars2show_normsample <- df_utility_normsample[df_utility_normsample[, "S_pMSE"]<10, ][1]

nrow(vars2show_normsample)  # there are 28 in total for normsample


#******************* for norm norm
compare_plots_normnorm<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_normnorm[[i]] <- compare(object = data.frame(Pdata = normnorm_select_vars[i]),
                                           data = data.frame(Pdata = bindori_select_vars[i]),
                                           vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                           msel = NULL, stat = "percents", breaks = 10,
                                           nrow = 2, ncol = 2, rel.size.x = 1,
                                           utility.stats = c("pMSE", "S_pMSE"),
                                           cols = c("#1A3C5A","#4187BF"),
                                           plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn5_norm/oneway_compare_normnorm.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_normnorm[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_normnorm <- c()
SpMSE_list_normnorm <- c()
for (i in 1:54) {
  pMSE_list_normnorm <- append(pMSE_list_normnorm, compare_plots_normnorm[[i]]$tab.utility[1])
  SpMSE_list_normnorm <- append(SpMSE_list_normnorm, compare_plots_normnorm[[i]]$tab.utility[2])
}

#create data frame
df_utility_normnorm <- data.frame(vars_list=colnames(normnorm_select_vars),
                                    pMSE=pMSE_list_normnorm,
                                    S_pMSE=SpMSE_list_normnorm)

write_utility_normnorm <- "./SyntheticData/Yue/syn5_norm/oneway_utility_normnorm.csv"
write.csv(df_utility_normnorm, write_utility_normnorm, row.names=FALSE)

vars2show_normnorm <- df_utility_normnorm[df_utility_normnorm[, "S_pMSE"]<10, ][1]

nrow(vars2show_normnorm)  # there are 25 in total for normnorm


#******************* for norm normrank
compare_plots_normnormrank<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_normnormrank[[i]] <- compare(object = data.frame(Pdata = normnormrank_select_vars[i]),
                                         data = data.frame(Pdata = bindori_select_vars[i]),
                                         vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                         msel = NULL, stat = "percents", breaks = 10,
                                         nrow = 2, ncol = 2, rel.size.x = 1,
                                         utility.stats = c("pMSE", "S_pMSE"),
                                         cols = c("#1A3C5A","#4187BF"),
                                         plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn5_norm/oneway_compare_normnormrank.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_normnormrank[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_normnormrank <- c()
SpMSE_list_normnormrank <- c()
for (i in 1:54) {
  pMSE_list_normnormrank <- append(pMSE_list_normnormrank, compare_plots_normnormrank[[i]]$tab.utility[1])
  SpMSE_list_normnormrank <- append(SpMSE_list_normnormrank, compare_plots_normnormrank[[i]]$tab.utility[2])
}

#create data frame
df_utility_normnormrank <- data.frame(vars_list=colnames(normnormrank_select_vars),
                                  pMSE=pMSE_list_normnormrank,
                                  S_pMSE=SpMSE_list_normnormrank)

write_utility_normnormrank <- "./SyntheticData/Yue/syn5_norm/oneway_utility_normnormrank.csv"
write.csv(df_utility_normnormrank, write_utility_normnormrank, row.names=FALSE)

vars2show_normnormrank <- df_utility_normnormrank[df_utility_normnormrank[, "S_pMSE"]<10, ][1]

nrow(vars2show_normnormrank)  # there are 26 in total for normnormrank


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
library(mlr3extralearners)

library(e1071)
library(MASS)
library(glmnet)
library(kernlab)
library(ranger)

set.seed(2023) # make sure the results are reproducible
#*****************************************************
# Model 1: contact tracing app -- F2_1

# step1: prepare the datasets
vars_inc_m1 <- c("D1","D2","D3","D4","D5","D7","D8","D9","E2","E3","E4","E7","E5","E6","F2_1")
ods_m1 <- bindori_dataset_threshold_chr[vars_inc_m1]
table(ods_m1$F2_1)

sds_normsample_m1 <- norm_sample_sds[vars_inc_m1]
table(sds_normsample_m1$F2_1)

sds_normnorm_m1 <- norm_norm_sds[vars_inc_m1]
table(sds_normnorm_m1$F2_1)

sds_normnormrank_m1 <- norm_normrank_sds[vars_inc_m1]
table(sds_normnormrank_m1$F2_1)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m1 <- TaskClassif$new(id="tsk_ods_m1",
                              backend=ods_m1, target="F2_1")

tsk_normsample_m1 <- TaskClassif$new(id="tsk_normsample_m1", 
                                        backend=sds_normsample_m1, target="F2_1")

tsk_normnorm_m1 <- TaskClassif$new(id="tsk_normnorm_m1", 
                                      backend=sds_normnorm_m1, target="F2_1")

tsk_normnormrank_m1 <- TaskClassif$new(id="tsk_normnormrank_m1",
                                          backend=sds_normnormrank_m1, target="F2_1")

tasks_list_norm <- list(tsk_ods_m1, tsk_normsample_m1, tsk_normnorm_m1, tsk_normnormrank_m1,
                        tsk_ods_m2, tsk_normsample_m2, tsk_normnorm_m2, tsk_normnormrank_m2)

# step3: prepare the required learners
learners_list_norm <- lrns(c("classif.multinom", "classif.ranger"))  # classif.lda excluded


# step4: benchmark the task and learners with cross-validation
# benchmark_grid is the design
bm_models_norm <- benchmark(benchmark_grid(tasks = tasks_list_norm,
                                           learners = learners_list_norm, resamplings = rsmp("holdout", ratio = 0.8)),
                       store_models = TRUE)

# step5: validate the accuracy of the model
#****** Measure to compare true observed 
#****** labels with predicted labels in 
#****** multiclass classification tasks.
bm_models_norm$aggregate(msr("classif.acc"))[learner_id=="classif.ranger",]

# step6: extract the coefficients of the trained instances
mlr3misc::map(as.data.table(bm_models_norm)$learner, "model")

# step7: save bm_model as rds
saveRDS(bm_models_norm, './SyntheticData/Yue/syn5_norm/bm_norm_models.rds')
saveRDS(coef_info_m1, './SyntheticData/Yue/syn5_norm/coef_norm_model1.rds')

#*****************************************************
# Model 2: covid positive -- B8 (multiclass)

# step1: prepare the datasets
vars_inc_m2 <- c("E2","E3","E4","E7","E5","E6","C1_m","C2","C3","C5","C6","C7","C8","B8")
ods_m2 <- bindori_dataset_threshold_chr[vars_inc_m2]
ods_m2$B8 = factor(ods_m2$B8)
table(ods_m2$B8)

sds_normsample_m2 <- norm_sample_sds[vars_inc_m2]
sds_normsample_m2$B8 = factor(sds_normsample_m2$B8)

sds_normnorm_m2 <- norm_norm_sds[vars_inc_m2]
sds_normnorm_m2$B8 = factor(sds_normnorm_m2$B8)

sds_normnormrank_m2 <- norm_normrank_sds[vars_inc_m2]
sds_normnormrank_m2$B8 = factor(sds_normnormrank_m2$B8)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m2 <- TaskClassif$new(id="tsk_ods_m2",
                              backend=ods_m2, target="B8")

tsk_normsample_m2 <- TaskClassif$new(id="tsk_normsample_m2", 
                                        backend=sds_normsample_m2, target="B8")

tsk_normnorm_m2 <- TaskClassif$new(id="tsk_normnorm_m2", 
                                      backend=sds_normnorm_m2, target="B8")

tsk_normnormrank_m2 <- TaskClassif$new(id="tsk_normnormrank_m2",
                                          backend=sds_normnormrank_m2, target="B8")

tasks_list_m2 <- list(tsk_ods_m2, tsk_normsample_m2, tsk_normnorm_m2, tsk_normnormrank_m2)

# step3: prepare the required learners
learners_list_model2 <- lrns(c("classif.naive_bayes"))

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



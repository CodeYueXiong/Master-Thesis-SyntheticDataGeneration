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
wd <- "Y:/MasterThesisRoxy/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# then we load the required preprocessed datasets
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)


#----------------------------------------------------------------#
#--------------------- method = cart ----------------------------#
#----------------------------------------------------------------#

# now we start with the cart group by looping the saved .rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder <- "./SyntheticData/Yue/syn_cart"
files <- list.files(folder, pattern = ".rda$")

syn_cart_models <- Map(rda2list, file.path(folder, files))
names(syn_cart_models) <- tools::file_path_sans_ext(files)

# we dataframe the lists
cart_sample_sds <- data.frame(syn_cart_models$sample_cart_syn)
cart_norm_sds <- data.frame(syn_cart_models$norm_cart_syn)
cart_normrank_sds <- data.frame(syn_cart_models$normrank_cart_syn)

# delete the prefix in variable naming
names(cart_sample_sds) <- sub('^syn.', '', names(cart_sample_sds))
names(cart_norm_sds) <- sub('^syn.', '', names(cart_norm_sds))
names(cart_normrank_sds) <- sub('^syn.', '', names(cart_normrank_sds))

str(cart_sample_sds)
str(cart_norm_sds)
str(cart_normrank_sds)

# for cart_sample
# also, we can probably subset those columns with constant inputs
cols_remove <- c("B13_1", "B13_2", "B13_3", "B13_4",
                 "B13_5", "B13_6", "B13_7",
                 "B14_1", "B14_2", "B14_3", "B14_4", "B14_5",
                 "D6_1", "D6_2", "D6_3", "F3_de")
cart_sample_sds <- cart_sample_sds %>% select(-(cols_remove))
cart_norm_sds <- cart_norm_sds %>% select(-(cols_remove))
cart_normrank_sds <- cart_normrank_sds %>% select(-(cols_remove))

# also for those B1b_x like vars and D10, we try exclude them from the synthesis
cols_rm_bd <- c("B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", "B1b_x5", "B1b_x6", "B1b_x7",
                "B1b_x8", "B1b_x9", "B1b_x10", "B1b_x11","B1b_x12", "B1b_x13", "D10", "D9")
cart_sample_sds <- cart_sample_sds %>% select(-(cols_rm_bd))
cart_norm_sds <- cart_norm_sds %>% select(-(cols_rm_bd))
cart_normrank_sds <- cart_normrank_sds %>% select(-(cols_rm_bd))

ncol(cart_normrank_sds)==59

# for cart_sample, we try change all the character type back to factor except "weight"
col_names <- names(cart_sample_sds)[2:59]
cart_sample_sds[col_names] <- lapply(cart_sample_sds[col_names], factor)
str(cart_sample_sds)

# for cart_norm and normrank, we will have to reverse the integer back to the factor

# additionally, for cart norm and norm, we reverse the integer back to interval
cart_norm_sds$B2[cart_norm_sds$B2 == "1"] <- "-1"
cart_norm_sds$B2[cart_norm_sds$B2 == "2"] <- "-99"
cart_norm_sds$B2[cart_norm_sds$B2 == "3"] <- "[0, 1)"
cart_norm_sds$B2[cart_norm_sds$B2 == "4"] <- "[1, 3)"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "1"] <- "-1"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "2"] <- "-99"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "3"] <- "[0, 1)"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "4"] <- "[1, 3)"

cart_norm_sds$B4[cart_norm_sds$B4 == "1"] <- "-99"
cart_norm_sds$B4[cart_norm_sds$B4 == "2"] <- "[0, 1)"
cart_norm_sds$B4[cart_norm_sds$B4 == "3"] <- "[1, 5)"
cart_normrank_sds$B4[cart_normrank_sds$B4 == "1"] <- "-99"
cart_normrank_sds$B4[cart_normrank_sds$B4 == "2"] <- "[0, 1)"
cart_normrank_sds$B4[cart_normrank_sds$B4 == "3"] <- "[1, 5)"

cart_norm_sds$E5[cart_norm_sds$E5 == "1"] <- "-99"
cart_norm_sds$E5[cart_norm_sds$E5 == "2"] <- "[0, 1)"
cart_norm_sds$E5[cart_norm_sds$E5 == "3"] <- "[1, 2)"
cart_normrank_sds$E5[cart_normrank_sds$E5 == "1"] <- "-99"
cart_normrank_sds$E5[cart_normrank_sds$E5 == "2"] <- "[0, 1)"
cart_normrank_sds$E5[cart_normrank_sds$E5 == "3"] <- "[1, 2)"

cart_norm_sds$E6[cart_norm_sds$E6 == "1"] <- "-99"
cart_norm_sds$E6[cart_norm_sds$E6 == "2"] <- "[0, 9)"
cart_normrank_sds$E6[cart_normrank_sds$E6 == "1"] <- "-99"
cart_normrank_sds$E6[cart_normrank_sds$E6 == "2"] <- "[0, 9)"

cols_factor <- c("B2", "B4", "E5", "E6")
cart_norm_sds[cols_factor] <- lapply(cart_norm_sds[cols_factor], factor)
cart_normrank_sds[cols_factor] <- lapply(cart_normrank_sds[cols_factor], factor)

str(cart_sample_sds)
str(cart_norm_sds)
str(cart_normrank_sds)

#------------------Evaluating the utility of the cart_syn sds------------------

#=========(1). one-way marginals using compare()
# try with 55 variables firstly, we subset the original ods first
ncol(cart_norm_sds)
bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(names(cart_sample_sds))
# cuz the compare function cannot tackle with factor type variables, we delete them for evaluation
bindori_select_vars <- subset(bindori_dataset_threshold_chr, select = -c(B2, B4, E5, E6))
cartsample_select_vars <- subset(cart_sample_sds, select = -c(B2, B4, E5, E6))
cartnorm_select_vars <- subset(cart_norm_sds, select = -c(B2, B4, E5, E6))
cartnormrank_select_vars <- subset(cart_normrank_sds, select = -c(B2, B4, E5, E6))

ncol(cartsample_select_vars)

#******************* for cart sample
compare_plots_cartsample<- c()

for (i in 1:55) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_cartsample[[i]] <- compare(object = data.frame(Pdata = cartsample_select_vars[i]),
                                data = data.frame(Pdata = bindori_select_vars[i]),
                                vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                msel = NULL, stat = "percents", breaks = 10,
                                nrow = 2, ncol = 2, rel.size.x = 1,
                                utility.stats = c("pMSE", "S_pMSE"),
                                cols = c("#1A3C5A","#4187BF"),
                                plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn_cart/oneway_compare_cartsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:55) {
  print(compare_plots_cartsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_cartsample <- c()
SpMSE_list_cartsample <- c()
for (i in 1:55) {
  pMSE_list_cartsample <- append(pMSE_list_cartsample, compare_plots_cartsample[[i]]$tab.utility[1])
  SpMSE_list_cartsample <- append(SpMSE_list_cartsample, compare_plots_cartsample[[i]]$tab.utility[2])
}

#create data frame
df_utility_cartsample <- data.frame(vars_list=colnames(cartsample_select_vars),
                         pMSE=pMSE_list_cartsample,
                         S_pMSE=SpMSE_list_cartsample)

write_utility_cartsample <- "./SyntheticData/Yue/syn_cart/oneway_utility_cartsample.csv"
write.csv(df_utility_cartsample, write_utility_cartsample, row.names=FALSE)

vars2show_cartsample <- df_utility_cartsample[df_utility_cartsample[, "S_pMSE"]<10, ][1]

nrow(vars2show_cartsample)  # there are 46 in total for cartsample


#******************* for cart norm
compare_plots_cartnorm<- c()

for (i in 1:55) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_cartnorm[[i]] <- compare(object = data.frame(Pdata = cartnorm_select_vars[i]),
                                           data = data.frame(Pdata = bindori_select_vars[i]),
                                           vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                           msel = NULL, stat = "percents", breaks = 10,
                                           nrow = 2, ncol = 2, rel.size.x = 1,
                                           utility.stats = c("pMSE", "S_pMSE"),
                                           cols = c("#1A3C5A","#4187BF"),
                                           plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn_cart/oneway_compare_cartnorm.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:55) {
  print(compare_plots_cartnorm[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_cartnorm <- c()
SpMSE_list_cartnorm <- c()
for (i in 1:55) {
  pMSE_list_cartnorm <- append(pMSE_list_cartnorm, compare_plots_cartnorm[[i]]$tab.utility[1])
  SpMSE_list_cartnorm <- append(SpMSE_list_cartnorm, compare_plots_cartnorm[[i]]$tab.utility[2])
}

#create data frame
df_utility_cartnorm <- data.frame(vars_list=colnames(cartnorm_select_vars),
                                    pMSE=pMSE_list_cartnorm,
                                    S_pMSE=SpMSE_list_cartnorm)

write_utility_cartnorm <- "./SyntheticData/Yue/syn_cart/oneway_utility_cartnorm.csv"
write.csv(df_utility_cartnorm, write_utility_cartnorm, row.names=FALSE)

vars2show_cartnorm <- df_utility_cartnorm[df_utility_cartnorm[, "S_pMSE"]<10, ][1]

nrow(vars2show_cartnorm)  # there are 45 in total for cartnorm


#******************* for cart normrank
compare_plots_cartnormrank<- c()

for (i in 1:55) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_cartnormrank[[i]] <- compare(object = data.frame(Pdata = cartnormrank_select_vars[i]),
                                         data = data.frame(Pdata = bindori_select_vars[i]),
                                         vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                         msel = NULL, stat = "percents", breaks = 10,
                                         nrow = 2, ncol = 2, rel.size.x = 1,
                                         utility.stats = c("pMSE", "S_pMSE"),
                                         cols = c("#1A3C5A","#4187BF"),
                                         plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn_cart/oneway_compare_cartnormrank.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:55) {
  print(compare_plots_cartnormrank[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_cartnormrank <- c()
SpMSE_list_cartnormrank <- c()
for (i in 1:55) {
  pMSE_list_cartnormrank <- append(pMSE_list_cartnormrank, compare_plots_cartnormrank[[i]]$tab.utility[1])
  SpMSE_list_cartnormrank <- append(SpMSE_list_cartnormrank, compare_plots_cartnormrank[[i]]$tab.utility[2])
}

#create data frame
df_utility_cartnormrank <- data.frame(vars_list=colnames(cartnormrank_select_vars),
                                  pMSE=pMSE_list_cartnormrank,
                                  S_pMSE=SpMSE_list_cartnormrank)

write_utility_cartnormrank <- "./SyntheticData/Yue/syn_cart/oneway_utility_cartnormrank.csv"
write.csv(df_utility_cartnormrank, write_utility_cartnormrank, row.names=FALSE)

vars2show_cartnormrank <- df_utility_cartnormrank[df_utility_cartnormrank[, "S_pMSE"]<10, ][1]

nrow(vars2show_cartnormrank)  # there are 46 in total for cartsample



#----------------------------------------------------------------#
#--------------------- method = polyreg -------------------------#
#----------------------------------------------------------------#

# now we start with the polyreg group by looping the saved .rda files

folder <- "./SyntheticData/Yue/syn_polyreg"
files <- list.files(folder, pattern = ".rda$")

syn_polyreg_models <- Map(rda2list, file.path(folder, files))
names(syn_polyreg_models) <- tools::file_path_sans_ext(files)

# we dataframe the lists
polyreg_sample_sds <- data.frame(syn_polyreg_models$sample_polyreg_syn)
# cart_norm_sds <- data.frame(syn_cart_models$norm_cart_syn)
# cart_normrank_sds <- data.frame(syn_cart_models$normrank_cart_syn)

# delete the prefix in variable naming
names(polyreg_sample_sds) <- sub('^syn.', '', names(polyreg_sample_sds))
# names(polyreg_norm_sds) <- sub('^syn.', '', names(cart_norm_sds))
# names(polyreg_normrank_sds) <- sub('^syn.', '', names(cart_normrank_sds))

str(polyreg_sample_sds) # =59
# str(cart_norm_sds)
# str(cart_normrank_sds)


#------------------Evaluating the utility of the polyreg_syn sds------------------

#=========(1). one-way marginals using compare()
# try with 55 variables firstly, we subset the original ods first
ncol(polyreg_sample_sds)
# bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(names(cart_sample_sds))
# # cuz the compare function cannot tackle with factor type variables, we delete them for evaluation
# bindori_select_vars <- subset(bindori_dataset_threshold_chr, select = -c(B2, B4, E5, E6))
polyregsample_select_vars <- subset(polyreg_sample_sds, select = -c(B2, B4, E5, E6))
# cartnorm_select_vars <- subset(cart_norm_sds, select = -c(B2, B4, E5, E6))
# cartnormrank_select_vars <- subset(cart_normrank_sds, select = -c(B2, B4, E5, E6))

ncol(polyregsample_select_vars) # ==59 ?

#******************* for cart sample
compare_plots_polyregsample<- c()

for (i in 1:55) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_polyregsample[[i]] <- compare(object = data.frame(Pdata = polyregsample_select_vars[i]),
                                           data = data.frame(Pdata = bindori_select_vars[i]),
                                           vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                           msel = NULL, stat = "percents", breaks = 10,
                                           nrow = 2, ncol = 2, rel.size.x = 1,
                                           utility.stats = c("pMSE", "S_pMSE"),
                                           cols = c("#1A3C5A","#4187BF"),
                                           plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn_polyreg/oneway_compare_polyregsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:55) {
  print(compare_plots_polyregsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_polyregsample <- c()
SpMSE_list_polyregsample <- c()
for (i in 1:55) {
  pMSE_list_polyregsample <- append(pMSE_list_polyregsample, compare_plots_polyregsample[[i]]$tab.utility[1])
  SpMSE_list_polyregsample <- append(SpMSE_list_polyregsample, compare_plots_polyregsample[[i]]$tab.utility[2])
}

#create data frame
df_utility_polyregsample <- data.frame(vars_list=colnames(polyregsample_select_vars),
                                    pMSE=pMSE_list_polyregsample,
                                    S_pMSE=SpMSE_list_polyregsample)

write_utility_polyregsample <- "./SyntheticData/Yue/syn_polyreg/oneway_utility_polyregsample.csv"
write.csv(df_utility_polyregsample, write_utility_polyregsample, row.names=FALSE)

vars2show_polyregsample <- df_utility_polyregsample[df_utility_polyregsample[, "S_pMSE"]<10, ][1]

nrow(vars2show_polyregsample)  # there are 46 in total for polyregsample

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
#                           Exp-2
#--------------------- method = rf ----------------------------#
#----------------------------------------------------------------#

# now we start with the cart group by looping the saved .rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder <- "./SyntheticData/Yue/syn2_rf"
files <- list.files(folder, pattern = ".rda$")

syn_rf_models <- Map(rda2list, file.path(folder, files))
names(syn_rf_models) <- tools::file_path_sans_ext(files)

# we dataframe the lists
rf_sample_0802 <- data.frame(syn_rf_models$rf0802_sample_syn)
rf_sample_0803 <- data.frame(syn_rf_models$rf0803_sample_syn)
rf_sample_0804 <- data.frame(syn_rf_models$rf0804_sample_syn)
rf_sample_0805 <- data.frame(syn_rf_models$rf0805_sample_syn)
rf_sample_0806 <- data.frame(syn_rf_models$rf0806_sample_syn)
rf_sample_0807 <- data.frame(syn_rf_models$rf0807_sample_syn)
rf_sample_0808 <- data.frame(syn_rf_models$rf0808_sample_syn)
names(rf_sample_0802) <- sub('^syn.', '', names(rf_sample_0802))
names(rf_sample_0803) <- sub('^syn.', '', names(rf_sample_0803))
names(rf_sample_0804) <- sub('^syn.', '', names(rf_sample_0804))
names(rf_sample_0805) <- sub('^syn.', '', names(rf_sample_0805))
names(rf_sample_0806) <- sub('^syn.', '', names(rf_sample_0806))
names(rf_sample_0807) <- sub('^syn.', '', names(rf_sample_0807))
names(rf_sample_0808) <- sub('^syn.', '', names(rf_sample_0808))
col_names <- names(rf_sample_0802)[2:54]
rf_sample_0802[col_names] <- lapply(rf_sample_0802[col_names], factor)
rf_sample_0803[col_names] <- lapply(rf_sample_0803[col_names], factor)
rf_sample_0804[col_names] <- lapply(rf_sample_0804[col_names], factor)
rf_sample_0805[col_names] <- lapply(rf_sample_0805[col_names], factor)
rf_sample_0806[col_names] <- lapply(rf_sample_0806[col_names], factor)
rf_sample_0807[col_names] <- lapply(rf_sample_0807[col_names], factor)
rf_sample_0808[col_names] <- lapply(rf_sample_0808[col_names], factor)
rf_sample_sds <- bind_rows(rf_sample_0802, rf_sample_0803, rf_sample_0804, rf_sample_0805,
                           rf_sample_0806, rf_sample_0807, rf_sample_0808)
table(rf_sample_sds$B2)
table(rf_sample_sds$B4)
table(rf_sample_sds$E5)
table(rf_sample_sds$E6)

rf_norm_0802 <- data.frame(syn_rf_models$rf0802_norm_syn)
rf_norm_0803 <- data.frame(syn_rf_models$rf0803_norm_syn)
rf_norm_0804 <- data.frame(syn_rf_models$rf0804_norm_syn)
rf_norm_0805 <- data.frame(syn_rf_models$rf0805_norm_syn)
rf_norm_0806 <- data.frame(syn_rf_models$rf0806_norm_syn)
rf_norm_0807 <- data.frame(syn_rf_models$rf0807_norm_syn)
rf_norm_0808 <- data.frame(syn_rf_models$rf0808_norm_syn)
names(rf_norm_0802) <- sub('^syn.', '', names(rf_norm_0802))
names(rf_norm_0803) <- sub('^syn.', '', names(rf_norm_0803))
names(rf_norm_0804) <- sub('^syn.', '', names(rf_norm_0804))
names(rf_norm_0805) <- sub('^syn.', '', names(rf_norm_0805))
names(rf_norm_0806) <- sub('^syn.', '', names(rf_norm_0806))
names(rf_norm_0807) <- sub('^syn.', '', names(rf_norm_0807))
names(rf_norm_0808) <- sub('^syn.', '', names(rf_norm_0808))
table(bindori_dataset_threshold_chr$B2)
table(rf_norm_0802$B2)  # 2     3     4
table(rf_norm_0802$B4)  # 1     2     3
table(rf_norm_0802$E5)  # 1     2     3
table(rf_norm_0802$E6)  # 0     1     2     3
table(rf_norm_0803$B2)  # 2     3     4
table(rf_norm_0803$B4)  # 1     2     3
table(rf_norm_0803$E5)  # 1     2     3
table(rf_norm_0803$E6)  # 0     1     2     3
table(rf_norm_0804$B2)  # -1  -99 [0, 1) [1, 3)
table(rf_norm_0804$B4)  # -99 [0, 1) [1, 5)
table(rf_norm_0804$E5)  # -99 [0, 1) [1, 2)
table(rf_norm_0804$E6)  # 0     1     2     3
table(rf_norm_0805$B2)  # 1     2     3
table(rf_norm_0805$B4)  # 1     2     3
table(rf_norm_0805$E5)  # 1     2     3
table(rf_norm_0805$E6)  # 0     1     2     3
table(rf_norm_0806$B2)  # 1     2     3
table(rf_norm_0806$B4)  # 1     2     3
table(rf_norm_0806$E5)  # 1     2     3
table(rf_norm_0806$E6)  # 0     1     2     3
table(rf_norm_0807$B2)  # 1     2     3
table(rf_norm_0807$B4)  # 1     2     3
table(rf_norm_0807$E5)  # 1     2     3
table(rf_norm_0807$E6)  # 0     1     2     3
table(rf_norm_0808$B2)  # 1     2     3
table(rf_norm_0808$B4)  # 1     2     3
table(rf_norm_0808$E5)  # 1     2     3
table(rf_norm_0808$E6)  # 0     1     2     3

rf_norm_0804$B2 <- as.integer(rf_norm_0804$B2)
rf_norm_0804$B4 <- as.integer(rf_norm_0804$B4)
rf_norm_0804$E5 <- as.integer(rf_norm_0804$E5)

col_names <- names(rf_norm_0802)[2:54]
rf_norm_0802[col_names] <- lapply(rf_norm_0802[col_names], factor)
rf_norm_0803[col_names] <- lapply(rf_norm_0803[col_names], factor)
rf_norm_0804[col_names] <- lapply(rf_norm_0804[col_names], factor)
rf_norm_0805[col_names] <- lapply(rf_norm_0805[col_names], factor)
rf_norm_0806[col_names] <- lapply(rf_norm_0806[col_names], factor)
rf_norm_0807[col_names] <- lapply(rf_norm_0807[col_names], factor)
rf_norm_0808[col_names] <- lapply(rf_norm_0808[col_names], factor)
rf_norm_sds <- bind_rows(rf_norm_0802, rf_norm_0803, rf_norm_0804, rf_norm_0805,
                         rf_norm_0806, rf_norm_0807, rf_norm_0808)
table(rf_norm_sds$B2)
table(rf_norm_sds$B4)
table(rf_norm_sds$E5)

rf_normrank_0802 <- data.frame(syn_rf_models$rf0802_normrank_syn)
rf_normrank_0803 <- data.frame(syn_rf_models$rf0803_normrank_syn)
rf_normrank_0804 <- data.frame(syn_rf_models$rf0804_normrank_syn)
rf_normrank_0805 <- data.frame(syn_rf_models$rf0805_normrank_syn)
rf_normrank_0806 <- data.frame(syn_rf_models$rf0806_normrank_syn)
rf_normrank_0807 <- data.frame(syn_rf_models$rf0807_normrank_syn)
rf_normrank_0808 <- data.frame(syn_rf_models$rf0808_normrank_syn)
names(rf_normrank_0802) <- sub('^syn.', '', names(rf_normrank_0802))
names(rf_normrank_0803) <- sub('^syn.', '', names(rf_normrank_0803))
names(rf_normrank_0804) <- sub('^syn.', '', names(rf_normrank_0804))
names(rf_normrank_0805) <- sub('^syn.', '', names(rf_normrank_0805))
names(rf_normrank_0806) <- sub('^syn.', '', names(rf_normrank_0806))
names(rf_normrank_0807) <- sub('^syn.', '', names(rf_normrank_0807))
names(rf_normrank_0808) <- sub('^syn.', '', names(rf_normrank_0808))
table(rf_normrank_0802$B2)  # 2     3     4
table(rf_normrank_0802$B4)  # 1     2     3
table(rf_normrank_0802$E5)  # 1     2     3
table(rf_normrank_0802$E6)  # 1     2
table(rf_normrank_0803$B2)  # 2     3     4
table(rf_normrank_0803$B4)  # 1     2     3
table(rf_normrank_0803$E5)  # 1     2     3
table(rf_normrank_0803$E6)  # 1     2
table(rf_normrank_0804$B2)  # -1  -99 [0, 1) [1, 3)
table(rf_normrank_0804$B4)  # -99 [0, 1) [1, 5)
table(rf_normrank_0804$E5)  # -99 [0, 1) [1, 2)
table(rf_normrank_0804$E6)  # 1    2
table(rf_normrank_0805$B2)  # 1     2     3
table(rf_normrank_0805$B4)  # 1     2     3
table(rf_normrank_0805$E5)  # 1     2     3
table(rf_normrank_0805$E6)  # 1     2
table(rf_normrank_0806$B2)  # 1     2     3
table(rf_normrank_0806$B4)  # 1     2     3
table(rf_normrank_0806$E5)  # 1     2     3
table(rf_normrank_0806$E6)  # 1     2
table(rf_normrank_0807$B2)  # 1     2     3
table(rf_normrank_0807$B4)  # 1     2     3
table(rf_normrank_0807$E5)  # 1     2     3
table(rf_normrank_0807$E6)  # 1     2
table(rf_normrank_0808$B2)  # 1     2     3
table(rf_normrank_0808$B4)  # 1     2     3
table(rf_normrank_0808$E5)  # 1     2     3
table(rf_normrank_0808$E6)  # 1     2

# rf_normrank_0804$B2[rf_normrank_0804$B2 == "-99"] <- "2"
# rf_normrank_0804$B2[rf_normrank_0804$B2 == "[0, 1)"] <- "3"
# rf_normrank_0804$B2[rf_normrank_0804$B2 == "[1, 3)"] <- "4"
rf_normrank_0804$B2 <- as.integer(rf_normrank_0804$B2)
rf_normrank_0804$B4 <- as.integer(rf_normrank_0804$B4)
rf_normrank_0804$E5 <- as.integer(rf_normrank_0804$E5)

col_names <- names(rf_normrank_0802)[2:54]
rf_normrank_0802[col_names] <- lapply(rf_normrank_0802[col_names], factor)
rf_normrank_0803[col_names] <- lapply(rf_normrank_0803[col_names], factor)
rf_normrank_0804[col_names] <- lapply(rf_normrank_0804[col_names], factor)
rf_normrank_0805[col_names] <- lapply(rf_normrank_0805[col_names], factor)
rf_normrank_0806[col_names] <- lapply(rf_normrank_0806[col_names], factor)
rf_normrank_0807[col_names] <- lapply(rf_normrank_0807[col_names], factor)
rf_normrank_0808[col_names] <- lapply(rf_normrank_0808[col_names], factor)
rf_normrank_sds <- bind_rows(rf_normrank_0802, rf_normrank_0803, rf_normrank_0804, rf_normrank_0805,
                             rf_normrank_0806, rf_normrank_0807, rf_normrank_0808)
table(rf_normrank_sds$B2)
table(rf_normrank_sds$B4)
table(rf_normrank_sds$E5)

str(rf_sample_sds)
str(rf_norm_sds)
str(rf_normrank_sds)

# for rf_sample, rf_norm and normrank, we will have to reverse the integer back to the factor

# additionally, for rf sample, norm and norm, we reverse the integer back to interval
table(rf_sample_sds$B2, rf_norm_sds$B2) # exclude B2 for norm
table(bindori_dataset_threshold_chr$B2)
table(rf_sample_sds$B2)
rf_sample_sds$B2[rf_sample_sds$B2 == "1"] <- "-1"
rf_sample_sds$B2[rf_sample_sds$B2 == "2"] <- "-99"
rf_sample_sds$B2[rf_sample_sds$B2 == "3"] <- "[0, 1)"
rf_sample_sds$B2[rf_sample_sds$B2 == "4"] <- "[1, 3)"
rf_norm_sds$B2[rf_norm_sds$B2 == "1"] <- "-1"
rf_norm_sds$B2[rf_norm_sds$B2 == "2"] <- "-99"
rf_norm_sds$B2[rf_norm_sds$B2 == "3"] <- "[0, 1)"
rf_norm_sds$B2[rf_norm_sds$B2 == "4"] <- "[1, 3)"
rf_normrank_sds$B2[rf_normrank_sds$B2 == "1"] <- "-1"
rf_normrank_sds$B2[rf_normrank_sds$B2 == "2"] <- "-99"
rf_normrank_sds$B2[rf_normrank_sds$B2 == "3"] <- "[0, 1)"
rf_normrank_sds$B2[rf_normrank_sds$B2 == "4"] <- "[1, 3)"

table(rf_sample_sds$B4, rf_norm_sds$B4) # exclude B4 for norm
table(bindori_dataset_threshold_chr$B4)
rf_sample_sds$B4[rf_sample_sds$B4 == "1"] <- "-99"
rf_sample_sds$B4[rf_sample_sds$B4 == "2"] <- "[0, 1)"
rf_sample_sds$B4[rf_sample_sds$B4 == "3"] <- "[1, 5)"
rf_norm_sds$B4[rf_norm_sds$B4 == "1"] <- "-99"
rf_norm_sds$B4[rf_norm_sds$B4 == "2"] <- "[0, 1)"
rf_norm_sds$B4[rf_norm_sds$B4 == "3"] <- "[1, 5)"
rf_normrank_sds$B4[rf_normrank_sds$B4 == "1"] <- "-99"
rf_normrank_sds$B4[rf_normrank_sds$B4 == "2"] <- "[0, 1)"
rf_normrank_sds$B4[rf_normrank_sds$B4 == "3"] <- "[1, 5)"

table(rf_sample_sds$E5, rf_norm_sds$E5) # exclude E5 for norm
table(bindori_dataset_threshold_chr$E5)
rf_sample_sds$E5[rf_sample_sds$E5 == "1"] <- "-99"
rf_sample_sds$E5[rf_sample_sds$E5 == "2"] <- "[0, 1)"
rf_sample_sds$E5[rf_sample_sds$E5 == "3"] <- "[1, 2)"
rf_norm_sds$E5[rf_norm_sds$E5 == "1"] <- "-99"
rf_norm_sds$E5[rf_norm_sds$E5 == "2"] <- "[0, 1)"
rf_norm_sds$E5[rf_norm_sds$E5 == "3"] <- "[1, 2)"
rf_normrank_sds$E5[rf_normrank_sds$E5 == "1"] <- "-99"
rf_normrank_sds$E5[rf_normrank_sds$E5 == "2"] <- "[0, 1)"
rf_normrank_sds$E5[rf_normrank_sds$E5 == "3"] <- "[1, 2)"

table(rf_sample_sds$E6, rf_norm_sds$E6) # exclude E6 for norm, 50 in total
table(bindori_dataset_threshold_chr$E6)
rf_sample_sds$E6[rf_sample_sds$E6 == "1"] <- "-99"
rf_sample_sds$E6[rf_sample_sds$E6 == "2"] <- "[0, 9)"
rf_norm_sds$E6[rf_norm_sds$E6 == "1"] <- "-99"
rf_norm_sds$E6[rf_norm_sds$E6 == "2"] <- "[0, 9)"
rf_normrank_sds$E6[rf_normrank_sds$E6 == "1"] <- "-99"
rf_normrank_sds$E6[rf_normrank_sds$E6 == "2"] <- "[0, 9)"

str(rf_sample_sds)
# change var "B2", "B4", "E5", "E6" to factor type
cols_factor <- c("B2", "B4", "E5", "E6")
cart_sample_sds[cols_factor] <- lapply(cart_sample_sds[cols_factor], factor)
cart_norm_sds[cols_factor] <- lapply(cart_norm_sds[cols_factor], factor)
cart_normrank_sds[cols_factor] <- lapply(cart_normrank_sds[cols_factor], factor)

str(cart_sample_sds)
str(cart_norm_sds)
str(cart_normrank_sds)
table(cart_norm_sds$E6)
table(cart_normrank_sds$E6)
table(cart_sample_sds$E6)
#------------------Evaluating the utility of the cart_syn sds------------------

#=========(1). one-way marginals using compare()
# try with 54-4 variables firstly, we subset the original ods first
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

for (i in 1:50) {
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
destination_path <- "./SyntheticData/Yue/syn1_cart/oneway_compare_cartsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:50) {
  print(compare_plots_cartsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_cartsample <- c()
SpMSE_list_cartsample <- c()
for (i in 1:50) {
  pMSE_list_cartsample <- append(pMSE_list_cartsample, compare_plots_cartsample[[i]]$tab.utility[1])
  SpMSE_list_cartsample <- append(SpMSE_list_cartsample, compare_plots_cartsample[[i]]$tab.utility[2])
}

#create data frame
df_utility_cartsample <- data.frame(vars_list=colnames(cartsample_select_vars),
                         pMSE=pMSE_list_cartsample,
                         S_pMSE=SpMSE_list_cartsample)

write_utility_cartsample <- "./SyntheticData/Yue/syn1_cart/oneway_utility_cartsample.csv"
write.csv(df_utility_cartsample, write_utility_cartsample, row.names=FALSE)

vars2show_cartsample <- df_utility_cartsample[df_utility_cartsample[, "S_pMSE"]<10, ][1]

nrow(vars2show_cartsample)  # there are 46 in total for cartsample


#******************* for cart norm
compare_plots_cartnorm<- c()

for (i in 1:50) {
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
destination_path <- "./SyntheticData/Yue/syn1_cart/oneway_compare_cartnorm.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:50) {
  print(compare_plots_cartnorm[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_cartnorm <- c()
SpMSE_list_cartnorm <- c()
for (i in 1:50) {
  pMSE_list_cartnorm <- append(pMSE_list_cartnorm, compare_plots_cartnorm[[i]]$tab.utility[1])
  SpMSE_list_cartnorm <- append(SpMSE_list_cartnorm, compare_plots_cartnorm[[i]]$tab.utility[2])
}

#create data frame
df_utility_cartnorm <- data.frame(vars_list=colnames(cartnorm_select_vars),
                                    pMSE=pMSE_list_cartnorm,
                                    S_pMSE=SpMSE_list_cartnorm)

write_utility_cartnorm <- "./SyntheticData/Yue/syn1_cart/oneway_utility_cartnorm.csv"
write.csv(df_utility_cartnorm, write_utility_cartnorm, row.names=FALSE)

vars2show_cartnorm <- df_utility_cartnorm[df_utility_cartnorm[, "S_pMSE"]<10, ][1]

nrow(vars2show_cartnorm)  # there are 45 in total for cartnorm


#******************* for cart normrank
compare_plots_cartnormrank<- c()

for (i in 1:50) {
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
destination_path <- "./SyntheticData/Yue/syn1_cart/oneway_compare_cartnormrank.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:50) {
  print(compare_plots_cartnormrank[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_cartnormrank <- c()
SpMSE_list_cartnormrank <- c()
for (i in 1:50) {
  pMSE_list_cartnormrank <- append(pMSE_list_cartnormrank, compare_plots_cartnormrank[[i]]$tab.utility[1])
  SpMSE_list_cartnormrank <- append(SpMSE_list_cartnormrank, compare_plots_cartnormrank[[i]]$tab.utility[2])
}

#create data frame
df_utility_cartnormrank <- data.frame(vars_list=colnames(cartnormrank_select_vars),
                                  pMSE=pMSE_list_cartnormrank,
                                  S_pMSE=SpMSE_list_cartnormrank)

write_utility_cartnormrank <- "./SyntheticData/Yue/syn1_cart/oneway_utility_cartnormrank.csv"
write.csv(df_utility_cartnormrank, write_utility_cartnormrank, row.names=FALSE)

vars2show_cartnormrank <- df_utility_cartnormrank[df_utility_cartnormrank[, "S_pMSE"]<10, ][1]

nrow(vars2show_cartnormrank)  # there are 46 in total for cartsample



#----------------------------------------------------------------#
#--------------------- method = polyreg -------------------------#
#----------------------------------------------------------------#

# now we start with the polyreg group by looping the saved .rda files

folder <- "./SyntheticData/Yue/syn4_polyreg"
files <- list.files(folder, pattern = ".rda$")

syn_polyreg_models <- Map(rda2list, file.path(folder, files))
names(syn_polyreg_models) <- tools::file_path_sans_ext(files)

# we dataframe the lists
polyreg_sample_sds <- data.frame(syn_polyreg_models$polyreg_sample_syn)
polyreg_norm_sds <- data.frame(syn_polyreg_models$polyreg_norm_syn)
polyreg_normrank_sds <- data.frame(syn_polyreg_models$polyreg_normrank_syn)
# cart_norm_sds <- data.frame(syn_cart_models$norm_cart_syn)
# cart_normrank_sds <- data.frame(syn_cart_models$normrank_cart_syn)

# delete the prefix in variable naming
names(polyreg_sample_sds) <- sub('^syn.', '', names(polyreg_sample_sds))
names(polyreg_norm_sds) <- sub('^syn.', '', names(cart_norm_sds))
names(polyreg_normrank_sds) <- sub('^syn.', '', names(cart_normrank_sds))

str(polyreg_sample_sds) # =59
str(polyreg_norm_sds)
str(polyreg_normrank_sds)

ncol(polyreg_normrank_sds)
#------------------Evaluating the utility of the polyreg_syn sds------------------

#=========(1). one-way marginals using compare()
# try with 55 variables firstly, we subset the original ods first
ncol(polyreg_sample_sds)
# bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(names(cart_sample_sds))
# # cuz the compare function cannot tackle with factor type variables, we delete them for evaluation
# bindori_select_vars <- subset(bindori_dataset_threshold_chr, select = -c(B2, B4, E5, E6))
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

str(polyreg_norm_sds)

polyregsample_select_vars <- subset(polyreg_sample_sds, select = -c(B2, B4, E5, E6))
polyregnorm_select_vars <- subset(polyreg_norm_sds, select = -c(B2, B4, E5, E6))
polyregnormrank_select_vars <- subset(polyreg_normrank_sds, select = -c(B2, B4, E5, E6))

ncol(polyregsample_select_vars) # ==50 ?

#******************* for polyreg sample
compare_plots_polyregsample<- c()

for (i in 1:50) {
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
destination_path <- "./SyntheticData/Yue/syn4_polyreg/oneway_compare_polyregsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:50) {
  print(compare_plots_polyregsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_polyregsample <- c()
SpMSE_list_polyregsample <- c()
for (i in 1:50) {
  pMSE_list_polyregsample <- append(pMSE_list_polyregsample, compare_plots_polyregsample[[i]]$tab.utility[1])
  SpMSE_list_polyregsample <- append(SpMSE_list_polyregsample, compare_plots_polyregsample[[i]]$tab.utility[2])
}

#create data frame
df_utility_polyregsample <- data.frame(vars_list=colnames(polyregsample_select_vars),
                                    pMSE=pMSE_list_polyregsample,
                                    S_pMSE=SpMSE_list_polyregsample)

write_utility_polyregsample <- "./SyntheticData/Yue/syn4_polyreg/oneway_utility_polyregsample.csv"
write.csv(df_utility_polyregsample, write_utility_polyregsample, row.names=FALSE)

vars2show_polyregsample <- df_utility_polyregsample[df_utility_polyregsample[, "S_pMSE"]<10, ][1]

nrow(vars2show_polyregsample)  # there are 46 in total for polyregsample

# two-way marginals with utility.tables()
# try with F1, F2_1, F2_2
selected_cols <- c("F1", "F2_1", "F2_2")
polyregsample_twoway_vars <- polyregsample_select_vars[, selected_cols]
bindori_twoway_vars <- bindori_select_vars[, selected_cols]
## S3 method for class 'data.frame'
utility.twoway <- utility.tables(object = data.frame(polyregsample_twoway_vars), 
                                 data = data.frame(bindori_twoway_vars),
                                 tables = "twoway",
                                 tab.stats = c("pMSE"), 
                                 plot.stat = "pMSE", plot = TRUE,  
                                 print.tabs = TRUE)
utility.twoway$utility.plot

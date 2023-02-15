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
wd <- "/Users/roxy/Desktop/Master-Thesis-SyntheticDataGeneration"
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
# first try change them to char from var B2 to E6 (51-54)
col_names <- names(rf_sample_0802)[51:54]
rf_sample_0802[col_names] <- lapply(rf_sample_0802[col_names], as.character)
rf_sample_0803[col_names] <- lapply(rf_sample_0803[col_names], as.character)
rf_sample_0804[col_names] <- lapply(rf_sample_0804[col_names], as.character)
rf_sample_0805[col_names] <- lapply(rf_sample_0805[col_names], as.character)
rf_sample_0806[col_names] <- lapply(rf_sample_0806[col_names], as.character)
rf_sample_0807[col_names] <- lapply(rf_sample_0807[col_names], as.character)
rf_sample_0808[col_names] <- lapply(rf_sample_0808[col_names], as.character)
rf_sample_sds <- bind_rows(rf_sample_0802, rf_sample_0803, rf_sample_0804, rf_sample_0805,
                           rf_sample_0806, rf_sample_0807, rf_sample_0808)
table(rf_sample_sds$B2)  # 1     2     3     4
table(rf_sample_sds$B4)  # 1     2      3
table(rf_sample_sds$E5)  # 1     2      3
table(rf_sample_sds$E6)  # 1     2


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
str(rf_norm_0804)

col_names <- names(rf_norm_0802)[51:54]
rf_norm_0802[col_names] <- lapply(rf_norm_0802[col_names], as.character)
rf_norm_0803[col_names] <- lapply(rf_norm_0803[col_names], as.character)
rf_norm_0804[col_names] <- lapply(rf_norm_0804[col_names], as.character)
rf_norm_0805[col_names] <- lapply(rf_norm_0805[col_names], as.character)
rf_norm_0806[col_names] <- lapply(rf_norm_0806[col_names], as.character)
rf_norm_0807[col_names] <- lapply(rf_norm_0807[col_names], as.character)
rf_norm_0808[col_names] <- lapply(rf_norm_0808[col_names], as.character)
rf_norm_sds <- bind_rows(rf_norm_0802, rf_norm_0803, rf_norm_0804, rf_norm_0805,
                         rf_norm_0806, rf_norm_0807, rf_norm_0808)
# check whether there are conflicts in B2, B4, E5 and E6
table(rf_norm_sds$B2)
table(rf_norm_sds$B4)
table(rf_norm_sds$E5)
table(rf_norm_sds$E6)
str(rf_norm_sds)

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

rf_normrank_0804$B2 <- as.integer(rf_normrank_0804$B2)
rf_normrank_0804$B4 <- as.integer(rf_normrank_0804$B4)
rf_normrank_0804$E5 <- as.integer(rf_normrank_0804$E5)
str(rf_normrank_0804)

col_names <- names(rf_normrank_0802)[51:54]
rf_normrank_0802[col_names] <- lapply(rf_normrank_0802[col_names], as.character)
rf_normrank_0803[col_names] <- lapply(rf_normrank_0803[col_names], as.character)
rf_normrank_0804[col_names] <- lapply(rf_normrank_0804[col_names], as.character)
rf_normrank_0805[col_names] <- lapply(rf_normrank_0805[col_names], as.character)
rf_normrank_0806[col_names] <- lapply(rf_normrank_0806[col_names], as.character)
rf_normrank_0807[col_names] <- lapply(rf_normrank_0807[col_names], as.character)
rf_normrank_0808[col_names] <- lapply(rf_normrank_0808[col_names], as.character)
rf_normrank_sds <- bind_rows(rf_normrank_0802, rf_normrank_0803, rf_normrank_0804, rf_normrank_0805,
                             rf_normrank_0806, rf_normrank_0807, rf_normrank_0808)
table(rf_normrank_sds$B2)
table(rf_normrank_sds$B4)
table(rf_normrank_sds$E5)
table(rf_normrank_sds$E6)

str(rf_sample_sds)
str(rf_norm_sds)
str(rf_normrank_sds)

# for rf_sample, rf_norm and normrank, we will have to reverse the integer back to the factor

# additionally, for rf sample, norm and norm, we reverse the integer back to interval
table(rf_sample_sds$B2, rf_norm_sds$B2) # include B2 for norm
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

table(rf_sample_sds$B4, rf_norm_sds$B4) # include B4 for norm
table(rf_sample_sds$B4, rf_normrank_sds$B4)
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

table(rf_sample_sds$E5, rf_norm_sds$E5) # include E5 for norm
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

table(rf_sample_sds$E6, rf_norm_sds$E6) # exclude E6 for rf_norm, 53 in total
table(rf_normrank_sds$E6)
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
rf_sample_sds[cols_factor] <- lapply(rf_sample_sds[cols_factor], factor)
rf_norm_sds[cols_factor] <- lapply(rf_norm_sds[cols_factor], factor)
rf_normrank_sds[cols_factor] <- lapply(rf_normrank_sds[cols_factor], factor)

str(rf_sample_sds)
str(rf_norm_sds)
str(rf_normrank_sds)
table(rf_sample_sds$E6, rf_norm_sds$E6)
table(rf_sample_sds$E6, rf_normrank_sds$E6)
table(bindori_dataset_threshold_chr$E6)
# data  export
#-----------------------------------------------
export_path <- "./SyntheticData/Yue/syn2_rf"
rfsample_sds <- "rf_sample_syn.rda"
rfnorm_sds <- "rf_norm_syn.rda"
rfnormrank_sds <- "rf_normrank_syn.rda"

save(rf_sample_sds, file=paste(c(export_path, rfsample_sds), 
                                collapse="/"))
save(rf_norm_sds, file=paste(c(export_path, rfnorm_sds), 
                              collapse="/"))
save(rf_normrank_sds, file=paste(c(export_path, rfnormrank_sds), 
                                  collapse="/"))

# is it necessary to add more levels to the dataset
levels(bindori_dataset_threshold_chr$E6) <- c(levels(bindori_dataset_threshold_chr$E6), "0", "3")
levels(rf_sample_sds$E6) <- c(levels(rf_sample_sds$E6), "0", "3")
levels(rf_normrank_sds$E6) <- c(levels(rf_normrank_sds$E6), "0", "3")

#------------------Evaluating the utility of the rf_syn sds------------------

#=========(1). one-way marginals using compare()
# try with 54-1 variables firstly, we subset the original ods first
ncol(rf_sample_sds)
bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(names(rf_sample_sds))
# cuz the compare function cannot tackle with factor type variables, we delete them for evaluation
bindori_select_vars <- bindori_dataset_threshold_chr
rfsample_select_vars <- rf_sample_sds
rfnorm_select_vars <- rf_norm_sds
rfnormrank_select_vars <- rf_normrank_sds

ncol(rfsample_select_vars)

#******************* for rf sample
compare_plots_rfsample<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_rfsample[[i]] <- compare(object = data.frame(Pdata = rfsample_select_vars[i]),
                                data = data.frame(Pdata = bindori_select_vars[i]),
                                vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                msel = NULL, stat = "percents", breaks = 10,
                                nrow = 2, ncol = 2, rel.size.x = 1,
                                utility.stats = c("pMSE", "S_pMSE"),
                                cols = c("#1A3C5A","#4187BF"),
                                plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn2_rf/oneway_compare_rfsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_rfsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_rfsample <- c()
SpMSE_list_rfsample <- c()
for (i in 1:54) {
  pMSE_list_rfsample <- append(pMSE_list_rfsample, compare_plots_rfsample[[i]]$tab.utility[1])
  SpMSE_list_rfsample <- append(SpMSE_list_rfsample, compare_plots_rfsample[[i]]$tab.utility[2])
}

#create data frame
df_utility_rfsample <- data.frame(vars_list=colnames(rfsample_select_vars),
                         pMSE=pMSE_list_rfsample,
                         S_pMSE=SpMSE_list_rfsample)

write_utility_rfsample <- "./SyntheticData/Yue/syn2_rf/oneway_utility_rfsample.csv"
write.csv(df_utility_rfsample, write_utility_rfsample, row.names=FALSE)

vars2show_rfsample <- df_utility_rfsample[df_utility_rfsample[, "S_pMSE"]<10, ][1]

nrow(vars2show_rfsample)  # there are 28 in total for rfsample


#******************* for rf norm
compare_plots_rfnorm<- c()

for (i in 1:length(rfnorm_select_vars)) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_rfnorm[[i]] <- compare(object = data.frame(Pdata = rfnorm_select_vars[i]),
                                           data = data.frame(Pdata = bindori_select_vars[i]),
                                           vars = c(colnames(rfnorm_select_vars[i])), cont.na = NULL,
                                           msel = NULL, stat = "percents", breaks = 10,
                                           nrow = 2, ncol = 2, rel.size.x = 1,
                                           utility.stats = c("pMSE", "S_pMSE"),
                                           cols = c("#1A3C5A","#4187BF"),
                                           plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn2_rf/oneway_compare_rfnorm.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_rfnorm[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_rfnorm <- c()
SpMSE_list_rfnorm <- c()
for (i in 1:54) {
  pMSE_list_rfnorm <- append(pMSE_list_rfnorm, compare_plots_rfnorm[[i]]$tab.utility[1])
  SpMSE_list_rfnorm <- append(SpMSE_list_rfnorm, compare_plots_rfnorm[[i]]$tab.utility[2])
}

#create data frame
df_utility_rfnorm <- data.frame(vars_list=colnames(rfnorm_select_vars),
                                    pMSE=pMSE_list_rfnorm,
                                    S_pMSE=SpMSE_list_rfnorm)

write_utility_rfnorm <- "./SyntheticData/Yue/syn2_rf/oneway_utility_rfnorm.csv"
write.csv(df_utility_rfnorm, write_utility_rfnorm, row.names=FALSE)

vars2show_rfnorm <- df_utility_rfnorm[df_utility_rfnorm[, "S_pMSE"]<10, ][1]

nrow(vars2show_rfnorm)  # there are 30 in total for rfnorm


#******************* for rf normrank
compare_plots_rfnormrank<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_rfnormrank[[i]] <- compare(object = data.frame(Pdata = rfnormrank_select_vars[i]),
                                         data = data.frame(Pdata = bindori_select_vars[i]),
                                         vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                         msel = NULL, stat = "percents", breaks = 10,
                                         nrow = 2, ncol = 2, rel.size.x = 1,
                                         utility.stats = c("pMSE", "S_pMSE"),
                                         cols = c("#1A3C5A","#4187BF"),
                                         plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn2_rf/oneway_compare_rfnormrank.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_rfnormrank[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_rfnormrank <- c()
SpMSE_list_rfnormrank <- c()
for (i in 1:54) {
  pMSE_list_rfnormrank <- append(pMSE_list_rfnormrank, compare_plots_rfnormrank[[i]]$tab.utility[1])
  SpMSE_list_rfnormrank <- append(SpMSE_list_rfnormrank, compare_plots_rfnormrank[[i]]$tab.utility[2])
}

#create data frame
df_utility_rfnormrank <- data.frame(vars_list=colnames(rfnormrank_select_vars),
                                  pMSE=pMSE_list_rfnormrank,
                                  S_pMSE=SpMSE_list_rfnormrank)

write_utility_rfnormrank <- "./SyntheticData/Yue/syn2_rf/oneway_utility_rfnormrank.csv"
write.csv(df_utility_rfnormrank, write_utility_rfnormrank, row.names=FALSE)

vars2show_rfnormrank <- df_utility_rfnormrank[df_utility_rfnormrank[, "S_pMSE"]<10, ][1]

nrow(vars2show_rfnormrank)  # there are 27 in total for rfnormrank

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
library(ranger)

set.seed(2023) # make sure the results is reproducible
#*****************************************************
# Model 1: contact tracing app -- F2_1

# step1: prepare the datasets
vars_inc_m1 <- c("D1","D2","D3","D4","D5","D7","D8","D9","E2","E3","E4","E7","E5","E6","F2_1")
ods_m1 <- bindori_dataset_threshold_chr[vars_inc_m1]
table(ods_m1$F2_1)

sds_rfsample_m1 <- rf_sample_sds[vars_inc_m1]
table(sds_rfsample_m1$F2_1)

sds_rfnorm_m1 <- rf_norm_sds[vars_inc_m1]
table(sds_rfnorm_m1$F2_1)

sds_rfnormrank_m1 <- rf_normrank_sds[vars_inc_m1]
table(sds_rfnormrank_m1$F2_1)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m1 <- TaskClassif$new(id="tsk_ods_m1",
                              backend=ods_m1, target="F2_1")

tsk_rfsample_m1 <- TaskClassif$new(id="tsk_rfsample_m1", 
                                     backend=sds_rfsample_m1, target="F2_1")

tsk_rfnorm_m1 <- TaskClassif$new(id="tsk_rfnorm_m1", 
                                   backend=sds_rfnorm_m1, target="F2_1")

tsk_rfnormrank_m1 <- TaskClassif$new(id="tsk_rfnormrank_m1",
                                       backend=sds_rfnormrank_m1, target="F2_1")

tasks_list <- list(tsk_ods_m1, tsk_rfsample_m1,tsk_rfnorm_m1, tsk_rfnormrank_m1, 
                   tsk_ods_m2, tsk_rfsample_m2, tsk_rfnorm_m2, tsk_rfnormrank_m2)

# step3: prepare the required learners
learners_list <- lrns(c("classif.multinom", "classif.ranger"))

# step4: benchmark the task and learners with cross-validation
# benchmark_grid is the design
bm_models <- benchmark(benchmark_grid(tasks = tasks_list,
                                      learners = learners_list, resamplings = rsmp("holdout", ratio=0.8)),
                       store_models = TRUE)

# step5: validate the accuracy of the model
#****** Measure to compare true observed 
#****** labels with predicted labels in 
#****** multiclass classification tasks.
bm_models$aggregate(msr("classif.acc"))[learner_id=="classif.ranger",]

# step6: extract the coefficients of the trained instances
mlr3misc::map(as.data.table(bm_model1)$learner, "model")

# step7: save bm_model as rds
saveRDS(bm_models, './SyntheticData/Yue/syn2_rf/bm_rf_models.rds')

#*****************************************************
# Model 2: covid positive -- B8 (multiclass)

# step1: prepare the datasets
vars_inc_m2 <- c("E2","E3","E4","E7","E5","E6","C1_m","C2","C3","C5","C6","C7","C8","B8")
ods_m2 <- bindori_dataset_threshold_chr[vars_inc_m2]
ods_m2$B8 = factor(ods_m2$B8)
table(ods_m2$B8)

sds_rfsample_m2 <- rf_sample_sds[vars_inc_m2]
sds_rfsample_m2$B8 = factor(sds_rfsample_m2$B8)

sds_rfnorm_m2 <- rf_norm_sds[vars_inc_m2]
sds_rfnorm_m2$B8 = factor(sds_rfnorm_m2$B8)

sds_rfnormrank_m2 <- rf_normrank_sds[vars_inc_m2]
sds_rfnormrank_m2$B8 = factor(sds_rfnormrank_m2$B8)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m2 <- TaskClassif$new(id="tsk_ods_m2",
                              backend=ods_m2, target="B8")

tsk_rfsample_m2 <- TaskClassif$new(id="tsk_rfsample_m2", 
                                     backend=sds_rfsample_m2, target="B8")

tsk_rfnorm_m2 <- TaskClassif$new(id="tsk_rfnorm_m2", 
                                   backend=sds_rfnorm_m2, target="B8")

tsk_rfnormrank_m2 <- TaskClassif$new(id="tsk_rfnormrank_m2",
                                       backend=sds_rfnormrank_m2, target="B8")

tasks_list_m2 <- list(tsk_ods_m2, tsk_rfsample_m2, tsk_rfnorm_m2, tsk_rfnormrank_m2)

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
mlr3misc::map(as.data.table(bm_model2)$learner, "model")

# step7: save bm_model as rds
saveRDS(bm_model2, './SyntheticData/Yue/syn2_rf/bm_rf_model2.rds')
# score_multinom_m1_ods <- sum(data.frame(bm_model1$score(msr("classif.acc"))[learner_id == 'classif.multinom', ][task_id == "tsk_ods_m1", ])["classif.acc"])/3
# score_multinom_m1_ods <- sum(data.frame(bm_model1$score(msr("classif.acc"))[learner_id == 'classif.multinom', ][task_id == "tsk_ods_m1", ])["classif.acc"])/3

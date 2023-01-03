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
#                           Exp-3
#--------------------- method = bag ----------------------------#
#----------------------------------------------------------------#

# now we start with the cart group by looping the saved .rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder <- "./SyntheticData/Yue/syn3_bag"
files <- list.files(folder, pattern = ".rda$")

syn_bag_models <- Map(rda2list, file.path(folder, files))
names(syn_bag_models) <- tools::file_path_sans_ext(files)

# we dataframe the lists
# bag sample
bag_sample_0802 <- data.frame(syn_bag_models$bag0802_sample_syn)
bag_sample_0803 <- data.frame(syn_bag_models$bag0803_sample_syn)
bag_sample_0804 <- data.frame(syn_bag_models$bag0804_sample_syn)
bag_sample_0805 <- data.frame(syn_bag_models$bag0805_sample_syn)
bag_sample_0806 <- data.frame(syn_bag_models$bag0806_sample_syn)
bag_sample_0807 <- data.frame(syn_bag_models$bag0807_sample_syn)
bag_sample_0808 <- data.frame(syn_bag_models$bag0808_sample_syn)
names(bag_sample_0802) <- sub('^syn.', '', names(bag_sample_0802))
names(bag_sample_0803) <- sub('^syn.', '', names(bag_sample_0803))
names(bag_sample_0804) <- sub('^syn.', '', names(bag_sample_0804))
names(bag_sample_0805) <- sub('^syn.', '', names(bag_sample_0805))
names(bag_sample_0806) <- sub('^syn.', '', names(bag_sample_0806))
names(bag_sample_0807) <- sub('^syn.', '', names(bag_sample_0807))
names(bag_sample_0808) <- sub('^syn.', '', names(bag_sample_0808))
# first try change them to char from var B2 to E6 (51-54)
str(bag_sample_0802)  # 54, E6
str(bag_sample_0803)  # 51-54
str(bag_sample_0804)  # 54, E6
str(bag_sample_0805)  # 54, E6
str(bag_sample_0806)  # 54, E6
str(bag_sample_0807)  # 54, E6
str(bag_sample_0808)  # 54, E6
col_names <- names(bag_sample_0803)[51:54]
bag_sample_0803[col_names] <- lapply(bag_sample_0803[col_names], as.character)

bag_sample_0803$B2[bag_sample_0803$B2 == "1"] <- "-1"
bag_sample_0803$B2[bag_sample_0803$B2 == "2"] <- "-99"
bag_sample_0803$B2[bag_sample_0803$B2 == "3"] <- "[0, 1)"
bag_sample_0803$B2[bag_sample_0803$B2 == "4"] <- "[1, 3)"
bag_sample_0803$B4[bag_sample_0803$B4 == "1"] <- "-99"
bag_sample_0803$B4[bag_sample_0803$B4 == "2"] <- "[0, 1)"
bag_sample_0803$B4[bag_sample_0803$B4 == "3"] <- "[1, 5)"
bag_sample_0803$E5[bag_sample_0803$E5 == "1"] <- "-99"
bag_sample_0803$E5[bag_sample_0803$E5 == "2"] <- "[0, 1)"
bag_sample_0803$E5[bag_sample_0803$E5 == "3"] <- "[1, 2)"

bag_sample_0802["E6"] <- lapply(bag_sample_0802["E6"], as.character)
bag_sample_0804["E6"] <- lapply(bag_sample_0804["E6"], as.character)
bag_sample_0805["E6"] <- lapply(bag_sample_0805["E6"], as.character)
bag_sample_0806["E6"] <- lapply(bag_sample_0806["E6"], as.character)
bag_sample_0807["E6"] <- lapply(bag_sample_0807["E6"], as.character)
bag_sample_0808["E6"] <- lapply(bag_sample_0808["E6"], as.character)

bag_sample_sds <- bind_rows(bag_sample_0802, bag_sample_0803, bag_sample_0804, bag_sample_0805,
                           bag_sample_0806, bag_sample_0807, bag_sample_0808)
table(bag_sample_sds$B2)  # -1    -99 [0, 1) [1, 3)
table(bag_sample_sds$B4)  # -99 [0, 1) [1, 5)
table(bag_sample_sds$E5)  # -99 [0, 1) [1, 2)
table(bag_sample_sds$E6)  # 1     2

# bag norm
bag_norm_0802 <- data.frame(syn_bag_models$bag0802_norm_syn)
bag_norm_0803 <- data.frame(syn_bag_models$bag0803_norm_syn)
bag_norm_0804 <- data.frame(syn_bag_models$bag0804_norm_syn)
bag_norm_0805 <- data.frame(syn_bag_models$bag0805_norm_syn)
bag_norm_0806 <- data.frame(syn_bag_models$bag0806_norm_syn)
bag_norm_0807 <- data.frame(syn_bag_models$bag0807_norm_syn)
bag_norm_0808 <- data.frame(syn_bag_models$bag0808_norm_syn)

names(bag_norm_0802) <- sub('^syn.', '', names(bag_norm_0802))
names(bag_norm_0803) <- sub('^syn.', '', names(bag_norm_0803))
names(bag_norm_0804) <- sub('^syn.', '', names(bag_norm_0804))
names(bag_norm_0805) <- sub('^syn.', '', names(bag_norm_0805))
names(bag_norm_0806) <- sub('^syn.', '', names(bag_norm_0806))
names(bag_norm_0807) <- sub('^syn.', '', names(bag_norm_0807))
names(bag_norm_0808) <- sub('^syn.', '', names(bag_norm_0808))
table(bindori_dataset_threshold_chr$B2)
table(bag_norm_0802$B2)  # -1    -99 [0, 1) [1, 3)
table(bag_norm_0802$B4)  # -99 [0, 1) [1, 5)
table(bag_norm_0802$E5)  # -99 [0, 1) [1, 2)
table(bag_norm_0802$E6)  # 0     1     2     3
table(bag_norm_0803$B2)  #  -1    -99 [0, 1) [1, 3)
table(bag_norm_0803$B4)  # -99 [0, 1) [1, 5)
table(bag_norm_0803$E5)  # -99 [0, 1) [1, 2)
table(bag_norm_0803$E6)  # 0     1     2     3
table(bag_norm_0804$B2)  # -1    -99 [0, 1) [1, 3)
table(bag_norm_0804$B4)  # -99 [0, 1) [1, 5)
table(bag_norm_0804$E5)  # -99 [0, 1) [1, 2)
table(bag_norm_0804$E6)  # 0     1     2     3
table(bag_norm_0805$B2)  # -99 [0, 1) [1, 3) missing -1
table(bag_norm_0805$B4)  # -99 [0, 1) [1, 5)
table(bag_norm_0805$E5)  # -99 [0, 1) [1, 2)
table(bag_norm_0805$E6)  # 0     1     2     3
table(bag_norm_0806$B2)  # -99 [0, 1) [1, 3) missing -1
table(bag_norm_0806$B4)  # -99 [0, 1) [1, 5)
table(bag_norm_0806$E5)  # -99 [0, 1) [1, 2)
table(bag_norm_0806$E6)  # 0     1     2     3
table(bag_norm_0807$B2)  # -99 [0, 1) [1, 3) missing -1
table(bag_norm_0807$B4)  # -99 [0, 1) [1, 5)
table(bag_norm_0807$E5)  # -99 [0, 1) [1, 2)
table(bag_norm_0807$E6)  # 0     1     2     3
table(bag_norm_0808$B2)  # -99 [0, 1) [1, 3) missing -1
table(bag_norm_0808$B4)  # -99 [0, 1) [1, 5)
table(bag_norm_0808$E5)  # -99 [0, 1) [1, 2)
table(bag_norm_0808$E6)  # 0     1     2     3

col_names <- names(bag_norm_0802)[54]
bag_norm_0802[col_names] <- lapply(bag_norm_0802[col_names], as.character)
bag_norm_0803[col_names] <- lapply(bag_norm_0803[col_names], as.character)
bag_norm_0804[col_names] <- lapply(bag_norm_0804[col_names], as.character)
bag_norm_0805[col_names] <- lapply(bag_norm_0805[col_names], as.character)
bag_norm_0806[col_names] <- lapply(bag_norm_0806[col_names], as.character)
bag_norm_0807[col_names] <- lapply(bag_norm_0807[col_names], as.character)
bag_norm_0808[col_names] <- lapply(bag_norm_0808[col_names], as.character)
bag_norm_sds <- bind_rows(bag_norm_0802, bag_norm_0803, bag_norm_0804, bag_norm_0805,
                          bag_norm_0806, bag_norm_0807, bag_norm_0808)
# check whether there are conflicts in B2, B4, E5 and E6
table(bag_norm_sds$B2)
table(bag_norm_sds$B4)
table(bag_norm_sds$E5)
table(bag_norm_sds$E6)
str(bag_norm_sds)

# bag normrank
bag_normrank_0802 <- data.frame(syn_bag_models$bag0802_normrank_syn)
bag_normrank_0803 <- data.frame(syn_bag_models$bag0803_normrank_syn)
bag_normrank_0804 <- data.frame(syn_bag_models$bag0804_normrank_syn)
bag_normrank_0805 <- data.frame(syn_bag_models$bag0805_normrank_syn)
bag_normrank_0806 <- data.frame(syn_bag_models$bag0806_normrank_syn)
bag_normrank_0807 <- data.frame(syn_bag_models$bag0807_normrank_syn)
bag_normrank_0808 <- data.frame(syn_bag_models$bag0808_normrank_syn)
names(bag_normrank_0802) <- sub('^syn.', '', names(bag_normrank_0802))
names(bag_normrank_0803) <- sub('^syn.', '', names(bag_normrank_0803))
names(bag_normrank_0804) <- sub('^syn.', '', names(bag_normrank_0804))
names(bag_normrank_0805) <- sub('^syn.', '', names(bag_normrank_0805))
names(bag_normrank_0806) <- sub('^syn.', '', names(bag_normrank_0806))
names(bag_normrank_0807) <- sub('^syn.', '', names(bag_normrank_0807))
names(bag_normrank_0808) <- sub('^syn.', '', names(bag_normrank_0808))
table(bag_normrank_0802$B2)  # -1    -99 [0, 1) [1, 3)
table(bag_normrank_0802$B4)  # -99 [0, 1) [1, 5)
table(bag_normrank_0802$E5)  # -99 [0, 1) [1, 2)
table(bag_normrank_0802$E6)  # 1     2
table(bag_normrank_0803$B2)  # -1    -99 [0, 1) [1, 3)
table(bag_normrank_0803$B4)  # -99 [0, 1) [1, 5) 
table(bag_normrank_0803$E5)  # -99 [0, 1) [1, 2)
table(bag_normrank_0803$E6)  # 1     2
table(bag_normrank_0804$B2)  # -1  -99 [0, 1) [1, 3)
table(bag_normrank_0804$B4)  # -99 [0, 1) [1, 5)
table(bag_normrank_0804$E5)  # -99 [0, 1) [1, 2)
table(bag_normrank_0804$E6)  # 1    2
table(bag_normrank_0805$B2)  # -99 [0, 1) [1, 3), missing -1
table(bag_normrank_0805$B4)  # -99 [0, 1) [1, 5)
table(bag_normrank_0805$E5)  # -99 [0, 1) [1, 2)
table(bag_normrank_0805$E6)  # 1     2
table(bag_normrank_0806$B2)  # -99 [0, 1) [1, 3), missing -1
table(bag_normrank_0806$B4)  # -99 [0, 1) [1, 5)
table(bag_normrank_0806$E5)  # -99 [0, 1) [1, 2)
table(bag_normrank_0806$E6)  # 1     2
table(bag_normrank_0807$B2)  # -99 [0, 1) [1, 3), missing -1
table(bag_normrank_0807$B4)  # -99 [0, 1) [1, 5)
table(bag_normrank_0807$E5)  # -99 [0, 1) [1, 2)
table(bag_normrank_0807$E6)  # 1     2
table(bag_normrank_0808$B2)  # -99 [0, 1) [1, 3), missing -1
table(bag_normrank_0808$B4)  # -99 [0, 1) [1, 5)
table(bag_normrank_0808$E5)  # -99 [0, 1) [1, 2)
table(bag_normrank_0808$E6)  # 1     2

col_names <- names(bag_normrank_0807)[54]
bag_normrank_0802[col_names] <- lapply(bag_normrank_0802[col_names], as.character)
bag_normrank_0803[col_names] <- lapply(bag_normrank_0803[col_names], as.character)
bag_normrank_0804[col_names] <- lapply(bag_normrank_0804[col_names], as.character)
bag_normrank_0805[col_names] <- lapply(bag_normrank_0805[col_names], as.character)
bag_normrank_0806[col_names] <- lapply(bag_normrank_0806[col_names], as.character)
bag_normrank_0807[col_names] <- lapply(bag_normrank_0807[col_names], as.character)
bag_normrank_0808[col_names] <- lapply(bag_normrank_0808[col_names], as.character)
bag_normrank_sds <- bind_rows(bag_normrank_0802, bag_normrank_0803, bag_normrank_0804, bag_normrank_0805,
                              bag_normrank_0806, bag_normrank_0807, bag_normrank_0808)
table(bag_normrank_sds$B2)  # -1    -99 [0, 1) [1, 3)
table(bag_normrank_sds$B4)  # -99 [0, 1) [1, 5)
table(bag_normrank_sds$E5)  # -99 [0, 1) [1, 2)
table(bag_normrank_sds$E6)  # 1      2, only E6 needs to be factored

str(bag_sample_sds)  # B2 to E6, chr
str(bag_norm_sds)   # E6 chr
str(bag_normrank_sds)  # E6 chr

# we will have to reverse the integer back to the factor

# additionally, for sample, norm and norm, we reverse the integer back to interval
table(bag_sample_sds$B2, bag_norm_sds$B2) # B2 is safe
table(bag_sample_sds$B2, bag_normrank_sds$B2)
table(bindori_dataset_threshold_chr$B2)
table(bag_sample_sds$B2)

table(bag_sample_sds$B4, bag_norm_sds$B4) # B4 is safe
table(bag_sample_sds$B4, bag_normrank_sds$B4)
table(bindori_dataset_threshold_chr$B4)

table(bag_sample_sds$E5, bag_norm_sds$E5) # E5 is safe
table(bag_sample_sds$E5, bag_normrank_sds$E5)
table(bindori_dataset_threshold_chr$E5)

table(bag_sample_sds$E6, bag_norm_sds$E6) # exclude E6 for bag-norm
table(bag_sample_sds$E6, bag_normrank_sds$E6)
table(bindori_dataset_threshold_chr$E6)
bag_sample_sds$E6[bag_sample_sds$E6 == "1"] <- "-99"
bag_sample_sds$E6[bag_sample_sds$E6 == "2"] <- "[0, 9)"
bag_norm_sds$E6[bag_norm_sds$E6 == "1"] <- "-99"
bag_norm_sds$E6[bag_norm_sds$E6 == "2"] <- "[0, 9)"
bag_normrank_sds$E6[bag_normrank_sds$E6 == "1"] <- "-99"
bag_normrank_sds$E6[bag_normrank_sds$E6 == "2"] <- "[0, 9)"

str(bag_sample_sds)
# change var "B2", "B4", "E5", "E6" to factor type
cols_factor <- c("B2", "B4", "E5", "E6")
bag_sample_sds[cols_factor] <- lapply(bag_sample_sds[cols_factor], factor)
bag_norm_sds["E6"] <- lapply(bag_norm_sds["E6"], factor)
bag_normrank_sds["E6"] <- lapply(bag_normrank_sds["E6"], factor)

str(bag_sample_sds)
str(bag_norm_sds)
str(bag_normrank_sds)
table(bag_norm_sds$E6)
table(bag_normrank_sds$E6)
# data  export
#-----------------------------------------------
export_path <- "./SyntheticData/Yue/syn3_bag"
bagsample_sds <- "bag_sample_syn.rda"
bagnorm_sds <- "bag_norm_syn.rda"
bagnormrank_sds <- "bag_normrank_syn.rda"

save(bag_sample_sds, file=paste(c(export_path, bagsample_sds), 
                                    collapse="/"))
save(bag_norm_sds, file=paste(c(export_path, bagnorm_sds), 
                                    collapse="/"))
save(bag_normrank_sds, file=paste(c(export_path, bagnormrank_sds), 
                                    collapse="/"))

# is it necessary to add more levels to the dataset
levels(bindori_dataset_threshold_chr$E6) <- c(levels(bindori_dataset_threshold_chr$E6), "0", "3")
levels(bag_sample_sds$E6) <- c(levels(bag_sample_sds$E6), "0", "3")
levels(bag_normrank_sds$E6) <- c(levels(bag_normrank_sds$E6), "0", "3")

#------------------Evaluating the utility of the bag_syn sds------------------

#=========(1). one-way marginals using compare()
# try with 54-1 variables firstly, we subset the original ods first
ncol(bag_sample_sds)
bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(names(bag_sample_sds))
# cuz the compare function cannot tackle with factor type variables, we delete them for evaluation
bindori_select_vars <- bindori_dataset_threshold_chr
bagsample_select_vars <- bag_sample_sds
bagnorm_select_vars <- bag_norm_sds
bagnormrank_select_vars <- bag_normrank_sds

ncol(bagnorm_select_vars)

#******************* for bag sample
compare_plots_bagsample<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_bagsample[[i]] <- compare(object = data.frame(Pdata = bagsample_select_vars[i]),
                                         data = data.frame(Pdata = bindori_select_vars[i]),
                                         vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                         msel = NULL, stat = "percents", breaks = 10,
                                         nrow = 2, ncol = 2, rel.size.x = 1,
                                         utility.stats = c("pMSE", "S_pMSE"),
                                         cols = c("#1A3C5A","#4187BF"),
                                         plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn3_bag/oneway_compare_bagsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_bagsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_bagsample <- c()
SpMSE_list_bagsample <- c()
for (i in 1:54) {
  pMSE_list_bagsample <- append(pMSE_list_bagsample, compare_plots_bagsample[[i]]$tab.utility[1])
  SpMSE_list_bagsample <- append(SpMSE_list_bagsample, compare_plots_bagsample[[i]]$tab.utility[2])
}

#create data frame
df_utility_bagsample <- data.frame(vars_list=colnames(bagsample_select_vars),
                                  pMSE=pMSE_list_bagsample,
                                  S_pMSE=SpMSE_list_bagsample)

write_utility_bagsample <- "./SyntheticData/Yue/syn3_bag/oneway_utility_bagsample.csv"
write.csv(df_utility_bagsample, write_utility_bagsample, row.names=FALSE)

vars2show_bagsample <- df_utility_bagsample[df_utility_bagsample[, "S_pMSE"]<10, ][1]

nrow(vars2show_bagsample)  # there are 37 in total for bagsample


#******************* for bag norm
compare_plots_bagnorm<- c()

for (i in 1:length(bagnorm_select_vars)) {
  cat(colnames(bindori_select_vars[1:54][i]), "\n")  # print the var string under analysis
  
  compare_plots_bagnorm[[i]] <- compare(object = data.frame(Pdata = bagnorm_select_vars[i]),
                                       data = data.frame(Pdata = bindori_select_vars[1:54][i]),
                                       vars = c(colnames(bagnorm_select_vars[i])), cont.na = NULL,
                                       msel = NULL, stat = "percents", breaks = 10,
                                       nrow = 2, ncol = 2, rel.size.x = 1,
                                       utility.stats = c("pMSE", "S_pMSE"),
                                       cols = c("#1A3C5A","#4187BF"),
                                       plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn3_bag/oneway_compare_bagnorm.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_bagnorm[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_bagnorm <- c()
SpMSE_list_bagnorm <- c()
for (i in 1:54) {
  pMSE_list_bagnorm <- append(pMSE_list_bagnorm, compare_plots_bagnorm[[i]]$tab.utility[1])
  SpMSE_list_bagnorm <- append(SpMSE_list_bagnorm, compare_plots_bagnorm[[i]]$tab.utility[2])
}

#create data frame
df_utility_bagnorm <- data.frame(vars_list=colnames(bagnorm_select_vars),
                                pMSE=pMSE_list_bagnorm,
                                S_pMSE=SpMSE_list_bagnorm)

write_utility_bagnorm <- "./SyntheticData/Yue/syn3_bag/oneway_utility_bagnorm.csv"
write.csv(df_utility_bagnorm, write_utility_bagnorm, row.names=FALSE)

vars2show_bagnorm <- df_utility_bagnorm[df_utility_bagnorm[, "S_pMSE"]<10, ][1]

nrow(vars2show_bagnorm)  # there are 38 in total for bagnorm


#******************* for bag normrank
compare_plots_bagnormrank<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_bagnormrank[[i]] <- compare(object = data.frame(Pdata = bagnormrank_select_vars[i]),
                                           data = data.frame(Pdata = bindori_select_vars[i]),
                                           vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                           msel = NULL, stat = "percents", breaks = 10,
                                           nrow = 2, ncol = 2, rel.size.x = 1,
                                           utility.stats = c("pMSE", "S_pMSE"),
                                           cols = c("#1A3C5A","#4187BF"),
                                           plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn3_bag/oneway_compare_bagnormrank.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_bagnormrank[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_bagnormrank <- c()
SpMSE_list_bagnormrank <- c()
for (i in 1:54) {
  pMSE_list_bagnormrank <- append(pMSE_list_bagnormrank, compare_plots_bagnormrank[[i]]$tab.utility[1])
  SpMSE_list_bagnormrank <- append(SpMSE_list_bagnormrank, compare_plots_bagnormrank[[i]]$tab.utility[2])
}

#create data frame
df_utility_bagnormrank <- data.frame(vars_list=colnames(bagnormrank_select_vars),
                                    pMSE=pMSE_list_bagnormrank,
                                    S_pMSE=SpMSE_list_bagnormrank)

write_utility_bagnormrank <- "./SyntheticData/Yue/syn3_bag/oneway_utility_bagnormrank.csv"
write.csv(df_utility_bagnormrank, write_utility_bagnormrank, row.names=FALSE)

vars2show_bagnormrank <- df_utility_bagnormrank[df_utility_bagnormrank[, "S_pMSE"]<10, ][1]

nrow(vars2show_bagnormrank)  # there are 37 in total for bag normrank

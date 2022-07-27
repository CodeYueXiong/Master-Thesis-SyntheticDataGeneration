# install the required R packages
# install.packages("readr")
# install.packages("vroom")
# install.packages("tidyverse")
# install.packages("arsenal")
# install.packages("reshape2")
# install.packages("synthpop")

library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(synthpop)

############################# Step1: Data Preparation ##############################
# In step1, we aim to read in the original datasets and the synthetic dataset
# while aligning the variables especially set in the syn dataset.
# Attempting to compare the two datasets, we should also comcatenate the original datasets
# vertically with variables all set as the same.

# set the working directory
setwd("F:/Master-Thesis-DifferentialPrivacy")
# read in the synthetic dataset
syn_data <- read.csv(file = "./syn_2020-08-02_2020-08-08.csv")
head(syn_data)
# syn_data <- read_csv("./syn_2020-08-02_2020-08-08.csv", show_col_types = FALSE)
# rename "sample weight" to "weight" to avoid comflicts
colnames(syn_data)[colnames(syn_data) == "sample_weight"] <- "weight"

# columns to be included from the original dataset
cols_list <- colnames(syn_data)

# initialize an empty dataset list
ori_dataset <- list()

for (i in 1:7){
ori_dataset[[i]] <- vroom(list.files(pattern = "*_full.csv$")[i],
                    show_col_types = FALSE) %>%
                    select(all_of(cols_list))
}

dim(ori_dataset[[2]])[2] == dim(syn_data)[2]
# check whether 2 dimensions coincide with each other

# bind the original datasets from 0802 to 0808 vertically
bindori_dataset <- bind_rows(ori_dataset)
bindori_dataset <- as.data.frame(bindori_dataset)
dim(bindori_dataset)
dim(syn_data)

syn_data$
# check gpdr countries and only do the alignment
gpdr_countries_data <- NA
gpdr_countries_data <- read.csv(file = "./gpdr.csv", sep = ",")
head(gpdr_countries_data$Country_GID, n = 6L)
row.names(bindori_dataset$GID_0)
rownames(bindori_dataset$GID_0)
# now filter the binded original datasets and syn dataset with Region_GID specified
country_name <- unique(as.character(gpdr_countries_data$Country_GID))
length(country_name)
length(unique(bindori_dataset$GID_0))
bindori_dataset_filtered <- bindori_dataset %>%
                                        filter(GID_0 %in% country_name)
length(unique(bindori_dataset_filtered$GID_0))

syn_data_filtered <- syn_data %>%
                          filter(GID_0 %in% country_name)
length(unique(syn_data_filtered$GID_0))

####################### Step2: Evaluating the utility of the syn data #######################
# In step2, we try to evaluate the utility of the synthetic dataset with one-way marginal
# and two-way marginal measures. (1). As for the one-way utility, the syn dataset is measured with
# the compare plots and pMSE/S_pMSE. (2). And for the two-way utility, the synthetic dataset is
# evaluated with the utility tables which takes up a heatmap fashion/manner.

# subset some example columns and try plotting the compare histograms
# these are symptoms variables
symptoms <- c("B3","B4","B1_1","B1_2","B1_3","B1_4","B1_5","B1_6","B1_7",
              "B1_8","B1_9","B1_10","B1_12","B1_13",
              "B1b_x1","B1b_x2","B1b_x3","B1b_x4","B1b_x5","B1b_x6","B1b_x7",
              "B1b_x8","B1b_x9","B1b_x10","B1b_x12","B1b_x13","B2")

# these are testing variables
testing <- c("B7")

# Columns `B0`, `B8a`, `B15_1`, `B15_2`, `B15_3`, etc. don't exist

ori_dataset_symptoms <- as.data.frame(bindori_dataset_filtered[, symptoms])
syn_dataset_symptoms <- as.data.frame(syn_data_filtered[, symptoms])

ori_dataset_testing <- as.data.frame(bindori_dataset_filtered$B7)
syn_dataset_testing <- as.data.frame(syn_data_filtered$B7)

### (1). one-way marginals using compare()

## for var B3 -> symptoms
compare(object = data.frame(B3 = syn_dataset_symptoms$B3),
        data = data.frame(B3 = ori_dataset_symptoms$B3),
        vars = c("B3"), cont.na = NULL,
        msel = NULL, stat = "percents", breaks = 20,
        nrow = 2, ncol = 2, rel.size.x = 1,
        utility.stats = c("pMSE", "S_pMSE"),
        cols = c("#1A3C5A","#4187BF"),
        plot = TRUE, table = TRUE)

## for var B1_1 -> symptoms
compare(object = data.frame(B1_1 = syn_dataset_symptoms$B1_1),
        data = data.frame(B1_1 = ori_dataset_symptoms$B1_1),
        vars = "B1_1", cont.na = NULL,
        msel = NULL, stat = "percents", breaks = 20,
        nrow = 2, ncol = 2, rel.size.x = 1,
        utility.stats = c("pMSE", "S_pMSE"),
        cols = c("#1A3C5A","#4187BF"),
        plot = TRUE, table = FALSE)

## for var B7 -> testing
compare(object = data.frame(B7 = syn_data_filtered$B7),
        data = data.frame(B7 = bindori_dataset_filtered$B7),
        vars = "B7", cont.na = NULL,
        msel = NULL, stat = "percents", breaks = 20,
        nrow = 2, ncol = 2, rel.size.x = 1,
        utility.stats = c("pMSE", "S_pMSE"),
        cols = c("#1A3C5A","#4187BF"),
        plot = TRUE, table = FALSE)

## for var D1 -> module B
compare(object = data.frame(D1 = syn_data_filtered$D1),
        data = data.frame(D1 = bindori_dataset_filtered$D1),
        vars = "D1", cont.na = NULL,
        msel = NULL, stat = "percents", breaks = 20,
        nrow = 2, ncol = 2, rel.size.x = 1,
        utility.stats = c("pMSE", "S_pMSE"),
        cols = c("#1A3C5A","#4187BF"),
        plot = TRUE, table = FALSE)

## for var D2 -> module B
compare(object = data.frame(D2 = syn_data_filtered$D2),
        data = data.frame(D2 = bindori_dataset_filtered$D2),
        vars = "D2", cont.na = NULL,
        msel = NULL, stat = "percents", breaks = 20,
        nrow = 2, ncol = 2, rel.size.x = 1,
        utility.stats = c("pMSE", "S_pMSE"),
        cols = c("#1A3C5A","#4187BF"),
        plot = TRUE, table = FALSE)

## for var E2 -> demographics
compare(object = data.frame(E2 = syn_data_filtered$E2),
        data = data.frame(E2 = bindori_dataset_filtered$E2),
        vars = "E2", cont.na = NULL,
        msel = NULL, stat = "percents", breaks = 20,
        nrow = 2, ncol = 2, rel.size.x = 1,
        utility.stats = c("pMSE", "S_pMSE"),
        cols = c("#1A3C5A","#4187BF"),
        plot = TRUE, table = FALSE)

## for var E3 -> demographics
compare(object = data.frame(E3 = syn_data_filtered$E3),
        data = data.frame(E3 = bindori_dataset_filtered$E3),
        vars = "E3", cont.na = NULL,
        msel = NULL, stat = "percents", breaks = 20,
        nrow = 2, ncol = 2, rel.size.x = 1,
        utility.stats = c("pMSE", "S_pMSE"),
        cols = c("#1A3C5A","#4187BF"),
        plot = TRUE, table = FALSE)

utility.gen(object = syn_data,
            data = bindori_dataset,
            not.synthesised = NULL,
            cont.na = NULL,
            print.stats = c("pMSE", "S_pMSE"))  # do not know why the s_pMSE is quite large
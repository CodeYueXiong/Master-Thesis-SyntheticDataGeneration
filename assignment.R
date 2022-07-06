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
syn_data <- read_csv("./syn_2020-08-02_2020-08-08.csv", col_types = NULL)
# rename "sample weight" to "weight" to avoid comflicts
colnames(syn_data)[colnames(syn_data) == "sample_weight"] <- "weight"

# columns to be included from the original dataset
cols_list <- colnames(syn_data)

# datentest <- vroom(list.files(pattern = "*_full.csv$")[1],
#                               show_col_types = FALSE) %>%
#              select(all_of(cols_list))
# dim(datentest)

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
bindori_dataset <- data.frame(bindori_dataset)
dim(bindori_dataset)

####################### Step2: Evaluating the utility of the syn data #######################
# In step2, we try to evaluate the utility of the synthetic dataset with one-way marginal
# and two-way marginal measures. (1). As for the one-way utility, the syn dataset is measured with
# the compare plots and pMSE/S_pMSE. (2). And for the two-way utility, the synthetic dataset is
# evaluated with the utility tables which takes up a heatmap fashion/manner.

# subset some example columns and try plotting the histograms
symptoms <- c("B3","B4","B1_1","B1_2","B1_3","B1_4","B1_5","B1_6","B1_7",
              "B1_8","B1_9","B1_10","B1_12","B1_13",
              "B1b_x1","B1b_x2","B1b_x3","B1b_x4","B1b_x5","B1b_x6","B1b_x7",
              "B1b_x8","B1b_x9","B1b_x10","B1b_x12","B1b_x13","B2")


testing <- c("B0","B7","B8a",
             "B15_1","B15_2",
             "B15_3","B15_4",
             "B15_5","B15_6","B15_7")

testori_dataset <- bindori_dataset[, symptoms_1]
testori_dataset <- data.frame(testori_dataset)
testsyn_dataset <- data.frame(syn_data[, symptoms_1])

### (1). one-way marginals using compare()
synxxx <- syn.try.passive

typeof(bindori_dataset$B3)
head(bindori_dataset$B3, n=10)
compare_symptoms <- compare(data = as.factor(bindori_dataset$B3),
                            object = as.factor(syn_data$B3))

## for var B3
compare(object = data.frame(B3 = testsyn_dataset$B3), 
        data = data.frame(B3 = testori_dataset$B3), 
        vars = c("B3"), cont.na = NULL,
        msel = NULL, stat = "percents", breaks = 20,
        nrow = 2, ncol = 2, rel.size.x = 1,
        utility.stats = c("pMSE", "S_pMSE"),
        cols = c("#1A3C5A","#4187BF"),
        plot = TRUE, table = TRUE)

## for var B4
length(testsyn_dataset$B3)
length(testori_dataset$B3)
length(testsyn_dataset$B4)
length(testori_dataset$B4)

compare(object = data.frame(B4 = testsyn_dataset$B4), 
        data = data.frame(B4 = testori_dataset$B4), 
        vars = "B4", cont.na = NULL,
        msel = NULL, stat = "percents", breaks = 20,
        nrow = 2, ncol = 2, rel.size.x = 1,
        utility.stats = c("pMSE", "S_pMSE"),
        cols = c("#1A3C5A","#4187BF"),
        plot = TRUE, table = FALSE)

head(testori_dataset)
# plot the distribution of the synthetic dataset
var_testori_dataset <- melt(testori_dataset)  # break down into var variables


var_testori_dataset$variable

ggplot(var_testori_dataset, aes(x=value, fill=variable)) +
       geom_histogram()+
       facet_grid(variable~.)


utility.gen(object = syn_data,
            data = bindori_dataset,
            not.synthesised = NULL,
            cont.na = NULL,
            print.stats = c("pMSE", "S_pMSE"))  # do not know why the s_pMSE is quite large
 
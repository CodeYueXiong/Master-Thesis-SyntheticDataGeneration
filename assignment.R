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

# read in the synthetic dataset
syn_data <- read_csv("./syn_2020-08-02_2020-08-08.csv", show_col_types = FALSE)
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

# subset some example columns and try plotting the histograms
symptoms <- c("B3","B4","B1_1","B1_2","B1_3","B1_4","B1_5","B1_6","B1_7",
              "B1_8","B1_9","B1_10","B1_12","B1_13",
              "B1b_x1","B1b_x2","B1b_x3","B1b_x4","B1b_x5","B1b_x6","B1b_x7",
              "B1b_x8","B1b_x9","B1b_x10","B1b_x12","B1b_x13","B2")

testing <- c("B0","B7","B8a",
             "B15_1","B15_2",
             "B15_3","B15_4",
             "B15_5","B15_6","B15_7")

testori_dataset <- bindori_dataset[, testing]

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
            print.stats = c("pMSE", "S_pMSE"))

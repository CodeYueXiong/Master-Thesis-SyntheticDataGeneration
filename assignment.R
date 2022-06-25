# install  the required R packages
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
symptoms <- c("B3", "B4", "B1_1", "B1_2")

testori_dataset <- bindori_dataset[, symptoms]
head(testori_dataset)
# plot the distribution of the synthetic dataset
var_testori_dataset <- melt(testori_dataset)  # break down into var variables
 

var_bindori_dataset$variable

ggplot(var_testori_dataset, aes(x=value, fill=variable)) +
       geom_histogram()+
       facet_grid(variable~.)


utility.gen(object = syn_data,
            data = bindori_dataset,
            not.synthesised = NULL,
            cont.na = NULL,
            print.stats = c("pMSE", "S_pMSE"))

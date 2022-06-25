# install  the required R packages
install.packages("readr")
install.packages("vroom")
install.packages("tidyverse")
install.packages("arsenal")
install.packages("reshape2")
install.packages("synthpop")

library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(synthpop)

# read in the original data
ori_data_0802 <- read_csv("./ori_data/2020-08-02_full.csv")
ori_data_0803 <- read_csv("./ori_data/2020-08-03_full.csv")
ori_data_0804 <- read_csv("./ori_data/2020-08-04_full.csv")
ori_data_0805 <- read_csv("./ori_data/2020-08-05_full.csv")
ori_data_0806 <- read_csv("./ori_data/2020-08-06_full.csv")
ori_data_0807 <- read_csv("./ori_data/2020-08-07_full.csv")
ori_data_0808 <- read_csv("./ori_data/2020-08-08_full.csv")

# read in the synthetic dataset
syn_data <- read_csv("./syn_2020-08-02_2020-08-08.csv", show_col_types = FALSE)
listcols_syn_data <- list(colnames(syn_data))
listcols_ori_data <- list(colnames(ori_dataset[[1]]))

setdiff(listcols_syn_data, listcols_ori_data)
# columns to be included from the original dataset
cols_list <- colnames(syn_data)

datentest <- vroom(list.files(pattern = "*_full.csv$")[1],
                              show_col_types = FALSE) %>%
             select(any_of(cols_list))
# initialize an empty dataset list
ori_dataset <- list()

for (i in 1:7){
ori_dataset[[i]] <- vroom(list.files(pattern = "*.csv$")[i],
                    show_col_types = FALSE) %>%
                    select(any_of(cols_list))
}

dim(ori_dataset[[2]])
dim(syn_data)

# bind the original datasets from 0802 to 0808
bindori_dataset <- bind_rows(ori_dataset)
bindori_dataset <- data.frame(bindori_dataset)
dim(bindori_dataset)

# plot the distribution of the synthetic dataset
var_bindori_dataset <- melt(bindori_dataset)  # break down into var variables


var_bindori_dataset$variable
ggplo
ggplot(var_bindori_dataset[1:3], aes(x=value, fill=variable)) +
       geom_histogram(binwidth=10)+
       facet_grid(variable~.)


utility.gen(object = syn_data,
            data = bindori_dataset,
            not.synthesised = NULL,
            cont.na = NULL,
            print.stats = c("pMSE", "S_pMSE"))
 
# load the required packages
library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(synthpop)
library(ggplot2)
library(dbplyr)
library(data.table)
library(here)

# source(here::here("./SyntheticData/Yue/data_preprocess.R"))

# set the working directory
# wd <- "F:/Master-Thesis-DifferentialPrivacy"
wd <- "C:/Users/ru27req/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# load the preprocessed original data
load("bindori_dataset_preprocessed.rda")
# we have the dataframe here named as "bindori_dataset_threshold_chr"
str(bindori_dataset_threshold_chr)
# also, we need to subset those columns with constant inputs
cols_remove <- c("B13_1", "B13_2", "B13_3", "B13_4",
                 "B13_5", "B13_6", "B13_7",
                 "B14_1", "B14_2", "B14_3", "B14_4", "B14_5",
                 "D6_1", "D6_2", "D6_3", "F3_de")
ds_col_syn <- bindori_dataset_threshold_chr %>% select(-cols_remove)
cols_syn <- colnames(ds_col_syn)
######---------------synthetic data with synthpop-------------------######
# method 1: default
my.seed <- 17914709
sds.default <- syn(bindori_dataset_threshold_chr, seed = my.seed)

# method 2: default parametric
sds.default.para <- syn(bindori_dataset_threshold_chr, method = "parametric", seed = my.seed)
sds.default.para$method

# method 3: by simple random sampling
sds.sample <- syn(bindori_dataset_threshold_chr, method = "sample")

# method 4: by unordered polytomous regression
sds.upr <- syn(bindori_dataset_threshold_chr, method = "polyreg")
# method 5: by CART
sds.cart <- syn(bindori_dataset_threshold_chr, method = "cart")
# method 6: by Random Forest
sds.ranger <- syn(bindori_dataset_threshold_chr, method = "ranger")
# method 7: by bagging
sds.bag <- syn(bindori_dataset_threshold_chr, method = "bag")

sds.parametric <- syn(bindori_dataset_threshold, method = "parametric", seed = my.seed)

syn_cart <- syn(bindori_dataset_threshold, method = c("cart"), seed = my.seed)

# utility evaluation
# one-way marginals using compare()
compare_plots <- c()

for (i in 1:85) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots[[i]] <- compare(object = data.frame(Pdata = syn_select_vars[i]),
          data = data.frame(Pdata = bindori_select_vars[i]),
          vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
          msel = NULL, stat = "percents", breaks = 10,
          nrow = 2, ncol = 2, rel.size.x = 1,
          utility.stats = c("pMSE", "S_pMSE"),
          cols = c("#1A3C5A","#4187BF"),
          plot = TRUE, table = TRUE)
  
}

pMSE_list <- c()
S_pMSE_list <- c()
for (i in 1:85) {
  pMSE_list <- append(pMSE_list, compare_plots[[i]]$tab.utility[1])
  S_pMSE_list <- append(S_pMSE_list, compare_plots[[i]]$tab.utility[2])
  }

vars2show <- df_utility[df_utility[, "S_pMSE"]<10, ][1]

# two-way marginals with utility.tables()
utility.twoway <- utility.tables(object = data.frame(syn_select_vars), 
                                 data = data.frame(bindori_select_vars),
                                 tables = "twoway",
                                 tab.stats = c("pMSE", "S_pMSE"),
                                 plot.stat = "S_pMSE", plot = TRUE,
                                 print.tabs = TRUE)

# sdc using
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

source(here::here("data_preprocess.R"))

# set the working directory
wd <- "F:/Master-Thesis-DifferentialPrivacy"
setwd(wd)

file_path <- "./SyntheticData/Terrance/version_1/syn_k2_2020-08-02_2020-08-08.csv"
gpdr_file_path <- "F:/Master-Thesis-DifferentialPrivacy/gpdr.csv"

gpdr_dataset_list <- gpdr_region_preprocess(file_path,gpdr_file_path)
bindori_dataset_gpdr <- data.frame(gpdr_dataset_list$bindori_dataset_gpdr)

bindori_dataset_threshold <- threshold_preprocess(bindori_dataset_gpdr)

# synthetic data
my.seed <- 17914709
sds.default <- syn(bindori_dataset_threshold, seed = my.seed)

#sds.default

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
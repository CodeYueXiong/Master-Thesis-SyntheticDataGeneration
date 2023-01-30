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
# wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
wd = "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# then we load the required preprocessed datasets
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)


#----------------------------------------------------------------#
#                           Exp-7
#--------------------- Terrance's sds ----------------------------#
#----------------------------------------------------------------#
sds_filepath_version1 <- "./SyntheticData/Terrance/version_1/syn_k2_2020-08-02_2020-08-08-version1.csv"
sds_filepath_version2 <- "./SyntheticData/Terrance/version_2/syn_k2_2020-08-02_2020-08-08-version2.csv"

sds <- read.csv(file = file_path)
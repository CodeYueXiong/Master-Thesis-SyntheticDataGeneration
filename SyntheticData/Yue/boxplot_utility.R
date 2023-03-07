################################################################################
# -------------------- Boxplot of Utility Evaluation --------------------------#
# This script is meant for the plotting the utility of evaluation based on the synthetic    #
# datasets compared to the original dataset using boxplot      

# firstly, we load the required libraries
library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(synthpop)
library(ggplot2)
library(dbplyr)

# set the working directory
# wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
# wd = "/dss/dsshome1/0C/ru27req2/MA_Experiment_Data/Master-Thesis-DifferentialPrivacy"
# setwd(wd)
wd <- "/Users/roxy/Desktop/Master-Thesis-SyntheticDataGeneration"
setwd(wd)

# for cart group
sds_cartsample_filepath <- "./SyntheticData/Yue/syn1_cart/oneway_utility_cartsample.csv"
sds_cartnorm_filepath <- "./SyntheticData/Yue/syn1_cart/oneway_utility_cartnorm.csv"
sds_cartnormrank_filepath <- "./SyntheticData/Yue/syn1_cart/oneway_utility_cartnormrank.csv"

# for rf group
sds_rfsample_filepath <- "./SyntheticData/Yue/syn2_rf/oneway_utility_rfsample.csv"
sds_rfnorm_filepath <- "./SyntheticData/Yue/syn2_rf/oneway_utility_rfnorm.csv"
sds_rfnormrank_filepath <- "./SyntheticData/Yue/syn2_rf/oneway_utility_rfnormrank.csv"

# for bag group
sds_bagsample_filepath <- "./SyntheticData/Yue/syn3_bag/oneway_utility_bagsample.csv"
sds_bagnorm_filepath <- "./SyntheticData/Yue/syn3_bag/oneway_utility_bagnorm.csv"
sds_bagnormrank_filepath <- "./SyntheticData/Yue/syn3_bag/oneway_utility_bagnormrank.csv"

# for polyreg group
sds_polyregsample_filepath <- "./SyntheticData/Yue/syn4_polyreg/oneway_utility_polyregsample.csv"
sds_polyregnorm_filepath <- "./SyntheticData/Yue/syn4_polyreg/oneway_utility_polyregnorm.csv"
sds_polyregnormrank_filepath <- "./SyntheticData/Yue/syn4_polyreg/oneway_utility_polyregnormrank.csv"

# for norm group
sds_normsample_filepath <- "./SyntheticData/Yue/syn5_norm/oneway_utility_normsample.csv"
sds_normnorm_filepath <- "./SyntheticData/Yue/syn5_norm/oneway_utility_normnorm.csv"
sds_normnormrank_filepath <- "./SyntheticData/Yue/syn5_norm/oneway_utility_normnormrank.csv"

# for normrank group
sds_normranksample_filepath <- "./SyntheticData/Yue/syn6_normrank/oneway_utility_normranksample.csv"
sds_normranknorm_filepath <- "./SyntheticData/Yue/syn6_normrank/oneway_utility_normranknorm.csv"
sds_normranknormrank_filepath <- "./SyntheticData/Yue/syn6_normrank/oneway_utility_normranknormrank.csv"

# for terrance's group
sds_filepath_version1 <- "./SyntheticData/Yue/syn7_terrance/oneway_utility_terrance_version1.csv"
sds_filepath_version2 <- "./SyntheticData/Yue/syn7_terrance/oneway_utility_terrance_version2.csv"

sds_cartsample_utility <- read.csv(file = sds_cartsample_filepath)
sds_cartnorm_utility <- read.csv(file = sds_cartnorm_filepath)
sds_cartnormrank_utility <- read.csv(file = sds_cartnormrank_filepath)
# add the group column
sds_cartsample_utility$group <- "sds_cart_sample"
sds_cartnorm_utility$group <- "sds_cart_norm"
sds_cartnormrank_utility$group <- "sds_cart_normrank"

sds_rfsample_utility <- read.csv(file = sds_rfsample_filepath)
sds_rfnorm_utility <- read.csv(file = sds_rfnorm_filepath)
sds_rfnormrank_utility <- read.csv(file = sds_rfnormrank_filepath)
# add the group column
sds_rfsample_utility$group <- "sds_rf_sample"
sds_rfnorm_utility$group <- "sds_rf_norm"
sds_rfnormrank_utility$group <- "sds_rf_normrank"

sds_bagsample_utility <- read.csv(file = sds_bagsample_filepath)
sds_bagnorm_utility <- read.csv(file = sds_bagnorm_filepath)
sds_bagnormrank_utility <- read.csv(file = sds_bagnormrank_filepath)
# add the group column
sds_bagsample_utility$group <- "sds_bag_sample"
sds_bagnorm_utility$group <- "sds_bag_norm"
sds_bagnormrank_utility$group <- "sds_bag_normrank"

sds_polyregsample_utility <- read.csv(file = sds_polyregsample_filepath)
sds_polyregnorm_utility <- read.csv(file = sds_polyregnorm_filepath)
sds_polyregnormrank_utility <- read.csv(file = sds_polyregnormrank_filepath)
# add the group column
sds_polyregsample_utility$group <- "sds_polyreg_sample"
sds_polyregnorm_utility$group <- "sds_polyreg_norm"
sds_polyregnormrank_utility$group <- "sds_polyreg_normrank"

sds_normsample_utility <- read.csv(file = sds_normsample_filepath)
sds_normnorm_utility <- read.csv(file = sds_normnorm_filepath)
sds_normnormrank_utility <- read.csv(file = sds_normnormrank_filepath)
# add the group column
sds_normsample_utility$group <- "sds_norm_sample"
sds_normnorm_utility$group <- "sds_norm_norm"
sds_normnormrank_utility$group <- "sds_norm_normrank"

sds_normranksample_utility <- read.csv(file = sds_normranksample_filepath)
sds_normranknorm_utility <- read.csv(file = sds_normranknorm_filepath)
sds_normranknormrank_utility <- read.csv(file = sds_normranknormrank_filepath)
# add the group column
sds_normranksample_utility$group <- "sds_normrank_sample"
sds_normranknorm_utility$group <- "sds_normrank_norm"
sds_normranknormrank_utility$group <- "sds_normrank_normrank"

sds_terrancev1_utility <- read.csv(file = sds_filepath_version1)
sds_terrancev2_utility <- read.csv(file = sds_filepath_version2)
# add the group column
sds_terrancev1_utility$group <- "sds_terrance_v1"
sds_terrancev2_utility$group <- "sds_terrance_v2"

combined_df <- rbind(sds_cartsample_utility, sds_cartnorm_utility, sds_cartnormrank_utility,
                     sds_rfsample_utility, sds_rfnorm_utility, sds_rfnormrank_utility,
                     sds_bagsample_utility, sds_bagnorm_utility, sds_bagnormrank_utility,
                     sds_polyregsample_utility, sds_polyregnorm_utility, sds_polyregnormrank_utility,
                     sds_normsample_utility, sds_normnorm_utility, sds_normnormrank_utility,
                     sds_normranksample_utility, sds_normranknorm_utility, sds_normranknormrank_utility,
                     sds_terrancev1_utility, sds_terrancev2_utility)

ggplot(combined_df, aes(x = group, y = S_pMSE, fill = group)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot()

# Create a ggplot with faceted panels
ggplot(combined_df, aes(x = factor(group), y = S_pMSE)) +
  geom_boxplot() +
  facet_wrap(~ group, ncol = 4)

library(RColorBrewer)
combined_df$group <- factor(combined_df$group, levels = unique(combined_df$group))
# Set plot size and font size
options(repr.plot.width = 10, repr.plot.height = 6)
theme_set(theme_bw(base_size = 14))

# Define plot
p <- ggplot(combined_df, aes(x = group, y = S_pMSE, fill = group)) +
  geom_boxplot(width = 0.5, outlier.size = 2, position = position_dodge(width = 0.75)) +
  labs(x = "Group", y = "S_pMSE") +
  ggtitle("Boxplots of S_pMSE by Group") +
  # scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Display plot
print(p)


# drop one column
sds_cartsample_Spmse <- subset(sds_cartsample_utility, select=-c(pMSE))
sds_cartnorm_Spmse <- subset(sds_cartnorm_utility, select=-c(pMSE))
sds_cartnormrank_Spmse <- subset(sds_cartnormrank_utility, select=-c(pMSE))

sds_rfsample_Spmse <- subset(sds_rfsample_utility, select=-c(pMSE))
sds_rfnorm_Spmse <- subset(sds_rfnorm_utility, select=-c(pMSE))
sds_rfnormrank_Spmse <- subset(sds_rfnormrank_utility, select=-c(pMSE))

sds_bagsample_Spmse <- subset(sds_bagsample_utility, select=-c(pMSE))
sds_bagnorm_Spmse <- subset(sds_bagnorm_utility, select=-c(pMSE))
sds_bagnormrank_Spmse <- subset(sds_bagnormrank_utility, select=-c(pMSE))

sds_polyregsample_Spmse <- subset(sds_polyregsample_utility, select=-c(pMSE))
sds_polyregnorm_Spmse <- subset(sds_polyregnorm_utility, select=-c(pMSE))
sds_polyregnormrank_Spmse <- subset(sds_polyregnormrank_utility, select=-c(pMSE))

sds_normsample_Spmse <- subset(sds_normsample_utility, select=-c(pMSE))
sds_normnorm_Spmse <- subset(sds_normnorm_utility, select=-c(pMSE))
sds_normnormrank_Spmse <- subset(sds_normnormrank_utility, select=-c(pMSE))

sds_normranksample_Spmse <- subset(sds_normranksample_utility, select=-c(pMSE))
sds_normranknorm_Spmse <- subset(sds_normranknorm_utility, select=-c(pMSE))
sds_normranknormrank_Spmse <- subset(sds_normranknormrank_utility, select=-c(pMSE))

sds_terrancev1_Spmse <- subset(sds_terrancev1_utility, select=-c(pMSE))
sds_terrancev2_Spmse <- subset(sds_terrancev2_utility, select=-c(pMSE))

sds_cartsample_Spmse["vars_list"][sds_cartsample_Spmse['S_pMSE']>10]
sds_cartnorm_Spmse["vars_list"][sds_cartnorm_Spmse['S_pMSE']>10]
sds_cartnormrank_Spmse["vars_list"][sds_cartnormrank_Spmse['S_pMSE']>10]
cart_list <- Reduce(union, list(sds_cartsample_Spmse["vars_list"][sds_cartsample_Spmse['S_pMSE']>10], 
                   sds_cartnorm_Spmse["vars_list"][sds_cartnorm_Spmse['S_pMSE']>10], 
                   sds_cartnormrank_Spmse["vars_list"][sds_cartnormrank_Spmse['S_pMSE']>10]))
#  for cart group
# "weight" "E3"     "E4"     "E7"     "F1"     "F2_1"   "F2_2"   "B2"     "E5"     "B1_13"  "E6"
sds_rfsample_Spmse["vars_list"][sds_rfsample_Spmse['S_pMSE']>10]
sds_rfnorm_Spmse["vars_list"][sds_rfnorm_Spmse['S_pMSE']>10]
sds_rfnormrank_Spmse["vars_list"][sds_rfnormrank_Spmse['S_pMSE']>10]
rf_list <- Reduce(union, list(sds_rfsample_Spmse["vars_list"][sds_rfsample_Spmse['S_pMSE']>10], 
                   sds_rfnorm_Spmse["vars_list"][sds_rfnorm_Spmse['S_pMSE']>10], 
                   sds_rfnormrank_Spmse["vars_list"][sds_rfnormrank_Spmse['S_pMSE']>10]))
# for rf group
# [1] "weight" "B1_11"  "B1_13"  "B3"     "B5"     "B7"     "B8"     "B9"     "B10"    "C7"     "C8"    
# [12] "D1"     "D2"     "D3"     "D4"     "D5"     "D7"     "D9"     "E3"     "E4"     "E7"     "F1"    
# [23] "F2_1"   "B2"     "B4"     "E5"     "C3"     "F2_2"   "E6"     "C1_m"  

bag_list <- Reduce(union, list(sds_bagsample_Spmse["vars_list"][sds_bagsample_Spmse['S_pMSE']>10], 
                   sds_bagnorm_Spmse["vars_list"][sds_bagnorm_Spmse['S_pMSE']>10], 
                   sds_bagnormrank_Spmse["vars_list"][sds_bagnormrank_Spmse['S_pMSE']>10]))
# for bag group
#  [1] "weight" "B1_11"  "B1_13"  "C8"     "D1"     "D2"     "D4"     "D5"     "D9"     "E3"    
# [11] "E4"     "E7"     "F1"     "F2_1"   "F2_2"   "B2"     "E5"     "B5"     "B4"     "E6"    
# [21] "B3"     "C3"  
require(gtools)
mixedsort(as.vector(bag_list))

polyreg_list <- Reduce(union, list(sds_polyregsample_Spmse["vars_list"][sds_polyregsample_Spmse['S_pMSE']>10], 
                   sds_polyregnorm_Spmse["vars_list"][sds_polyregnorm_Spmse['S_pMSE']>10], 
                   sds_polyregnormrank_Spmse["vars_list"][sds_polyregnormrank_Spmse['S_pMSE']>10]))
# for polyreg group
#  [1] "weight" "B1_13"  "B10"    "E3"     "E4"     "E7"     "F1"     "F2_2"   "B2"     "B4"    
# [11] "E5"     "E6"   
mixedsort(as.vector(polyreg_list))

norm_list <- Reduce(union, list(sds_normsample_Spmse["vars_list"][sds_normsample_Spmse['S_pMSE']>10], 
                   sds_normnorm_Spmse["vars_list"][sds_normnorm_Spmse['S_pMSE']>10], 
                   sds_normnormrank_Spmse["vars_list"][sds_normnormrank_Spmse['S_pMSE']>10]))
# for norm group
#  [1] "weight" "B1_13"  "C1_m"   "C2"     "C3"     "C5"     "C6"     "C7"     "C8"     "D1"    
# [11] "D2"     "D3"     "D4"     "D5"     "D7"     "D8"     "D9"     "E3"     "E4"     "E7"    
# [21] "F1"     "F2_1"   "F2_2"   "B2"     "B4"     "E5"     "B1_11"  "B5"     "E6" 
mixedsort(as.vector(norm_list))

normrank_list <- Reduce(union, list(sds_normranksample_Spmse["vars_list"][sds_normranksample_Spmse['S_pMSE']>10], 
                   sds_normranknorm_Spmse["vars_list"][sds_normranknorm_Spmse['S_pMSE']>10], 
                   sds_normranknormrank_Spmse["vars_list"][sds_normranknormrank_Spmse['S_pMSE']>10]))
# for normrank group
#  [1] "weight" "B1_11"  "B1_13"  "C1_m"   "C2"     "C3"     "C5"     "C6"     "C7"     "C8"    
# [11] "D1"     "D2"     "D3"     "D4"     "D5"     "D7"     "D8"     "D9"     "E3"     "E4"    
# [21] "E7"     "F1"     "F2_1"   "F2_2"   "B2"     "B4"     "E5"     "E6"  
mixedsort(as.vector(normrank_list))

terrance_list <- Reduce(union, list(sds_terrancev1_Spmse["vars_list"][sds_terrancev1_Spmse['S_pMSE']>10], 
                   sds_terrancev2_Spmse["vars_list"][sds_terrancev2_Spmse['S_pMSE']>10]))
# for terrance group
#  [1] "weight" "B1_3"   "B1_7"   "B1_9"   "B1_12"  "B1_13"  "B3"     "B6"     "B7"     "B11"   
# [11] "C1_m"   "C2"     "C3"     "C5"     "C6"     "C7"     "C8"     "D1"     "D2"     "D3"    
# [21] "D4"     "D5"     "D7"     "D8"     "E2"     "E3"     "E4"     "E7"     "F1"     "F2_1"  
# [31] "F2_2"   "B2"     "B4"     "E5"     "E6"     "B1_1"   "B1_2"   "B1_4"   "B1_5"   "B1_6"  
# [41] "B1_8"   "B1_10"  "B1_11"  "B5"     "B10"    "B12_1"  "B12_2"  "B12_3"  "B12_4"  "B12_5" 
# [51] "B12_6"  "D9"   
mixedsort(as.vector(terrance_list))
length(terrance_list)

mixedsort(Reduce(intersect, list(as.vector(cart_list),as.vector(rf_list),as.vector(bag_list), 
                       as.vector(polyreg_list), as.vector(norm_list), as.vector(normrank_list),
                       as.vector(terrance_list))))

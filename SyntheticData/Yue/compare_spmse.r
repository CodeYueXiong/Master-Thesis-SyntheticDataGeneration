# load the required packages
library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(ggplot2)
library(dbplyr)
library(data.table)
library(tidyverse)
library(lattice)

# set the working directory
wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# S_pMSE comparison
compare_file_path <- "./SyntheticData/Yue/compare_spmse.xlsx"
utility_compare_df <- as.data.frame(readxl::read_excel(compare_file_path))

str(utility_compare_df)


# Extract the columns with the numerical data for each group
cart_group <- as.vector(utility_compare_df[utility_compare_df$method=="cart",][3])
rf_group <- utility_compare_df[utility_compare_df$method=="rf",]
bag_group <- utility_compare_df[utility_compare_df$method=="bag",]
polyreg_group <- utility_compare_df[utility_compare_df$method=="polyreg",]
norm_group <- utility_compare_df[utility_compare_df$method=="norm",]
normrank_group <- utility_compare_df[utility_compare_df$method=="normrank",]

class(cart_group)
as.numeric
# Create a histogram comparing the occurrences of numbers in the two groups
hist(cart_group, col = "red", main = "Group 1", xlab = "Value", ylab = "Frequency")
hist(rf_group, col = "blue", add = TRUE)
legend("topright", c("Group 1", "Group 2"), fill = c("red", "blue"))


# load library ggplot2
library(ggplot2)

# set theme
theme_set(theme_bw(12))

# create x vector
xAxis <- rnorm(500)             

# create groups in variable using conditional 
# statements
group <- rep(1, 500)              
group[xAxis > -2] <- 2
group[xAxis > -1] <- 3
group[xAxis > 0] <- 4
group[xAxis > 1] <- 5
group[xAxis > 2] <- 6

# create sample data frame
sample_data <- data.frame(xAxis, group) 

# create histogram using ggplot() 
# function colored by group
ggplot(utility_compare_df, aes(x=syn_method, fill = factor(number)))+
  geom_bar(position = "fill") +
  geom_text(aes(label = ..count..), stat = "count", position = "fill")
  # geom_histogram( color='#e9ecef', alpha=0.6, position='identity')+
  # geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

ggplot(data=utility_compare_df, aes(x=syn_method, fill = factor(weight_method))) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

ggplot(mtcars, aes(cyl, fill = factor(gear)))+
  geom_bar(position = "fill") +
  geom_text(aes(label = ..count..), stat = "count", position = "fill")
mtcars

ggplot(utility_compare_df,aes(x=syn_method)) + 
  geom_histogram(data=subset(utility_compare_df,syn_method == 'cart'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(utility_compare_df,syn_method == 'rf'),fill = "blue", alpha = 0.2) +
  geom_histogram(data=subset(utility_compare_df,syn_method == 'bag'),fill = "green", alpha = 0.2)

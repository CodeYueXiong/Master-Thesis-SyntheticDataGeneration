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

# set the working directory
wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
# wd <- "Y:/Master-Thesis-DifferentialPrivacy"
setwd(wd)
wd <- "/Users/roxy/Desktop/Master-Thesis-SyntheticDataGeneration"
setwd(wd)

# then we load the required preprocessed datasets
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)


#----------------------------------------------------------------#
#                           Exp-1
#--------------------- method = cart ----------------------------#
#----------------------------------------------------------------#

# now we start with the cart group by looping the saved .rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder <- "./SyntheticData/Yue/compare_results"
files <- list.files(folder, pattern = ".rda$")

syn_cart_models <- Map(rda2list, file.path(folder, files))
names(syn_cart_models) <- tools::file_path_sans_ext(files)

# we dataframe the lists
cart_sample_sds <- data.frame(syn_cart_models$cart_sample_syn)
cart_norm_sds <- data.frame(syn_cart_models$cart_norm_syn)
cart_normrank_sds <- data.frame(syn_cart_models$cart_normrank_syn)

# delete the prefix in variable naming
names(cart_sample_sds) <- sub('^syn.', '', names(cart_sample_sds))
names(cart_norm_sds) <- sub('^syn.', '', names(cart_norm_sds))
names(cart_normrank_sds) <- sub('^syn.', '', names(cart_normrank_sds))

str(cart_sample_sds)
str(cart_norm_sds)
str(cart_normrank_sds)
table(cart_norm_sds$E6)
table(bindori_dataset_threshold_chr$E6)


ncol(cart_norm_sds)==54  # there are 54 variables in total

# # for cart_sample, we try change all the character type back to factor except "weight"
# col_names <- names(cart_sample_sds)[2:59]
# cart_sample_sds[col_names] <- lapply(cart_sample_sds[col_names], factor)
# str(cart_sample_sds)

# for cart_sample, cart_norm and normrank, we will have to reverse the integer back to the factor

# additionally, for cart sample, norm and norm, we reverse the integer back to interval
table(cart_sample_sds$B2, cart_norm_sds$B2) # B2 is ok
cart_sample_sds$B2[cart_sample_sds$B2 == "1"] <- "-1"
cart_sample_sds$B2[cart_sample_sds$B2 == "2"] <- "-99"
cart_sample_sds$B2[cart_sample_sds$B2 == "3"] <- "[0, 1)"
cart_sample_sds$B2[cart_sample_sds$B2 == "4"] <- "[1, 3)"
cart_norm_sds$B2[cart_norm_sds$B2 == "1"] <- "-1"
cart_norm_sds$B2[cart_norm_sds$B2 == "2"] <- "-99"
cart_norm_sds$B2[cart_norm_sds$B2 == "3"] <- "[0, 1)"
cart_norm_sds$B2[cart_norm_sds$B2 == "4"] <- "[1, 3)"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "1"] <- "-1"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "2"] <- "-99"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "3"] <- "[0, 1)"
cart_normrank_sds$B2[cart_normrank_sds$B2 == "4"] <- "[1, 3)"

table(cart_sample_sds$B4, cart_norm_sds$B4) # B4 is ok
cart_sample_sds$B4[cart_sample_sds$B4 == "1"] <- "-99"
cart_sample_sds$B4[cart_sample_sds$B4 == "2"] <- "[0, 1)"
cart_sample_sds$B4[cart_sample_sds$B4 == "3"] <- "[1, 5)"
cart_norm_sds$B4[cart_norm_sds$B4 == "1"] <- "-99"
cart_norm_sds$B4[cart_norm_sds$B4 == "2"] <- "[0, 1)"
cart_norm_sds$B4[cart_norm_sds$B4 == "3"] <- "[1, 5)"
cart_normrank_sds$B4[cart_normrank_sds$B4 == "1"] <- "-99"
cart_normrank_sds$B4[cart_normrank_sds$B4 == "2"] <- "[0, 1)"
cart_normrank_sds$B4[cart_normrank_sds$B4 == "3"] <- "[1, 5)"

table(cart_sample_sds$E5, cart_norm_sds$E5) # E5 is ok
cart_sample_sds$E5[cart_sample_sds$E5 == "1"] <- "-99"
cart_sample_sds$E5[cart_sample_sds$E5 == "2"] <- "[0, 1)"
cart_sample_sds$E5[cart_sample_sds$E5 == "3"] <- "[1, 2)"
cart_norm_sds$E5[cart_norm_sds$E5 == "1"] <- "-99"
cart_norm_sds$E5[cart_norm_sds$E5 == "2"] <- "[0, 1)"
cart_norm_sds$E5[cart_norm_sds$E5 == "3"] <- "[1, 2)"
cart_normrank_sds$E5[cart_normrank_sds$E5 == "1"] <- "-99"
cart_normrank_sds$E5[cart_normrank_sds$E5 == "2"] <- "[0, 1)"
cart_normrank_sds$E5[cart_normrank_sds$E5 == "3"] <- "[1, 2)"

table(cart_sample_sds$E6, cart_norm_sds$E6) # exclude E6, 54-1
cart_sample_sds$E6[cart_sample_sds$E6 == "1"] <- "-99"
cart_sample_sds$E6[cart_sample_sds$E6 == "2"] <- "[0, 9)"
cart_norm_sds$E6[cart_norm_sds$E6 == "1"] <- "-99"
cart_norm_sds$E6[cart_norm_sds$E6 == "2"] <- "[0, 9)"
cart_normrank_sds$E6[cart_normrank_sds$E6 == "1"] <- "-99"
cart_normrank_sds$E6[cart_normrank_sds$E6 == "2"] <- "[0, 9)"

# change var "B2", "B4", "E5", "E6" to factor type
cols_factor <- c("B2", "B4", "E5", "E6")
cart_sample_sds[cols_factor] <- lapply(cart_sample_sds[cols_factor], factor)
cart_norm_sds[cols_factor] <- lapply(cart_norm_sds[cols_factor], factor)
cart_normrank_sds[cols_factor] <- lapply(cart_normrank_sds[cols_factor], factor)

str(cart_sample_sds)
str(cart_norm_sds)
str(cart_normrank_sds)
table(cart_norm_sds$E6)
table(cart_normrank_sds$E6)
table(cart_sample_sds$E6)
table(bindori_dataset_threshold_chr$E6)
# is it necessary to add more levels to the dataset
levels(bindori_dataset_threshold_chr$E6) <- c(levels(bindori_dataset_threshold_chr$E6), "0", "3")
levels(cart_sample_sds$E6) <- c(levels(cart_sample_sds$E6), "0", "3")
levels(cart_normrank_sds$E6) <- c(levels(cart_normrank_sds$E6), "0", "3")

#------------------Evaluating the utility of the cart_syn sds------------------

#=========(1). one-way marginals using compare()
# try with 54-4 variables firstly, we subset the original ods first
ncol(cart_norm_sds)
bindori_dataset_threshold_chr <- bindori_dataset_threshold_chr %>% select(names(cart_sample_sds))
# cuz the compare function cannot tackle with factor type variables, we delete them for evaluation
bindori_select_vars <- bindori_dataset_threshold_chr  # 54
cartsample_select_vars <- cart_sample_sds  # 54
cartnorm_select_vars <- cart_norm_sds
cartnormrank_select_vars <- cart_normrank_sds  # 54

ncol(cartnorm_select_vars)
cartsample_select_vars$weight

#******************* for cart sample
compare_plots_cartsample<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_cartsample[[i]] <- compare(object = data.frame(Pdata = cartsample_select_vars[i]),
                                data = data.frame(Pdata = bindori_select_vars[i]),
                                vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                msel = NULL, stat = "percents", breaks = 10,
                                nrow = 2, ncol = 2, rel.size.x = 1,
                                utility.stats = c("pMSE", "S_pMSE"),
                                cols = c("#1A3C5A","#4187BF"),
                                plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/compare_results/oneway_compare_cartsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_cartsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# specify the file path to store the PNG files
destination_path <- "./SyntheticData/Yue/compare_results/one_way_"

# Print plots to a pdf file
# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/compare_results/oneway_compare_cartsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_cartsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()
# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_cartsample <- c()
SpMSE_list_cartsample <- c()
for (i in 1:54) {
  pMSE_list_cartsample <- append(pMSE_list_cartsample, compare_plots_cartsample[[i]]$tab.utility[1])
  SpMSE_list_cartsample <- append(SpMSE_list_cartsample, compare_plots_cartsample[[i]]$tab.utility[2])
}

#create data frame
df_utility_cartsample <- data.frame(vars_list=colnames(cartsample_select_vars),
                         pMSE=pMSE_list_cartsample,
                         S_pMSE=SpMSE_list_cartsample)

write_utility_cartsample <- "./SyntheticData/Yue/compare_results/oneway_utility_cartsample.csv"
write.csv(df_utility_cartsample, write_utility_cartsample, row.names=FALSE)

vars2show_cartsample <- df_utility_cartsample[df_utility_cartsample[, "S_pMSE"]<10, ][1]

nrow(vars2show_cartsample)  # there are 45 in total for cartsample


#******************* for cart norm
compare_plots_cartnorm<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_cartnorm[[i]] <- compare(object = data.frame(Pdata = cartnorm_select_vars[i]),
                                           data = data.frame(Pdata = bindori_select_vars[i]),
                                           vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                           msel = NULL, stat = "percents", breaks = 10,
                                           nrow = 2, ncol = 2, rel.size.x = 1,
                                           utility.stats = c("pMSE", "S_pMSE"),
                                           cols = c("#1A3C5A","#4187BF"),
                                           plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/compare_results/oneway_compare_cartnorm.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_cartnorm[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_cartnorm <- c()
SpMSE_list_cartnorm <- c()
for (i in 1:54) {
  pMSE_list_cartnorm <- append(pMSE_list_cartnorm, compare_plots_cartnorm[[i]]$tab.utility[1])
  SpMSE_list_cartnorm <- append(SpMSE_list_cartnorm, compare_plots_cartnorm[[i]]$tab.utility[2])
}

#create data frame
df_utility_cartnorm <- data.frame(vars_list=colnames(cartnorm_select_vars),
                                    pMSE=pMSE_list_cartnorm,
                                    S_pMSE=SpMSE_list_cartnorm)

write_utility_cartnorm <- "./SyntheticData/Yue/compare_results/oneway_utility_cartnorm.csv"
write.csv(df_utility_cartnorm, write_utility_cartnorm, row.names=FALSE)

vars2show_cartnorm <- df_utility_cartnorm[df_utility_cartnorm[, "S_pMSE"]<10, ][1]

nrow(vars2show_cartnorm)  # there are 43 in total for cartnorm


#******************* for cart normrank
compare_plots_cartnormrank<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_cartnormrank[[i]] <- compare(object = data.frame(Pdata = cartnormrank_select_vars[i]),
                                         data = data.frame(Pdata = bindori_select_vars[i]),
                                         vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                         msel = NULL, stat = "percents", breaks = 10,
                                         nrow = 2, ncol = 2, rel.size.x = 1,
                                         utility.stats = c("pMSE", "S_pMSE"),
                                         cols = c("#1A3C5A","#4187BF"),
                                         plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/compare_results/oneway_compare_cartnormrank.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_cartnormrank[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_cartnormrank <- c()
SpMSE_list_cartnormrank <- c()
for (i in 1:54) {
  pMSE_list_cartnormrank <- append(pMSE_list_cartnormrank, compare_plots_cartnormrank[[i]]$tab.utility[1])
  SpMSE_list_cartnormrank <- append(SpMSE_list_cartnormrank, compare_plots_cartnormrank[[i]]$tab.utility[2])
}

#create data frame
df_utility_cartnormrank <- data.frame(vars_list=colnames(cartnormrank_select_vars),
                                  pMSE=pMSE_list_cartnormrank,
                                  S_pMSE=SpMSE_list_cartnormrank)

write_utility_cartnormrank <- "./SyntheticData/Yue/compare_results/oneway_utility_cartnormrank.csv"
write.csv(df_utility_cartnormrank, write_utility_cartnormrank, row.names=FALSE)

vars2show_cartnormrank <- df_utility_cartnormrank[df_utility_cartnormrank[, "S_pMSE"]<10, ][1]

nrow(vars2show_cartnormrank)  # there are 43 in total for cartnormrank

# -----------------------------------------------------------------------------
################################# Machine Learning #############################
# -----------------------------------------------------------------------------
library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3viz)
library(mlr3verse)
library(mlr3benchmark)

library(e1071)
library(MASS)
library(glmnet)
library(ranger)

set.seed(2023) # make sure the results is reproducible
#*****************************************************
# Model 1: contact tracing app -- F2_1

# step1: prepare the datasets
vars_inc_m1 <- c("D1","D2","D3","D4","D5","D7","D8","D9","E2","E3","E4","E7","E5","E6","F2_1")
ods_m1 <- bindori_dataset_threshold_chr[vars_inc_m1]
table(ods_m1$F2_1)
# # also dump these variables with missingness -99
# ods_m1 <- ods_m1[ods_m1$F2_1 == "1" | ods_m1$F2_1 == "2",]
# ods_m1$F2_1 <- factor(ods_m1$F2_1, levels = c('1', '2'))
# table(ods_m1$F2_1)


sds_cartsample_m1 <- cart_sample_sds[vars_inc_m1]
table(sds_cartsample_m1$F2_1)
# # also dump these variables with missingness -99
# sds_cartsample_m1 <- sds_cartsample_m1[sds_cartsample_m1$F2_1 == "1" | sds_cartsample_m1$F2_1 == "2",]
# sds_cartsample_m1$F2_1 <- factor(sds_cartsample_m1$F2_1, levels = c('1', '2'))
# table(sds_cartsample_m1$F2_1)

sds_cartnorm_m1 <- cart_norm_sds[vars_inc_m1]
table(sds_cartnorm_m1$F2_1)
# also dump these variables with missingness -99
# sds_cartnorm_m1 <- sds_cartnorm_m1[sds_cartnorm_m1$F2_1 == "1" | sds_cartnorm_m1$F2_1 == "2",]
# sds_cartnorm_m1$F2_1 <- factor(sds_cartnorm_m1$F2_1, levels = c('1', '2'))
# table(sds_cartnorm_m1$F2_1)

sds_cartnormrank_m1 <- cart_normrank_sds[vars_inc_m1]
table(sds_cartnormrank_m1$F2_1)
# # also dump these variables with missingness -99
# sds_cartnormrank_m1 <- sds_cartnormrank_m1[sds_cartnormrank_m1$F2_1 == "1" | sds_cartnormrank_m1$F2_1 == "2",]
# sds_cartnormrank_m1$F2_1 <- factor(sds_cartnormrank_m1$F2_1, levels = c('1', '2'))
# table(sds_cartnormrank_m1$F2_1)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m1 <- TaskClassif$new(id="tsk_ods_m1",
                              backend=ods_m1, target="F2_1")

tsk_cartsample_m1 <- TaskClassif$new(id="tsk_cartsample_m1", 
                                     backend=sds_cartsample_m1, target="F2_1")

tsk_cartnorm_m1 <- TaskClassif$new(id="tsk_cartnorm_m1", 
                                   backend=sds_cartnorm_m1, target="F2_1")

tsk_cartnormrank_m1 <- TaskClassif$new(id="tsk_cartnormrank_m1",
                                       backend=sds_cartnormrank_m1, target="F2_1")

tasks_list_cart <- list(tsk_ods_m1, tsk_cartsample_m1,tsk_cartnorm_m1, tsk_cartnormrank_m1,
                        tsk_ods_m2, tsk_cartsample_m2,tsk_cartnorm_m2, tsk_cartnormrank_m2)

autoplot(tsk_ods_m1)

# step3: prepare the required learners
learners_list_cart <- lrns(c("classif.multinom", "classif.ranger"))  # classif.lda

# step4: benchmark the task and learners with cross-validation
# benchmark_grid is the design
# rather than cv, we use holdout as the resampling technique, ratio=0.8
bm_models_cart <- benchmark(benchmark_grid(tasks = tasks_list_cart,
                                      learners = learners_list_cart, resamplings = rsmp("holdout", ratio=0.8)),
                       store_models = TRUE)

# step5: validate the accuracy of the model
#****** Measure to compare true observed 
#****** labels with predicted labels in 
#****** multiclass classification tasks.
bm_models_cart$aggregate(msr("classif.acc"))[learner_id=="classif.ranger",]

autoplot(bm_model1)

# step6: extract the coefficients of the trained instances
mlr3misc::map(as.data.table(bm_models_cart)$learner, "model")[[2]]

# step7: save bm_model as rds
saveRDS(bm_models_cart, './SyntheticData/Yue/compare_results/bm_models_cart.rds')

#*****************************************************
# Model 2: covid positive -- B8 (multiclass)

# step1: prepare the datasets
vars_inc_m2 <- c("E2","E3","E4","E7","E5","E6","C1_m","C2","C3","C5","C6","C7","C8","B8")
ods_m2 <- bindori_dataset_threshold_chr[vars_inc_m2]
ods_m2$B8 = factor(ods_m2$B8)
table(ods_m2$B8)

sds_cartsample_m2 <- cart_sample_sds[vars_inc_m2]
sds_cartsample_m2$B8 = factor(sds_cartsample_m2$B8)

sds_cartnorm_m2 <- cart_norm_sds[vars_inc_m2]
sds_cartnorm_m2$B8 = factor(sds_cartnorm_m2$B8)

sds_cartnormrank_m2 <- cart_normrank_sds[vars_inc_m2]
sds_cartnormrank_m2$B8 = factor(sds_cartnormrank_m2$B8)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m2 <- TaskClassif$new(id="tsk_ods_m2",
                              backend=ods_m2, target="B8")

tsk_cartsample_m2 <- TaskClassif$new(id="tsk_cartsample_m2", 
                                     backend=sds_cartsample_m2, target="B8")

tsk_cartnorm_m2 <- TaskClassif$new(id="tsk_cartnorm_m2", 
                                   backend=sds_cartnorm_m2, target="B8")

tsk_cartnormrank_m2 <- TaskClassif$new(id="tsk_cartnormrank_m2",
                                       backend=sds_cartnormrank_m2, target="B8")

tasks_list_m2 <- list(tsk_ods_m2, tsk_cartsample_m2,tsk_cartnorm_m2, tsk_cartnormrank_m2)


# step3: prepare the required learners
learners_list_model2 <- lrns(c("classif.lda", "classif.multinom"))

# step4: benchmark the task and learners with resampling
# benchmark_grid is the design
bm_model2 <- benchmark(benchmark_grid(tasks = tasks_list_m2,
                                      learners = learners_list_model2,
                                      resamplings = rsmp("holdout", ratio=0.8)),

                       store_models = TRUE)

# step5: validate the accuracy of the model
#****** Measure to compare true observed 
#****** labels with predicted labels in 
#****** multiclass classification tasks.
bm_model2$aggregate(msr("classif.acc"))[learner_id == "classif.multinom",]

# step7: save bm_model as rds
saveRDS(bm_model2, './SyntheticData/Yue/compare_results/bm_model2.rds')

# step6: extract the coefficients of the trained instances
mlr3misc::map(as.data.table(bm_model2)$learner, "model")
# score_multinom_m1_ods <- sum(data.frame(bm_model1$score(msr("classif.acc"))[learner_id == 'classif.multinom', ][task_id == "tsk_ods_m1", ])["classif.acc"])/3
# score_multinom_m1_ods <- sum(data.frame(bm_model1$score(msr("classif.acc"))[learner_id == 'classif.multinom', ][task_id == "tsk_ods_m1", ])["classif.acc"])/3
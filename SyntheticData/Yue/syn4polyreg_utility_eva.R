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
wd <- "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# then we load the required preprocessed datasets
load("bindori_dataset_preprocessed_factor.rda")
str(bindori_dataset_threshold_chr)

#----------------------------------------------------------------#
#--------------------- method = polyreg -------------------------#
#----------------------------------------------------------------#

# now we start with the cart group by looping the saved .rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder <- "./SyntheticData/Yue/syn4_polyreg"
files <- list.files(folder, pattern = ".rda$")

syn_polyreg_models <- Map(rda2list, file.path(folder, files))
names(syn_polyreg_models) <- tools::file_path_sans_ext(files)

# we dataframe the lists
polyreg_sample_sds <- data.frame(syn_polyreg_models$polyreg_sample_syn)
polyreg_norm_sds <- data.frame(syn_polyreg_models$polyreg_norm_syn)
polyreg_normrank_sds <- data.frame(syn_polyreg_models$polyreg_normrank_syn)

# delete the prefix in variable naming
names(polyreg_sample_sds) <- sub('^syn.', '', names(polyreg_sample_sds))
names(polyreg_norm_sds) <- sub('^syn.', '', names(polyreg_norm_sds))
names(polyreg_normrank_sds) <- sub('^syn.', '', names(polyreg_normrank_sds))

str(polyreg_sample_sds) # =54
str(polyreg_norm_sds)
str(polyreg_normrank_sds)

table(polyreg_norm_sds$E6, polyreg_normrank_sds$E6)
table(polyreg_norm_sds$E6, polyreg_sample_sds$E6)
table(polyreg_sample_sds$E6, polyreg_normrank_sds$E6)
#------------------Evaluating the utility of the polyreg_syn sds------------------

#=========(1). one-way marginals using compare()
ncol(polyreg_sample_sds)
# only the E6 variable needs to be reversed back to factor
polyreg_sample_sds$E6[polyreg_sample_sds$E6 == "1"] <- "-99"
polyreg_sample_sds$E6[polyreg_sample_sds$E6 == "2"] <- "[0, 9)"
polyreg_norm_sds$E6[polyreg_norm_sds$E6 == "1"] <- "-99"
polyreg_norm_sds$E6[polyreg_norm_sds$E6 == "2"] <- "[0, 9)"
polyreg_normrank_sds$E6[polyreg_normrank_sds$E6 == "1"] <- "-99"
polyreg_normrank_sds$E6[polyreg_normrank_sds$E6 == "2"] <- "[0, 9)"

cols_factor <- c("E6")

polyreg_sample_sds[cols_factor] <- lapply(polyreg_sample_sds[cols_factor], factor)
polyreg_norm_sds[cols_factor] <- lapply(polyreg_norm_sds[cols_factor], factor)
polyreg_normrank_sds[cols_factor] <- lapply(polyreg_normrank_sds[cols_factor], factor)

str(polyreg_sample_sds)
str(polyreg_norm_sds)
str(polyreg_normrank_sds)

# polyregsample_select_vars <- subset(polyreg_sample_sds, select = -c(B2, B4, E5, E6))
# polyregnorm_select_vars <- subset(polyreg_norm_sds, select = -c(B2, B4, E5, E6))
# polyregnormrank_select_vars <- subset(polyreg_normrank_sds, select = -c(B2, B4, E5, E6))
bindori_select_vars <- bindori_dataset_threshold_chr %>% select(names(polyreg_sample_sds))
ncol(bindori_select_vars) # ==54 ?
# is it necessary to add more levels to the dataset
table(bindori_dataset_threshold_chr$E6, polyreg_norm_sds$E6)
levels(bindori_select_vars$E6) <- c(levels(bindori_select_vars$E6), "0", "3")
levels(polyreg_sample_sds$E6) <- c(levels(polyreg_sample_sds$E6), "0", "3")
levels(polyreg_normrank_sds$E6) <- c(levels(polyreg_normrank_sds$E6), "0", "3")

#******************* for polyreg sample
compare_plots_polyregsample<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_polyregsample[[i]] <- compare(object = data.frame(Pdata = polyreg_sample_sds[i]),
                                           data = data.frame(Pdata = bindori_select_vars[i]),
                                           vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                           msel = NULL, stat = "percents", breaks = 10,
                                           nrow = 2, ncol = 2, rel.size.x = 1,
                                           utility.stats = c("pMSE", "S_pMSE"),
                                           cols = c("#1A3C5A","#4187BF"),
                                           plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn4_polyreg/oneway_compare_polyregsample.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_polyregsample[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

table(bindori_select_vars$B2, polyreg_sample_sds$B2)
# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_polyregsample <- c()
SpMSE_list_polyregsample <- c()
for (i in 1:54) {
  pMSE_list_polyregsample <- append(pMSE_list_polyregsample, compare_plots_polyregsample[[i]]$tab.utility[1])
  SpMSE_list_polyregsample <- append(SpMSE_list_polyregsample, compare_plots_polyregsample[[i]]$tab.utility[2])
}

#create data frame
df_utility_polyregsample <- data.frame(vars_list=colnames(polyreg_sample_sds),
                                    pMSE=pMSE_list_polyregsample,
                                    S_pMSE=SpMSE_list_polyregsample)

write_utility_polyregsample <- "./SyntheticData/Yue/syn4_polyreg/oneway_utility_polyregsample.csv"
write.csv(df_utility_polyregsample, write_utility_polyregsample, row.names=FALSE)

vars2show_polyregsample <- df_utility_polyregsample[df_utility_polyregsample[, "S_pMSE"]<10, ][1]

nrow(vars2show_polyregsample)  # there are 43 in total for polyregsample


#******************* for polyreg norm
compare_plots_polyregnorm<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_polyregnorm[[i]] <- compare(object = data.frame(Pdata = polyreg_norm_sds[i]),
                                              data = data.frame(Pdata = bindori_select_vars[i]),
                                              vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                              msel = NULL, stat = "percents", breaks = 10,
                                              nrow = 2, ncol = 2, rel.size.x = 1,
                                              utility.stats = c("pMSE", "S_pMSE"),
                                              cols = c("#1A3C5A","#4187BF"),
                                              plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn4_polyreg/oneway_compare_polyregnorm.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_polyregnorm[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

table(bindori_select_vars$E6, polyreg_norm_sds$E6)  # E6 should be left aside to print
# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_polyregnorm <- c()
SpMSE_list_polyregnorm <- c()
for (i in 1:54) {
  pMSE_list_polyregnorm <- append(pMSE_list_polyregnorm, compare_plots_polyregnorm[[i]]$tab.utility[1])
  SpMSE_list_polyregnorm <- append(SpMSE_list_polyregnorm, compare_plots_polyregnorm[[i]]$tab.utility[2])
}

#create data frame
df_utility_polyregnorm <- data.frame(vars_list=colnames(polyreg_norm_sds)[1:54],
                                       pMSE=pMSE_list_polyregnorm,
                                       S_pMSE=SpMSE_list_polyregnorm)

write_utility_polyregnorm <- "./SyntheticData/Yue/syn4_polyreg/oneway_utility_polyregnorm.csv"
write.csv(df_utility_polyregnorm, write_utility_polyregnorm, row.names=FALSE)

vars2show_polyregnorm <- df_utility_polyregnorm[df_utility_polyregnorm[, "S_pMSE"]<10, ][1]

nrow(vars2show_polyregnorm)  # there are 42 in total for polyregnorm

#******************* for polyreg normrank
compare_plots_polyregnormrank<- c()

for (i in 1:54) {
  cat(colnames(bindori_select_vars[i]), "\n")  # print the var string under analysis
  
  compare_plots_polyregnormrank[[i]] <- compare(object = data.frame(Pdata = polyreg_normrank_sds[i]),
                                              data = data.frame(Pdata = bindori_select_vars[i]),
                                              vars = c(colnames(bindori_select_vars[i])), cont.na = NULL,
                                              msel = NULL, stat = "percents", breaks = 10,
                                              nrow = 2, ncol = 2, rel.size.x = 1,
                                              utility.stats = c("pMSE", "S_pMSE"),
                                              cols = c("#1A3C5A","#4187BF"),
                                              plot = TRUE, table = TRUE)
  
}

# specify the file path to store the pdf
destination_path <- "./SyntheticData/Yue/syn4_polyreg/oneway_compare_polyregnormrank.pdf"

# Print plots to a pdf file
pdf(destination_path)

for (i in 1:54) {
  print(compare_plots_polyregnormrank[[i]]$plots)  # Plot 1 --> in the first page of PDF
}

dev.off()

table(bindori_select_vars$B2, polyreg_normrank_sds$B2)
# try exporting the __tab_utility__ as a csv file in convenience of comparing 
# and choose vars which performed better in the synthesis
pMSE_list_polyregnormrank <- c()
SpMSE_list_polyregnormrank <- c()
for (i in 1:54) {
  pMSE_list_polyregnormrank <- append(pMSE_list_polyregnormrank, compare_plots_polyregnormrank[[i]]$tab.utility[1])
  SpMSE_list_polyregnormrank <- append(SpMSE_list_polyregnormrank, compare_plots_polyregnormrank[[i]]$tab.utility[2])
}

#create data frame
df_utility_polyregnormrank <- data.frame(vars_list=colnames(polyreg_normrank_sds),
                                       pMSE=pMSE_list_polyregnormrank,
                                       S_pMSE=SpMSE_list_polyregnormrank)

write_utility_polyregnormrank <- "./SyntheticData/Yue/syn4_polyreg/oneway_utility_polyregnormrank.csv"
write.csv(df_utility_polyregnormrank, write_utility_polyregnormrank, row.names=FALSE)

vars2show_polyregnormrank <- df_utility_polyregnormrank[df_utility_polyregnormrank[, "S_pMSE"]<10, ][1]

nrow(vars2show_polyregnormrank)  # there are 42 in total for polyregnormrank

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

set.seed(2023) # make sure the results are reproducible
#*****************************************************
# Model 1: contact tracing app -- F2_1

# step1: prepare the datasets
vars_inc_m1 <- c("D1","D2","D3","D4","D5","D7","D8","D9","E2","E3","E4","E7","E5","E6","F2_1")
ods_m1 <- bindori_dataset_threshold_chr[vars_inc_m1]
table(ods_m1$F2_1)

sds_polyregsample_m1 <- polyreg_sample_sds[vars_inc_m1]
table(sds_polyregsample_m1$F2_1)

sds_polyregnorm_m1 <- polyreg_norm_sds[vars_inc_m1]
table(sds_polyregnorm_m1$F2_1)

sds_polyregnormrank_m1 <- polyreg_normrank_sds[vars_inc_m1]
table(sds_polyregnormrank_m1$F2_1)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m1 <- TaskClassif$new(id="tsk_ods_m1",
                              backend=ods_m1, target="F2_1")

tsk_polyregsample_m1 <- TaskClassif$new(id="tsk_polyregsample_m1", 
                                    backend=sds_polyregsample_m1, target="F2_1")

tsk_polyregnorm_m1 <- TaskClassif$new(id="tsk_polyregnorm_m1", 
                                  backend=sds_polyregnorm_m1, target="F2_1")

tsk_polyregnormrank_m1 <- TaskClassif$new(id="tsk_polyregnormrank_m1",
                                      backend=sds_polyregnormrank_m1, target="F2_1")

tasks_list_polyreg <- list(tsk_ods_m1, tsk_polyregsample_m1, tsk_polyregnorm_m1, tsk_polyregnormrank_m1,
                           tsk_ods_m2, tsk_polyregsample_m2, tsk_polyregnorm_m2, tsk_polyregnormrank_m2)

# step3: prepare the required learners
learners_list_polyreg <- lrns(c("classif.multinom", "classif.ranger"))  # classif.lda

# step4: benchmark the task and learners with cross-validation
# benchmark_grid is the design
bm_models_polyreg <- benchmark(benchmark_grid(tasks = tasks_list_polyreg,
                                              learners = learners_list_polyreg, resamplings = rsmp("holdout", ratio = 0.8)),
                       store_models = TRUE)

# step5: validate the accuracy of the model
#****** Measure to compare true observed 
#****** labels with predicted labels in 
#****** multiclass classification tasks.
bm_models_polyreg$aggregate(msr("classif.acc"))[learner_id=="classif.ranger",]

# step6: extract the coefficients of the trained instances
mlr3misc::map(as.data.table(bm_models_polyreg)$learner, "model")

# step7: save bm_model as rds
saveRDS(bm_models_polyreg, './SyntheticData/Yue/syn4_polyreg/bm_polyreg_models.rds')
saveRDS(coef_info_m1, './SyntheticData/Yue/syn4_polyreg/coef_polyreg_model1.rds')

#*****************************************************
# Model 2: covid positive -- B8 (multiclass)

# step1: prepare the datasets
vars_inc_m2 <- c("E2","E3","E4","E7","E5","E6","C1_m","C2","C3","C5","C6","C7","C8","B8")
ods_m2 <- bindori_dataset_threshold_chr[vars_inc_m2]
ods_m2$B8 = factor(ods_m2$B8)
table(ods_m2$B8)

sds_polyregsample_m2 <- polyreg_sample_sds[vars_inc_m2]
sds_polyregsample_m2$B8 = factor(sds_polyregsample_m2$B8)

sds_polyregnorm_m2 <- polyreg_norm_sds[vars_inc_m2]
sds_polyregnorm_m2$B8 = factor(sds_polyregnorm_m2$B8)

sds_polyregnormrank_m2 <- polyreg_normrank_sds[vars_inc_m2]
sds_polyregnormrank_m2$B8 = factor(sds_polyregnormrank_m2$B8)

# Step2: new machine learning tasks for ods and sds
tsk_ods_m2 <- TaskClassif$new(id="tsk_ods_m2",
                              backend=ods_m2, target="B8")

tsk_polyregsample_m2 <- TaskClassif$new(id="tsk_polyregsample_m2", 
                                    backend=sds_polyregsample_m2, target="B8")

tsk_polyregnorm_m2 <- TaskClassif$new(id="tsk_polyregnorm_m2", 
                                  backend=sds_polyregnorm_m2, target="B8")

tsk_polyregnormrank_m2 <- TaskClassif$new(id="tsk_polyregnormrank_m2",
                                      backend=sds_polyregnormrank_m2, target="B8")

tasks_list_m2 <- list(tsk_ods_m2, tsk_polyregsample_m2, tsk_polyregnorm_m2, tsk_polyregnormrank_m2)

# step3: prepare the required learners
learners_list_model2 <- lrns(c("classif.naive_bayes", "classif.lda"))

# step4: benchmark the task and learners with cross-validation
# benchmark_grid is the design
bm_model2 <- benchmark(benchmark_grid(tasks = tasks_list_m2,
                                      learners = learners_list_model2,
                                      resamplings = rsmp("cv", folds = 2)),
                       store_models = TRUE)

# step5: validate the accuracy of the model
#****** Measure to compare true observed 
#****** labels with predicted labels in 
#****** multiclass classification tasks.
bm_model2$aggregate(msr("classif.acc"))

# step6: extract the coefficients of the trained instances
coef_info_m2 <- mlr3misc::map(as.data.table(bm_model2)$learner, "model")

# step7: save bm_model as rds
saveRDS(bm_model2, './SyntheticData/Yue/syn4_polyreg/bm_polyreg_model2.rds')
saveRDS(coef_info_m2, './SyntheticData/Yue/syn4_polyreg/coef_polyreg_model2.rds')
# score_multinom_m1_ods <- sum(data.frame(bm_model1$score(msr("classif.acc"))[learner_id == 'classif.multinom', ][task_id == "tsk_ods_m1", ])["classif.acc"])/3
# score_multinom_m1_ods <- sum(data.frame(bm_model1$score(msr("classif.acc"))[learner_id == 'classif.multinom', ][task_id == "tsk_ods_m1", ])["classif.acc"])/3



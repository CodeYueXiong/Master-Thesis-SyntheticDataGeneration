# load the required packages
pkg_list <- c("mlr3", "mlr3learners", "mlr3filters", "mlr3pipelines", "mlr3tuning",
              "mlr3viz", "mlr3verse", "mlr3benchmark",
              "MASS", "e1071", "glmnet")
install.packages(pkg_list)
# install.packages("mlr3benchmark")

library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3viz)
library(mlr3verse)
library(mlr3benchmark)

library(readr)
library(vroom)
library(tidyverse)
library(arsenal)
library(reshape2)
library(synthpop)
library(ggplot2)
library(dbplyr)
library(data.table)
library(tidyverse)

# set the working directory
# wd <- "F:/Master-Thesis-DifferentialPrivacy"
wd <- "Y:/Master-Thesis-DifferentialPrivacy"
setwd(wd)

# load the ods dataset
load("bindori_dataset_preprocessed_factor.rda")

ncol(bindori_dataset_threshold_chr)

# for example
# now we start with the cart group by looping the saved .rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

folder <- "./SyntheticData/Yue/syn1_cart"
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
table(cart_norm_sds$B8)
table(bindori_dataset_threshold_chr$E6)


# new a machine learning task
tsk_syn_default <- Task$new(id="syn_default", task_type="classif", 
                            backend=subset(syn_default, -c(target_vars)),
                            label = "target")

tsk_syn_paraDefault <- Task$new(id="syn_paraDefault", task_type="classif", 
                                backend=subset(syn_ParaDefault, -c(target_vars)),
                                label = "target")

tsk_syn_cart <- Task$new(id="syn_cart", task_type="classif", 
                         backend=subset(syn_cart, -c(target_vars)),
                         label = "target")

# set up the learners to be trained
learners = list(
  lrn("classif.rpart", id = "rpart"),
  lrn("classif.ranger", id = "ranger"),
  lrn("classif.xgboost", id = "xgboost")
)

# set up the pipeline
graph =
  po("branch", options = c("rpart", "ranger", "xgboost")) %>>%
  gunion(lapply(learners, po)) %>>%
  po("unbranch")
# visualize the training path
graph$plot(html = FALSE)

# set up the graph learner
graph_learner = as_learner(graph)
# showup the parameter to tune
as.data.table(graph_learner$param_set)[, .(id, class, lower, upper, nlevels)]

# tune the pipeline
# branch
graph_learner$param_set$values$branch.selection =
  to_tune(c("kknn", "svm", "ranger"))

# kknn
graph_learner$param_set$values$kknn.k =
  to_tune(p_int(3, 50, logscale = TRUE, depends = branch.selection == "kknn"))

# svm
graph_learner$param_set$values$svm.cost =
  to_tune(p_dbl(-1, 1, trafo = function(x) 10^x, depends = branch.selection == "svm"))

# ranger
graph_learner$param_set$values$ranger.mtry =
  to_tune(p_int(1, 8, depends = branch.selection == "ranger"))

# short learner id for printing
graph_learner$id = "graph_learner"

# now we define a auto-tune with nested resampling instance and select random search
rr = tune_nested(
  method = "random_search",
  task = task, # also can be tasks
  learner = graph_learner,
  inner_resampling = rsmp("cv", folds = 3),
  outer_resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  term_evals = 10,
)

learner = as_learner(graph)
learner$param_set$values = instance$result_learner_param_vals
learner$train(task)

extract_inner_tuning_results(rr)
rr$score()[, .(iteration, task_id, learner_id, resampling_id, classif.auc)]
rr$aggregate()
# visualize the tuning result
autoplot(instance, type = "marginal",
         cols_x = c("x_domain_kknn.k", "x_domain_svm.cost", "ranger.mtry"))
########  ----- source code for data preprocessing -----
# packages_list <- c("readr", "vroom", "tidyverse", "arsenal", "reshape2", 
#                   "synthpop", "ggplot2", "dbplyr", "data.table")
# install.packages(packages_list)

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

# set the working directory
# wd <- "F:/Master-Thesis-DifferentialPrivacy" # used for thinkpad
wd <- "C:/Users/ru27req/Master-Thesis-DifferentialPrivacy"
setwd(wd)

file_path <- "./SyntheticData/Terrance/version_1/syn_k2_2020-08-02_2020-08-08.csv"
# gpdr_file_path <- "F:/Master-Thesis-DifferentialPrivacy/gpdr.csv" # used for tp
gpdr_file_path <- "C:/Users/ru27req/Master-Thesis-DifferentialPrivacy/gpdr.csv"

gpdr_region_preprocess <- function(file_path, gpdr_file_path) {
    syn_data <- read.csv(file = file_path)
    colnames(syn_data)[colnames(syn_data)=="sample_weight"] <- "weight"
    cols_list <- colnames(syn_data)
    ori_dataset <- list()
    # read in the original datasets
    for (i in 1:7){
        ori_dataset[[i]] <- vroom(list.files(pattern = "*_full.csv$")[i],
                    show_col_types = FALSE) %>%
                    select(all_of(cols_list))
    }
    bindori_dataset <- as.data.frame(bind_rows(ori_dataset))
    print("binding dataset is successful!")

    gpdr_countries_data <- NA
    gpdr_countries_data <- read.csv(file = gpdr_file_path, sep = ",")
    country_name <- unique(as.character(gpdr_countries_data$Country_GID))
    bindori_dataset_gpdr <- bindori_dataset %>%
                                        filter(GID_0 %in% country_name)
    syn_dataset_gpdr <- syn_data %>%
                            filter(GID_0 %in% country_name)
    print("filterring gpdr countries is successful!")

    gpdr_dataset_list <- list()
    gpdr_dataset_list$bindori_dataset_gpdr <- bindori_dataset_gpdr
    gpdr_dataset_list$syn_dataset_gpdr <- syn_dataset_gpdr
    if(ncol(gpdr_dataset_list$bindori_dataset_gpdr)==ncol(gpdr_dataset_list$syn_dataset_gpdr)) {
        print("stored in list successful!")
        return(gpdr_dataset_list)
    } else {
        print("stored in list unsuccessful!")
        return(FALSE)
    }
}

# # read in the original dataset and syn data by Terrance
# gpdr_dataset_list <- gpdr_region_preprocess(file_path, gpdr_file_path)
# bindori_dataset_gpdr <- data.frame(gpdr_dataset_list$bindori_dataset_gpdr)
# syn_dataset_gpdr <- data.frame(gpdr_dataset_list$syn_dataset_gpdr)
# ncol(syn_dataset_gpdr)
# 
# class(bindori_dataset_gpdr[[2]])
# 
# for (i in 1:ncol(bindori_dataset_gpdr)) {
#     if (class(bindori_dataset_gpdr[[i]])=="numeric") {
#         print(i)
#     }
# }
# 
# class(syn_dataset_gpdr[[1]])

threshold_preprocess <- function(bindori_dataset_gpdr) {
    # character type for B2,B4,E5,E6
    bindori_dataset_threshold <- copy(bindori_dataset_gpdr)
    # for B2
    bindori_dataset_threshold["B2"][bindori_dataset_threshold["B2"] == -99] <- "-99"
    bindori_dataset_threshold["B2"][bindori_dataset_threshold["B2"] >= 1000] <- "1000"
    bindori_dataset_threshold$B2[bindori_dataset_threshold$B2 >= 0 & bindori_dataset_threshold$B2 < 1] <- "[0, 1)"
    bindori_dataset_threshold$B2[bindori_dataset_threshold$B2 >= 1 & bindori_dataset_threshold$B2 < 3] <- "[1, 3)"
    bindori_dataset_threshold$B2[bindori_dataset_threshold$B2 >= 3 & bindori_dataset_threshold$B2 < 8] <- "[3, 8)"
    bindori_dataset_threshold$B2[bindori_dataset_threshold$B2 >= 8 & bindori_dataset_threshold$B2 < 15] <- "[8, 15)"
    bindori_dataset_threshold$B2[bindori_dataset_threshold$B2 >= 15 & bindori_dataset_threshold$B2 < 28] <- "[15, 28)"
    bindori_dataset_threshold$B2[bindori_dataset_threshold$B2 >= 28 & bindori_dataset_threshold$B2 < 90] <- "[28, 90)"
    bindori_dataset_threshold$B2[bindori_dataset_threshold$B2 >= 90 & bindori_dataset_threshold$B2 < 180] <- "[90, 180)"
    bindori_dataset_threshold$B2[bindori_dataset_threshold$B2 >= 180 & bindori_dataset_threshold$B2 < 366] <- "[180, 366)"
    bindori_dataset_threshold$B2[bindori_dataset_threshold$B2 >= 366 & bindori_dataset_threshold$B2 < 1000] <- "[366, 1000)"
    print("Var B2 thresholding succeed")
    # for B4
    bindori_dataset_threshold$B4[bindori_dataset_threshold$B4 < 0] <- "-99"
    bindori_dataset_threshold$B4[bindori_dataset_threshold$B4 >= 1000] <- "1000"
    bindori_dataset_threshold$B4[bindori_dataset_threshold$B4 >= 0 & bindori_dataset_threshold$B4 < 1] <- "[0, 1)"
    bindori_dataset_threshold$B4[bindori_dataset_threshold$B4 >= 1 & bindori_dataset_threshold$B4 < 5] <- "[1, 5)"
    bindori_dataset_threshold$B4[bindori_dataset_threshold$B4 >= 5 & bindori_dataset_threshold$B4 < 10] <- "[5, 10)"
    bindori_dataset_threshold$B4[bindori_dataset_threshold$B4 >= 10 & bindori_dataset_threshold$B4 < 1000] <- "[10, 1000)"
    print("Var B4 thresholding succeed")
    # for E5
    bindori_dataset_threshold$E5[bindori_dataset_threshold$E5 < 0] <- "-99"
    bindori_dataset_threshold$E5[bindori_dataset_threshold$E5 >= 1000] <- "1000"
    bindori_dataset_threshold$E5[bindori_dataset_threshold$E5 >= 0 & bindori_dataset_threshold$E5 < 1] <- "[0, 1)"
    bindori_dataset_threshold$E5[bindori_dataset_threshold$E5 >= 1 & bindori_dataset_threshold$E5 < 2] <- "[1, 2)"
    bindori_dataset_threshold$B4[bindori_dataset_threshold$B4 >= 2 & bindori_dataset_threshold$B4 < 4] <- "[2, 4)"
    bindori_dataset_threshold$B4[bindori_dataset_threshold$B4 >= 4 & bindori_dataset_threshold$B4 < 6] <- "[4, 6)"
    bindori_dataset_threshold$E5[bindori_dataset_threshold$E5 >= 6 & bindori_dataset_threshold$E5 < 1000] <- "[6, 1000)"
    print("Var E5 thresholding succeed")
    # for E6
    bindori_dataset_threshold$E6[bindori_dataset_threshold$E6 < 0] <- "-99"
    bindori_dataset_threshold$E6[bindori_dataset_threshold$E6 >= 26] <- "26"
    bindori_dataset_threshold$E6[bindori_dataset_threshold$E6 >= 0 & bindori_dataset_threshold$E6 < 9] <- "[0, 9)"
    bindori_dataset_threshold$E6[bindori_dataset_threshold$E6 >= 9 & bindori_dataset_threshold$E6 < 26] <- "[9, 26)"
    print("Var E6 thresholding succeed")

    return(data.frame(bindori_dataset_threshold))
}

bindori_dataset_threshold <- threshold_preprocess(bindori_dataset_gpdr)


# preprocessed original data import and export
#-----------------------------------------------
export_path <- "C:/Users/ru27req/Master-Thesis-DifferentialPrivacy"
bindori_data_name <- "bindori_dataset_threshold.rda"

save(bindori_dataset_threshold, file=paste(c(export_path, bindori_data_name), 
                                collapse="/"))

# load("sdssdc1.rda")
########  ----- source code for data preprocessing -----
packages_list <- c("readr", "vroom", "tidyverse", "arsenal", "reshape2","caret",
                  "synthpop", "ggplot2", "dbplyr", "data.table", "mltools")
install.packages(packages_list)

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
library(tidyverse)
library(here)

# set the working directory
# wd <- "F:/Master-Thesis-DifferentialPrivacy" # used for thinkpad

# wd <- "C:/Users/ru27req/Master-Thesis-DifferentialPrivacy"
# setwd(here())

wd <- "/dss/dsshome1/0C/ru27req2"
setwd(wd)


file_path <- "./Master-Thesis-DifferentialPrivacy/SyntheticData/Terrance/version_1/syn_k2_2020-08-02_2020-08-08.csv"
# gpdr_file_path <- "F:/Master-Thesis-DifferentialPrivacy/gpdr.csv" # used for tp

# gpdr_file_path <- "C:/Users/ru27req/Master-Thesis-DifferentialPrivacy/gpdr.csv"
gpdr_file_path <- "gpdr.csv"

# step 1: filter with only gdpr countries included
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
    gpdr_dataset_list$ori_dataset_list <- ori_dataset # store the original datalist datewise
    if(ncol(gpdr_dataset_list$bindori_dataset_gpdr)==ncol(gpdr_dataset_list$syn_dataset_gpdr)) {
        print("stored in list successful!")
        return(gpdr_dataset_list)
    } else {
        print("stored in list unsuccessful!")
        return(FALSE)
    }
}

# read in the original dataset and syn data by Terrance
gpdr_dataset_list <- gpdr_region_preprocess(file_path, gpdr_file_path)
class(gpdr_dataset_list$ori_dataset_list[[7]]) # No. = 7
ods_gpdr_0802 <- data.frame(gpdr_dataset_list$ori_dataset_list[[1]])
ods_gpdr_0803 <- data.frame(gpdr_dataset_list$ori_dataset_list[[2]])
ods_gpdr_0804 <- data.frame(gpdr_dataset_list$ori_dataset_list[[3]])
ods_gpdr_0805 <- data.frame(gpdr_dataset_list$ori_dataset_list[[4]])
ods_gpdr_0806 <- data.frame(gpdr_dataset_list$ori_dataset_list[[5]])
ods_gpdr_0807 <- data.frame(gpdr_dataset_list$ori_dataset_list[[6]])
ods_gpdr_0808 <- data.frame(gpdr_dataset_list$ori_dataset_list[[7]])
# ncol(ods_gpdr_0802)
gpdr_countries_data <- NA
gpdr_countries_data <- read.csv(file = gpdr_file_path, sep = ",")
country_name <- unique(as.character(gpdr_countries_data$Country_GID))

ods_gpdr_0802 <- ods_gpdr_0802 %>%
                 filter(GID_0 %in% country_name)
ods_gpdr_0803 <- ods_gpdr_0803 %>%
                 filter(GID_0 %in% country_name)
ods_gpdr_0804 <- ods_gpdr_0804 %>%
                 filter(GID_0 %in% country_name)
ods_gpdr_0805 <- ods_gpdr_0805 %>%
                 filter(GID_0 %in% country_name)
ods_gpdr_0806 <- ods_gpdr_0806 %>%
                 filter(GID_0 %in% country_name)
ods_gpdr_0807 <- ods_gpdr_0807 %>%
                 filter(GID_0 %in% country_name)
ods_gpdr_0808 <- ods_gpdr_0808 %>%
                 filter(GID_0 %in% country_name)

bindori_dataset_gpdr <- data.frame(gpdr_dataset_list$bindori_dataset_gpdr)
syn_dataset_gpdr <- data.frame(gpdr_dataset_list$syn_dataset_gpdr)
ncol(bindori_dataset_gpdr)


str(bindori_dataset_gpdr)

# bindori_dataset_gpdr$D6_1


str(bindori_dataset_gpdr)


# step 2: threshold with only gdpr countries included
threshold_preprocess <- function(bindori_dataset_gpdr) {
    # remove the GID_0 and GID_1 columns
    bindori_dataset_threshold <- copy(bindori_dataset_gpdr)
    bindori_dataset_threshold <- subset(bindori_dataset_threshold, select = -c(GID_0, GID_1))
    
    # character type for B2,B4,E5,E6
    
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

ods_threshold_0802 <- threshold_preprocess(ods_gpdr_0802)
ods_threshold_0803 <- threshold_preprocess(ods_gpdr_0803)
ods_threshold_0804 <- threshold_preprocess(ods_gpdr_0804)
ods_threshold_0805 <- threshold_preprocess(ods_gpdr_0805)
ods_threshold_0806 <- threshold_preprocess(ods_gpdr_0806)
ods_threshold_0807 <- threshold_preprocess(ods_gpdr_0807)
ods_threshold_0808 <- threshold_preprocess(ods_gpdr_0808)

bindori_dataset_threshold <- threshold_preprocess(bindori_dataset_gpdr)
str(bindori_dataset_threshold)
# factor all the var types to factor except for the "weight" column
bindori_dataset_threshold_chr <- bindori_dataset_threshold %>% mutate(across(c(where(is.numeric), -weight), as.character))
print("factor done!!!")
str(bindori_dataset_threshold_chr)

# dummify the data, first we change them to factor
names(ods_threshold_0802)[2:90]==names(ods_threshold_0803)[2:90] # check
colnames_ods_datewise <- names(ods_threshold_0802)[2:90]
ods_threshold_0802[colnames_ods_datewise] <- lapply(ods_threshold_0802[colnames_ods_datewise] , factor)
ods_threshold_0803[colnames_ods_datewise] <- lapply(ods_threshold_0803[colnames_ods_datewise] , factor)
ods_threshold_0804[colnames_ods_datewise] <- lapply(ods_threshold_0804[colnames_ods_datewise] , factor)
ods_threshold_0805[colnames_ods_datewise] <- lapply(ods_threshold_0805[colnames_ods_datewise] , factor)
ods_threshold_0806[colnames_ods_datewise] <- lapply(ods_threshold_0806[colnames_ods_datewise] , factor)
ods_threshold_0807[colnames_ods_datewise] <- lapply(ods_threshold_0807[colnames_ods_datewise] , factor)
ods_threshold_0808[colnames_ods_datewise] <- lapply(ods_threshold_0808[colnames_ods_datewise] , factor)


col_names <- names(bindori_dataset_threshold_chr)[2:90]
bindori_dataset_threshold_chr[col_names] <- lapply(bindori_dataset_threshold_chr[col_names] , factor)

# preprocessed original data import and export
#-----------------------------------------------
export_path <- "/dss/dsshome1/0C/ru27req2/Master-Thesis-DifferentialPrivacy"
bindori_data_name <- "bindori_dataset_preprocessed_factor.rda"
ods_0802 <- "ods_preprocess_0802.rda"
ods_0803 <- "ods_preprocess_0803.rda"
ods_0804 <- "ods_preprocess_0804.rda"
ods_0805 <- "ods_preprocess_0805.rda"
ods_0806 <- "ods_preprocess_0806.rda"
ods_0807 <- "ods_preprocess_0807.rda"
ods_0808 <- "ods_preprocess_0808.rda"
save(ods_threshold_0802, file=paste(c(export_path, ods_0802), 
                                               collapse="/"))
save(ods_threshold_0803, file=paste(c(export_path, ods_0803), 
                                               collapse="/"))
save(ods_threshold_0804, file=paste(c(export_path, ods_0804), 
                                               collapse="/"))
save(ods_threshold_0805, file=paste(c(export_path, ods_0805), 
                                               collapse="/"))
save(ods_threshold_0806, file=paste(c(export_path, ods_0806), 
                                               collapse="/"))
save(ods_threshold_0807, file=paste(c(export_path, ods_0807), 
                                               collapse="/"))
save(ods_threshold_0808, file=paste(c(export_path, ods_0808), 
                                               collapse="/"))

save(bindori_dataset_threshold_chr, file=paste(c(export_path, bindori_data_name), 
                                               collapse="/"))
# we have the dataframe here named as "bindori_dataset_threshold_chr"

# # load("sdssdc1.rda")
# test_df <- read.csv(file = "C:/Users/ru27req/Master-Thesis-DifferentialPrivacy/2020-08-01_full.csv", sep = ",")
# ori_dataset_list <- list.files(pattern = "*_full.csv$") %>%
#                     map_df(~read_csv(.))

bindori_dataset <- as.data.frame(bind_rows(ori_dataset))
print("binding dataset is successful!")

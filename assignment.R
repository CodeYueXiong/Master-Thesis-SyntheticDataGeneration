# install  the required R packages
devtools::install_github("CaroHaensch/CTIS")
install.packages("readr")
library(CTIS)
library(readr)

# read in the original data
ori_data_0802 <- read_csv("./ori_data/2020-08-02_full.csv")
ori_data_0803 <- read_csv("./ori_data/2020-08-03_full.csv")
ori_data_0804 <- read_csv("./ori_data/2020-08-04_full.csv")
ori_data_0805 <- read_csv("./ori_data/2020-08-05_full.csv")
ori_data_0806 <- read_csv("./ori_data/2020-08-06_full.csv")
ori_data_0807 <- read_csv("./ori_data/2020-08-07_full.csv")
ori_data_0808 <- read_csv("./ori_data/2020-08-08_full.csv")

head(ori_data_0802)
ori_data_0802 
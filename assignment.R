# install  the required R packages
devtools::install_github("CaroHaensch/CTIS")
library(CTIS)


ori_data <- CTIS_open_data_country(indicator = "vaccine_acpt", 
                                   type = "daily", 
                                   country = "Germany",
                                   daterange = "20210501-20210503")

username <- "yue.xiong@stat.uni-muenchen.de"
password <- "967270Xgy@"

ori_data_0802 <- CTIS_microdata(username = username, 
                           password = password,
                           date = "2020-08-02")

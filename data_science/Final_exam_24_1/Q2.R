## asssignemnt 3rd
## 2번문제

setRepositories(ind = 1:7)

library("data.table")
library("dplyr")
library("caret")
library("class")



WORK_DIR <- "C:\\DataScience_R_Compliation\\data_science\\Rdata"
DATA_DIR <-"C:\\DataScience_R_Compliation\\data_science\\data"

#---------------------------------------------------------------

# 1. 데이터 불러오기

setwd(DATA_DIR) # 데이터 디렉토리 설정 


data <-data.frame(fread("Q2.tsv"))

setwd(WORK_DIR)



#---------------------------------------------------------------

# 2. 



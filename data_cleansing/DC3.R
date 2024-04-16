#----------------------
# 공부 날짜 : 4월 16일
# 내용 : 데이터 클렌징 3
# 설명 : fread 를 통해 데이터를 tibble 형태로 가져오고 여러 방법으로 데이터 관찰하기
#----------------------

setRepositories(ind = 1:7)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\Rdata"
DATA_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\data"

setwd(WORK_DIR)
getwd()

library(tidyverse) # tibble 사용하기 위한 패키지
library(data.table) # fileread, filewrite 에 필요

setwd(DATA_DIR)
#--------------------- 1. -----------------------



# head 가 T 이면 첫 row 가 head 가 된다.
data<- tibble(fread("small_data.csv",head = T, sep=","))
head(data)


data_tab <- tibble(fread("small_data.tsv", head = T, sep = "\t"))
head(data_tab)

# 위 코드 두줄과 아래 코드 두줄은 동일하다.
data_tab_1 <- fread("small_data.tsv", head=T, sep="\t") %>%  as.tibble()
head(data_tab)


#--------------------- 2. -------------------------

# data 보는 여러 방법
str(data)
glimpse(data) # tibble 데이터 관찰 (dataframe 도 가능)
head(data)
tail(data)
summary(data)

class(data$age)
class(data$height_m)
View(data)


# 이미지 세이브 전에 쓰던 데이터 디렉토리에서 원래 WORK_DIR 로 이동한후 image.save 하기
setwd(WORK_DIR)
save.image("DC3.Rdata")

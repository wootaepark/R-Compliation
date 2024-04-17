#----------------------
# 공부 날짜 : 4월 17일
# 내용 : 데이터 클렌징 6
# 설명 :  
#----------------------

setRepositories(ind = 1:7)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\Rdata"
DATA_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\data"

setwd(WORK_DIR)
getwd()

library(tidyverse) # tibble 사용하기 위한 패키지
library(data.table) # fileread, filewrite 에 필요


#------------------- 1. ---------------------

setwd(DATA_DIR)
# demo_data <- fread("yrbss_demo.csv", sep = ",") # 이 경우 dataframe 형식으로 가져옴

demo_data <- read_csv("yrbss_demo.csv") # 이경우는 tibble 형식으로 가져옴


# 여러 방법으로 데이터 관찰 해 보기
head(demo_data)
tail(demo_data)
glimpse(demo_data)

View(demo_data)
class(demo_data)

#------------------ 2. -----------------------

demo_data

demo_data %>% 
  filter(bmi>20) %>%  # 특정 조건 만족하는 row 가져오기
  select(record, grade, sex, bmi) # 특정 이름의 column 만 추출하기



sum(na.omit(demo_data$bmi)>35) # bmi 가 35이상인 row 의 개수

# 하지만 위와 같이 무조건 na.omit 을 이용하여 NA 를 가진 행을 삭제하는 것은 좋지 않다.

setwd(WORK_DIR)
save.image("DC6.Rdata")
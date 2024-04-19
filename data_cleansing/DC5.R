#----------------------
# 공부 날짜 : 4월 17일
# 내용 : 데이터 클렌징 5
# 설명 :  gather 함수를 이용하여 가로로 긴 데이터를 세로로 길도록 변환하고 클렌징 하기
#----------------------

setRepositories(ind = 1:7)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\Rdata"
DATA_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\data"

setwd(WORK_DIR)
getwd()

library(tidyverse) # tibble 사용하기 위한 패키지
library(data.table) # fileread, filewrite 에 필요

#---------------------- 1. ------------------------

untidy_data <- tibble(
  name = c("Ana", "Bob", "Cara"),
  wt_07_01_2018 = c(100, 150, 140),
  wt_08_01_2018 = c(104, 155, 138),
  wt_09_01_2018 = c(NA, 160, 142)
)

glimpse(untidy_data)
View(untidy_data)


# 가로로 긴 것을 세로로 길게 변경 
untidy_data

# 특정 행을 -로 넣어줌으로서 해당 행은 유지 가능

untidy_data %>% 
  gather(key = "date", value = "weight", -name) %>% 
  mutate(date = str_remove(date,"wt_"), date = dmy(date)) -> cleanUntidy


# dmy 를 이용해 <date> 형태로 바꾸지 않으면 아래의 결과는 NA 가 된다.

cleanUntidy
as.numeric(cleanUntidy$date)
as.factor(cleanUntidy$date)


#---------------------- 2. ----------------------

# 위 코드에서 추가로 날짜 정보를 년,월,일 로 separate 하기

# name 행은 그대로 둔다는 뜻으로 -name으로 gather 를 씀
untidy_data %>% 
  gather(key = "date", value = "weight", -name) %>% 
  mutate(date = str_remove(date, "wt_"), date = mdy(date)) %>% 
  separate(date, into = c("Year", "Month", "Day"), sep = "-")





setwd(WORK_DIR)
save.image("DC5.Rdata")


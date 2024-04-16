#----------------------
# 공부 날짜 : 4월 16일
# 내용 : 데이터 클렌징 2
# 설명 : data.frame 에서 향상된 성능을 가진 tibble 사용하기
#----------------------

setRepositories(ind = 1:7)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\Rdata"
DATA_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\data"

setwd(WORK_DIR)
getwd()

library(tidyverse) # tibble 사용하기 위한 패키지


#------------------- 1. ---------------------

# 기본적으로 data.frame 형태의 데이터 생성하는 법
data <- data.frame(name = c("Taewoo", "Minseok", "Chulsoo"),
                   rank = 1:3, #integer
                   age = c(34, 35, 36), #numeric
                   city = c("Seoul", "Sejong", "Suwon"))
View(data)
str(data)
data

#------------------ 2. -----------------------

# data.frame 과 선언하는 방법은 동일하다. 
# tibble 은 data.frame 을 확장한 아주 좋은 형태의 데이터 이다.

dataTibble <- tibble(name = c("Taewoo", "Minseok", "Chulsoo"),
                     rank = 1:3, #integer
                     age = c(34,35, 36), #numeric
                     city = c("Seoul", "Sejong", "Suwon"))

class(data)
class(dataTibble)
str(dataTibble)
View(dataTibble)


save.image("DC2.Rdata")

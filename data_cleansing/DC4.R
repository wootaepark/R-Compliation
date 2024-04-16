#----------------------
# 공부 날짜 : 4월 16일
# 내용 : 데이터 클렌징 4
# 설명 : tibble 데이터에서 mutate를 이용하여 행의 정보 클렌징 (불필요한 단위 정보 제거)
#----------------------

setRepositories(ind = 1:7)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\Rdata"
DATA_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\data"

setwd(WORK_DIR)
getwd()

library(tidyverse) # tibble 사용하기 위한 패키지
library(data.table) # fileread, filewrite 에 필요


#--------------------- 1. -----------------------

# tibble 데이터 생성

untidy_data <- tibble(
  name = c("Ana", "Bob", "Cara"),
  meds = c("advil 600mg 2xday", "tylenol 650mg 4xday", "advil 200mg 3xday")
)

untidy_data


#--------------------- 2. ------------------------

# 형태가 char 이므로 변화를 시켜야 한다.


# str_remove 를 통해 
untidy_data %>% 
  separate(col = meds, into = c("med_name", "dose_mg", "times_per_day"), sep=" ") %>% 
  mutate(times_per_day = as.numeric(str_remove(times_per_day, "xday"))) %>% 
  mutate(dose_mg = as.numeric(str_remove(dose_mg, "mg"))) -> untidy_data # -> 를 이용해 untidy_data 에 대입


#--------------------- 3. ------------------------

# tibble 데이터에 column, row 추가해보기 

untidy_data_2 <- tibble(
  name = c("Ana", "Bob", "Cara"),
  meds = c("advil 600mg 2xday", "tylenol 650mg 4xday", "advil 200mg 3xday")
)


# 아래는 2. 의 경우와 달리 mutate 안의 col name 이 다른 경우 이름에 해당하는 행을 추가하여 적용한다.
untidy_data_2 %>% 
  separate(col = meds, into = c("medName", "Dosage", "TimesPerDay"), sep = " ") %>% 
  mutate(TimesPerDay_2 = as.numeric(str_remove(TimesPerDay, "xday"))) ->untidy_data_2


View(untidy_data_2)


untidy_data_2 %>% 
  mutate(Dosage = as.numeric(str_remove(Dosage, "mg"))) -> clean_data

clean_data

clean_data$medName <- as.factor(clean_data$medName)
glimpse(clean_data)

save.image("DC4.Rdata")
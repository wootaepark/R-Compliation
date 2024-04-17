#----------------------
# 공부 날짜 : 4월 18일
# 내용 : 데이터 클렌징 7
# 설명 : 인덱스 찾기, 특정 조건에 따라 원하는 대로 데이터 형태 수정하기 (주의 해야함)
#----------------------

setRepositories(ind = 1:7)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\Rdata"
DATA_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\data"

setwd(WORK_DIR)
getwd()

library(tidyverse) # tibble 사용하기 위한 패키지
library(data.table) # fileread, filewrite 에 필요


#------------------- 1. ---------------------

A <- c(1,2,3,4,5,NA,NA)
B <- c(2,3,4)

A %in% B # A 안에 B의 내용이 있는지 (앞의 연산자 기준)

is.na(A) # NA 인지 확인하는 코드

which(is.na(A)) # NA 를 가진 A 내의 인덱스 반환

A[which(is.na(A))] <- "temp" # NA 값을 원하는 값으로 변환
A
glimpse(A)

#------------------- 2. ---------------------

setwd(DATA_DIR)
demo_data <- read_csv("yrbss_demo.csv") # fread의 경우가 head 포함 유무 등 자세히 설정 가능함

demo_data %>% 
  rename(id=record) # 특정 행의 이름을 변경

newdata <- demo_data %>% 
  mutate(height_m = sqrt(stweight/bmi)) # 새로운 행 추가

newdata %>% select(record, bmi, stweight, height_m) %>% view()

#logical 한 값으로 해당 조건 만족하면 TRUE, 아니면 FALSE 그리고 특정 행을 출력
demo_data %>%  mutate(bmi_high = (bmi>30)) %>% 
  select(bmi_high)


demo_data %>%  mutate(male = (sex == "Male")) %>% 
  select(sex,male)

# One-Hot-Encoding : 확률적으로 사ㅓ용할 수 있도록 변환 : Male 을 1로 하겠다는 의미
demo_data %>%  mutate(male = 1 * (sex == "Male")) %>% 
  select(sex, male)


# char 형의 등수 데이터를 numeric 형태의 숫자 데이터로 변환
demo_data %>% mutate(grade_num = as.numeric(str_remove(grade,"th"))) %>% 
  select(grade, grade_num)


#----------------------- 3. ---------------------------

demo_data2 <- demo_data %>% 
  mutate(
    bmi_group = (case_when(
      bmi < 18.5 ~ "underweigh'",
      bmi >= 18.5 & bmi <= 24.9 ~ "normal",
      bmi > 24.9 & bmi <= 29.9 ~ "overweight",
      bmi >29.9 ~ "obese"
    ))
  )

demo_data2 %>%  select(bmi, bmi_group) %>% head()

# continuouse RV를 catecorical 하게 변환하는 예제이지만 위험성이 아래와 같이 있다.
# 1. 편차가 발생 (실제 격차가 없어지는 현상 발생), 산포
# 2. 손실 가능 성 있음 (벡터로 그려보면 앎)

## 가끔 제한적으로 위와 같이 변경해야 하는 경우가 있다.




setwd(WORK_DIR)
save.image("DC7.Rdata")

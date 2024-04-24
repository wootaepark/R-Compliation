#----------------------
# 공부 날짜 : 4월 24일
# 내용 : 데이터 클렌징 8
# 설명 : 특정 행을 여러 행으로 쪼개고, 묶고, mutate의 여러 버전 이용
#----------------------

setRepositories(ind = 1:7)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\Rdata"
DATA_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\data"

setwd(WORK_DIR)
getwd()

library(tidyverse) # tibble 사용하기 위한 패키지
library(data.table) # fileread, filewrite 에 필요


#----------------- 1. ---------------------

setwd(DATA_DIR)
demo_data <- read_csv("yrbss_demo.csv")


demo_data %>% # 칼럼을 쪼개서 여러 행으로 표현하기
  separate(age, c("Age", "o1", "o2", "o3", "o4"),
           sep = " ") %>% 
  select(c("Age","o4")) # 특정 행만 가져오기


#---------------- 2. ---------------------

demo_data$sex
demo_data$grade

# 두 행을 하나로 묶기, mutate로 추후에 추가 가능
demo_data %>% 
  unite("sexgr", sex, grade, sep=":") %>% 
  select(sexgr)



#--------------- 3. ----------------------

demo_data %>% na.omit() # na 가 없는 row를 삭제하지만, 이는 데이터의 일관성 삭제, 사용하지 않는것 권장


#-------------- 4. --------------------

data_dups <- tibble(
  name = c("Ana", "Bob", "Cara", "Ana"),
  race = c("Hispanic", "Other", "White", "Hispanic")
)

data_dups

# 중복된 데이터 삭제
data_dups %>%  distinct()

# bmi 우선순위 정렬, 이후 같은 경우 stweight 기준으로 3개만 출력 (오름차순)
demo_data %>% arrange(bmi, stweight) %>% head(n=3)

# 내름차순의 경우 아래와 같다. (아래의 경우 bmi 내림차순, stdweight 오름차순)
demo_data %>% arrange(desc(bmi), stweight) %>%  head(n=10)

#------------- 5. ----------------------

# mutate 의 여러 버전 이용하기

demo_data %>% 
  glimpse()


# numeric 형인 경우, char 형으로 변경
demo_data %>% mutate_if(is.numeric, as.character) %>% glimpse()

# 행의 내용을 대문자로 변경 (char 형인 경우)
demo_data %>% mutate_if(is.character, toupper) %>% glimpse()
demo_data %>% mutate_if(is.double,round, digits=0) %>% glimpse() # 첫째자리 에서 반올림, is.numeric도 동일 동작


# if 와 달리 특정 부분만 적용하고 싶은 경우
demo_data %>% mutate_at(vars(age:grade), toupper) %>% glimpse()
demo_data %>% mutate_at(vars(bmi,stweight), log) # 자연로그 취함, (log2, log3 과 같이 쓸 수도 있다.)

# race 라는 이름을 가진 행에서 white 인 경우 true, 아니면 false 그리고 소문자화
demo_data %>% mutate_at(vars(contains("race")), str_detect, pattern = "White")  %>% 
  mutate_at(vars(contains("race")), tolower)


# 모든 경우 char 화
demo_data %>% glimpse()
demo_data %>% mutate_all(as.character) %>% glimpse()






save.image("DC8.Rdata")
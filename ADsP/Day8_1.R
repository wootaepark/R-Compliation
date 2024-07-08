####################################################
## 작성일 : 240708                                ##
## 일  차 : 8_1                                   ##
## 내  용 : 햇갈리는 코드 및 처음 보는 코드 위주  ##
####################################################

setRepositories(ind = 1:8)


WORK_DIR <- "C:\\DataScience_R_Compliation\\ADsP\\rdata"
setwd(WORK_DIR)


load("Day8_1.Rdata")

# 4. ------- plyr 패키지를 이용하기 -----

score <- data.frame(class = c('A', 'A', 'B', 'B'), math = c(50, 70, 60, 90), 
                    english = c(70,80,60,80))

library(plyr)

# 데이터 요약용 summarise 사용
ddply(score, 'class', summarise, math_avg = mean(math), ang_avg = mean(english))

# 데이터 확장용 transform 사용
ddply(score, 'class', transform, math_avg = mean(math), eng_avg = mean(english))

data <- data.frame(year = c(rep(2012,4), rep(2013,4)), month = c(1,1,2,2,1,1,2,2),
                   value = (c(3,5,7,9,1,5,4,6)))

# 2개 이상의 기준 변수를 두는 경우 
ddply(data, c("year", "month"), summarise, value_avg = mean(value))


#원하는 임의의 함수를 작성해서 사용한 경우
ddply(data, c("year", "month"), function(x){
  value_avg = mean(x$value)
  value_sd = sd(x$value)
  data.frame(avg_sd = value_avg / value_sd)
})



# 5. ------- data.table 패키지, 검색 능력 향상 ---------

library('data.table')

year <- rep(c(2012:2015), each = 12000000) # each 는 생략 가능 (12000000 만)
month <- rep( rep(c(1:12), each = 1000000), 4)
value <- runif(48000000) # uniform distribution 에 해당하는 값

DataFrame <- data.frame(year, month, value)
DataTable <- as.data.table(DataFrame)

# 데이터 프레임 검색 시간 측정정
system.time(DataFrame[DataFrame$year == 2012,])

# 데이터 테이블의 검색 시간 측정
system.time(DataTable[DataTable$year == 2012,])

# 키 값으로 설정된 칼럼, 표현식 이용한 검색시간 측정정
setkey(DataTable, year)
system.time(DataTable[J(2012)]) # J 는 2012를 선택하기 위한 인덱싱 도구



# save.image("Day8_1.Rdata") # 용량이 너무 크다 (800메가바이트)

####################################################
## 작성일 : 240708                                ##
## 일  차 : 8                                     ##
## 내  용 : 햇갈리는 코드 및 처음 보는 코드 위주  ##
####################################################

setRepositories(ind = 1:8)


WORK_DIR <- "C:\\DataScience_R_Compliation\\ADsP\\rdata"
setwd(WORK_DIR)


load("Day8.Rdata")




# 1. ---- reshape2 패키지의 melt 함수를 이용한 예시------

#install.packages("reshape")
library(reshape)
score <- data.frame(student_number = c(1,2,1,2), semester = c(1,1,2,2), math_score = c(60,90,70,90),
                    english_score = c(80,70,40,60))

score


# 특정 변수를 기준으로 녹여서 나머지 변수에 대한 세분화 된 데이터를 만들 수 있다.
melt(score, id = c("student_number", "semester"))


# 2. ---- cast 함수를 이용하여 melt 의 녹은 데이터를 요약을 위해 새롭게 가공 -----

melted_score <- melt(score, id = c("student_number", "semester"))





# 학생의 과목별 평균 점수
cast (melted_score ,student_number ~ variable, mean)

# 학생의 학기별 평균 점수
cast(melted_score, student_number ~ semester, mean )

# 학생의 과목별 최댓값 
cast(melted_score, student_number ~ variable, max)


# 3. ----- sqldf 패키지 사용하기 -----------

#install.packages("sqldf")
library(sqldf)


# 일반적인 sql 문을 사용할 수 있다.
sqldf('select * from score')

sqldf('select * from score where student_number = 1')

sqldf('select avg(math_score), avg(english_score) from score group by student_number')




save.image("Day8.Rdata")

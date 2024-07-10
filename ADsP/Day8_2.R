####################################################
## 작성일 : 240708                                ##
## 일  차 : 8_2                                   ##
## 내  용 : 햇갈리는 코드 및 처음 보는 코드 위주  ##
####################################################

setRepositories(ind = 1:8)


WORK_DIR <- "C:\\DataScience_R_Compliation\\ADsP\\rdata"
setwd(WORK_DIR)


load("Day8_2.Rdata")


# 6. ---- 탐색적 데이터 분석(EDA) -----

# head 쪽 3개 출력 (default 값 = 6)
head(iris,3)

# 기초통계량
summary(iris)

# 데이터 구조 파악
str(iris)



# 7. ---- 결측값 다루기 -----


# 단순 대치법
copy_iris <- iris
dim(copy_iris)
copy_iris[sample(1:150, 30), 1] <- NA # 30개의 결측값 생성
copy_iris <- copy_iris[complete.cases(copy_iris),]
dim(copy_iris)


# 평균 대치법
copy_iris <- iris
copy_iris[sample(1:150, 30), 1] <- NA
meanValue <- mean(copy_iris$Sepal.Length, na.rm = T) # 결측값 제외 평균 계산
copy_iris$Sepal.Length[is.na(copy_iris$Sepal.Length)] <- meanValue

# centrallImputation을 활용한 중앙값 대치
# install.packages('DMwR2')
library(DMwR2)
copy_iris[sample(1:150, 30), 1] <- NA
cpy_iris <- centralImputation(copy_iris)

# 테스트를 위한 결측값을 가진 iris 데이터 생성
# 단순 확률 대치법법
copy_iris <- iris
copy_iris[sample(1:150, 30), 1] <- NA
copy_iris <- knnImputation(copy_iris, k=10)

# 다중 대치법
copy_iris <- iris
copy_iris[sample(1:150, 30), 1] <-NA



#install.packages("Amelia")
library(Amelia)
iris_imp <- amelia(copy_iris, m=3, cs="Species")
copy_iris$Sepal.Length <- iris_imp$imputations[[3]]$Sepal.Length


# 8. ----- 이상값 판단. ------
data<-c(3,10,13,16,11,20,17,25,43)
boxplot(data, horizontal = T)



save.image("Day8_2.Rdata")

#----------------------
# 공부 날짜 : 4월 16일
# 내용 : 데이터 클렌징 1
# 설명 : 기본적인 데이터 프레임 생성 및 matrix 와의 차이점 
#----------------------

setRepositories(ind = 1:7)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\Rdata"
DATA_DIR <- "C:\\DataScience_R_Compliation\\data_cleansing\\data"

setwd(WORK_DIR)
getwd()


#---------------------- 1.--------------------


# 10000 개의 랜덤 normal distribution (평균 173.2, 표준편차 : 3.13^2 )
Height <- rnorm(10000, mean = 173.2, sd = 3.13^2) 

# histogram 그리는 함수
hist(Height)
class(Height)

#---------------------- 2.-------------------- 

# c() 는 같은 행으로 묶어 버리는 역할, 위의 Height 와 묶는다.
Gender <- as.factor(c(rep("Male", 5000), rep("Female", 5000)))
data <- data.frame(Height, Gender)
dim(data)

class(data[,1])
class(data$Height)
class(data[,2])
class(data$Gender)

data$Height[5]
data[5,1]
as.numeric(data$Gender)
data$Gender

class(Gender)
View(data)
str(data) # 객체의 구조를 보여주는 함수 str


#---------------------- 3. --------------------
dataMatrix <- as.matrix(data)
class(dataMatrix)
View(dataMatrix)

class(data[,2])
class(data$Gender)

# matrix 는 $로 접근 불가
class(dataMatrix[,2])
dataMatrix[,1]
dataMatrix[,2]

save.image("DC1.Rdata")

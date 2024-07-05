####################################################
## 작성일 : 240703                                ##
## 일  차 : 7_1                                   ##
## 내  용 : 햇갈리는 코드 및 처음 보는 코드 위주  ##
####################################################

setRepositories(ind = 1:8)


WORK_DIR <- "C:\\DataScience_R_Compliation\\ADsP\\rdata"
setwd(WORK_DIR)


load("Day7_1.Rdata")

# 12. -------------- 데이터 이름 변경 ---------------

m1 <- matrix(c(1:6), nrow = 2)
colnames(m1) <- c('c1','c2','c3') # col 이름 변경
rownames(m1) <- c('r1','r2')      # row 이름 변경경
m1

# column, row 이름을 불러온다.
colnames(m1)
rownames(m1)


df1 <- data.frame(x=c(1:3), y=c(4:6))
colnames(df1) <- c('c1', 'c2')
rownames(df1) <- c('r1', 'r2', 'r3')

df1



# 13. ------------- 데이터 추출 --------------------


# 아래 두 식은 같은 결과 
df1$c1
df1[,1]


# 14. -------------- 데이터 결합 -------------------

v1 <- c(1:3)
v2 <- c(4:6)
rbind(v1,v2) # row 방향 결합
cbind(v1,v2) # col 방향 결합


# 15. -------------- 반복문 -----------------------


# for 문

data <- c('a', 'b', 'c')
for (i in data){
  print(i)
}

# while 문

i <- 0
while(i<5){
  print(i)
  i<-i+1
}


# 조건문 생략


# 16. --------------- 사용자 정의 함수 ------------

comparedTo5 <- function(number){
  if(number < 5){
    print('number 는 5보다 작다.')
  }
  else if (number > 5){
    print('number 는 5보다 크다.')
  }
  else{
    print('number 는 5와 같다.')
  }
}

comparedTo5(10) # 함수 사용


# 17. -------------- 통계 분석 함수 --------------

strData<- 'This is a pen'
tolower(strData)
toupper(strData)
nchar(strData)
substr(strData, 9, 13) # 9부터 13 인덱스 까지
strsplit(strData, 'is') # is 기준으로 좌우로 쪼갠다.
grepl('pen', strData) # 주어진 문자가 존재하는지
gsub('pen', 'banana', strData) # 다른 문자로 대체



# 18. -------------- 벡터 연산 -------------------

v3 <- c(1,1,2,2,3,3,4,4,5)
v4 <- c(11:19)
length(v3)
cov(v3, v4) # 공분산
cor(v3, v4) # 상관계수
order(v4) # 단순한 순서
table(v3) # 각 수치의 개수

# 행렬 연산 패스



# 19. ------------ 데이터 탐색 ------------------

x<-c(1:12)
head(x,5)
tail(x,5)
quantile(x) # "수치 벡터" 의 4분위 수







save.image("Day7_1.Rdata")

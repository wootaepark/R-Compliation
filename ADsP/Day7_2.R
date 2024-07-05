####################################################
## 작성일 : 240703                                ##
## 일  차 : 7_2                                   ##
## 내  용 : 햇갈리는 코드 및 처음 보는 코드 위주  ##
####################################################

setRepositories(ind = 1:8)


WORK_DIR <- "C:\\DataScience_R_Compliation\\ADsP\\rdata"
setwd(WORK_DIR)


load("Day7_2.Rdata")

# 20. --------------  데이터 전처리 ----------------

df1 <- data.frame( x = c(1,1,1,2,2), y=c(2,3,4,3,3))
df2 <- data.frame( x = c(1,2,3,4), y = c(5,6,7,8))

subset(df1, x == 1) # df1 에서 해당 조건 만족하는 데이터 추출

merge(df1, df2, by=c('x')) # 두 데이터를 특정 공통된 열 기준으로 병합

apply(df1, 1, sum) # 1은 각 행에 함수를 적용
apply(df1, 2, sum)# 2는 각 열에 함수를 적용


# 21. ------------ 날짜 ------------------------

Sys.Date()
Sys.time()
as.Date("2024/07/05") # / 대신 -를 써도 동일

format(Sys.Date(), '%Y/%m/%d')# 사용자 지정 format
format(Sys.Date(), '%A') # 요일추출

unclass(Sys.time()) # 1970년 1월 1일로 부터 몇초가 흘렀는지
as.POSIXct(1720184262, origin = '1970-01-01') # 해당 날짜로부터 해당 초 지난 시간 추출



# 22. --------------- 산점도 -------------------


x <- c(1:10)
y <- rnorm(10)

length(y) # 10

plot(x,y, type = 'l', xlim = c(-2, 12), ylim = c(-3, 3), xlab = 'X axis', 
     ylab = 'Y axis', main = 'Test plot')

# 위의 그래프에서 선을 그려주는 역할 (그래프 없으면 코드 실행 불가)
abline(v = c(1,10), col = 'blue')




save.image("Day7_2.Rdata")

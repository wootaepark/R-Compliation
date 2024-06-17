## asssignemnt 3rd
## Q3, Q4, Q5

setRepositories(ind = 1:7)

library("data.table") # 데이터 불러오기 위함
library("dplyr")
library("caret")
library("class")


# 아래 두 패키지 설치 (서로다른 데이터 형의 데이터 불러오기 위함)
#install.packages("arrow")
#install.packages("fst")

library(arrow)
library(fst)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_science\\Rdata"
DATA_DIR <-"C:\\DataScience_R_Compliation\\data_science\\data"

setwd(WORK_DIR)
getwd()


# Q3 ---------------------------------------------

# 1.데이터 불러오기

setwd(DATA_DIR)

hospital1 <-data.frame(fread("MicrobiomeData_Hospital1.tsv"))
hospital2 <-data.frame(read_feather("MicrobiomeData_Hospital2.feather"))
hospital3 <-data.frame(read_fst("MicrobiomeData_Hospital3.fst"))



# 2. 데이터 병합하기
data <- rbind(hospital1, hospital2, hospital3)




# 3. Disease 정보를 맨 뒤 column 으로 옮기고 numericalize 한 disease 이름 행 추가
cleanData <- cbind(data[ , 2:ncol(data) ] , Disease = data[,1])
cleanData$Disease <- factor(cleanData$Disease)
cleanData$Disease_as_numeric <- as.numeric(factor(cleanData$Disease))




# 4. data 섞기
# Raw Data shuffling

randomIdx <- sample(1:nrow(cleanData))
cleanData <- cleanData[randomIdx,]
dim(cleanData)


















# Q4 ---------------------------------------------


# 1. Pvalue 구하기 (나뉘는 클래스가 2개가 아니라 여러개 이므로 kruskal test 이용)

Pval <- data.frame()

for (i in 1:(ncol(cleanData)-2)){
  Pval[i,1]<-kruskal.test(cleanData[,i] ~ cleanData$Disease, )$p.value
  Pval[i,2]<-colnames(cleanData[i])
  
  
}
colnames(Pval)<-c("Pvalue", "microbiome_info")


# 2. Pvalue 가 작은 순서대로 ordering


Pval <- Pval[order(Pval[,1]),] # ordering 
print(Pval)
head(Pval, 100)
View(Pval)
















# Q5 ---------------------------------------------

# 


setwd(WORK_DIR)
save.image("Q3_Q5.Rdata")

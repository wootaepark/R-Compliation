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
View(Pval)



# 3. Pval 에 따라 featrue 를 정렬 하고, index 정보 붙여서 출력
cleanedPval <- cbind(head(Pval, 100),index = row.names(head(Pval,100)))
cleanedPval$index <- as.numeric(cleanedPval$index)
View(cleanedPval)




# 4.새로운 데이터 프레임 생성 및 중요한 feature 만 골라서 다시 데이터 생성

MLdata <- data.frame(cleanData[cleanedPval$index], Disease = cleanData$Disease)
dim(MLdata)
View(MLdata)







# Q5 ---------------------------------------------


# 1. 10-cross validation

fitControl <- trainControl(
  method = "cv",
  number = 10)


# 2. knn_model 이용하기

# 100개의 feature 로 knn 모델링

# (1) knn 이용

knn_model <- train(Disease ~ ., 
                   data = MLdata, 
                   method = "knn", 
                   trControl = fitControl,
)

print(knn_model)




# (2) treebag 이용

treebag_model <- train(Disease ~ ., 
                    data = MLdata, 
                    method = "treebag",  
                    trControl = fitControl,
)

print(treebag_model)



# (3)glmnet
glmnet_model <- train(Disease ~ ., 
                       data = MLdata, 
                       method = "glmnet",  
                       trControl = fitControl,
)

print(glmnet_model)


# (5) C5.0


C50_model <- train(Disease ~ .,  # . 을 이용해서 꼭 모든 feature 를 모델링 할 필요 x, 
                   # filtered method, wrappered method 등 사용 
                        data = MLdata, 
                        method = "C5.0",  
                        trControl = fitControl,
)

print(C50_model)


setwd(WORK_DIR)
save.image("Q3_Q5.Rdata")

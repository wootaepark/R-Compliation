## asssignemnt 3rd
## 1번문제


library("data.table")
library("dplyr")
library("caret")
library("class")



WORK_DIR <- "C:\\DataScience_R_Compliation\\data_science\\Rdata"
DATA_DIR <-"C:\\DataScience_R_Compliation\\data_science\\data"


#---------------------------------------------------------------

# 1. 데이터 불러오기

setwd(DATA_DIR) # 데이터 디렉토리 설정 


trainset <-data.frame(fread("Q1_Train.txt"))
testset <- data.frame(fread("Q1_Test.txt"))

setwd(WORK_DIR)



#---------------------------------------------------------------



# 2. 수치 -> 문자화


trainset <- trainset %>% 
  mutate(
    Gender = factor(Gender,labels = c("Male", "Female")),
    Race = factor(Race, labels = c("white", "Balck or African Americn")),
    HaveCough = factor(HaveCough, labels = c("No", "Yes"))
  )

View(trainset)

testset <- testset %>% 
  mutate(
    Gender = factor(Gender,labels = c("Male", "Female")),
    Race = factor(Race, labels = c("white", "Balck or African Americn")),
    HaveCough = factor(HaveCough, labels = c("No", "Yes")),
    Disease = 0
  )

View(testset)






#---------------------------------------------------------------



# 3.Knn 모델링 (step 1)


trainset_ <- trainset[1:1000,] # 모든 데이터를 training 시 매우 오래걸림, 범위 조정 (sampling 이 필요할 수도 )

## 10-fold CV
fitControl <- trainControl(
  method = "cv",
  number = 10)

knn_model <- train(Disease ~ ., data = trainset_, 
                   method = "knn", 
                   trControl = fitControl)

contingency_table <- confusionMatrix(knn_model)
contingency_table



# -----------------------------------------------------
# 4. Feature Selection (step 2)




# ----------------------------------------------------



# tip : 선형 회귀 모델에서 모델을 평가하려면 원래 있던 feature 수의 +2가 필요하다






save.image("Q1.Rdata")

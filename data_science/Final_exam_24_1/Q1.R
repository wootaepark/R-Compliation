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


#----------------------------------------------------------------------


# 3. 데이터 클렌징

cleanData <- trainset[,2:14]
rownames(cleanData) <- trainset$ID


# 데이터 섞기 랜덤화
randomIdx <- sample(1:nrow(cleanData)) # 하나의 값이 아닌 여러 값을 가지는 matrix 이다.
cleanData <- cleanData[randomIdx,]
dim(cleanData)
View(cleanData)



#---------------------------------------------------------------



# 4. Knn 모델링 (step 1)



## 10-fold CV
fitControl <- trainControl(
  method = "cv",
  number = 10)

knn_model <- train(Disease ~ ., data = cleanData, 
                   method = "knn", 
                   trControl = fitControl,
                   tuneGrid = data.frame(k=3),
                   tuneLength=10)




# 예측하기 (자신의 데이터를 이용)

predictionResult <- predict(knn_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)





# -----------------------------------------------------
# -----------------------------------------------------




# 5. Feature Selection (step 2)




# ----------------------------------------------------



# tip : 선형 회귀 모델에서 모델을 평가하려면 원래 있던 feature 수의 +2가 필요하다






save.image("Q1.Rdata")

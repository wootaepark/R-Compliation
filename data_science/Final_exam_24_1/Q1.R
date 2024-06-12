## asssignemnt 3rd
## 1번문제

setRepositories(ind = 1:7)

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
# 나중에 Feature selection 을 위해 생략 해야 함


# trainset <- trainset %>% 
#   mutate(
#     Gender = factor(Gender,labels = c("Male", "Female")),
#     Race = factor(Race, labels = c("white", "Balck or African Americn")),
#     HaveCough = factor(HaveCough, labels = c("No", "Yes"))
#   )
# 
# View(trainset)
# 
# testset <- testset %>% 
#   mutate(
#     Gender = factor(Gender,labels = c("Male", "Female")),
#     Race = factor(Race, labels = c("white", "Balck or African Americn")),
#     HaveCough = factor(HaveCough, labels = c("No", "Yes")),
#     Disease = 0
#   )
# 
# View(testset)


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
# filtered method 쓸 예정

str(cleanData) 
cleanData$Disease <- factor(cleanData$Disease) # factorization

summary(cleanData$Disease) # 각 요소에 해당하는 Disease를 가지는 row 의 개수


## t.test 를 이용하여 각 featrue 의 pvalue 구하여 feature-selection 하기
Pval <- data.frame()
for (i in 1:12){
  Pval[i,1]<-t.test(cleanData[,i]~Disease, cleanData)$p.value
  Pval[i,2]<-colnames(cleanData[i])
  
}


Pval <- Pval[order(Pval[,1]),] # ordering 
print(Pval)
View(Pval)



# ----------------------------------------------------


# 6. 가장 적합한 knn 모델의 k값 찾기 

# feature selection 이후의 knn 모델 찾기


# k=3

Pval[1:3,]
knn_model_clean <- train(Disease ~ FEV1 + FVC + Age, data = cleanData, 
                   method = "knn", 
                   trControl = fitControl,
                   tuneGrid = data.frame(k=3),
                   tuneLength=10)


predictionResult_clean <- predict(knn_model_clean, newdata = cleanData)

table(predictionResult_clean, cleanData$Disease)


#k=5

Pval[1:5,]
knn_model_clean <- train(Disease ~ FEV1 + FVC + Age + Resting_SaO2 + Race, data = cleanData, 
                         method = "knn", 
                         trControl = fitControl,
                         tuneGrid = data.frame(k=5),
                         tuneLength=10)


predictionResult_clean <- predict(knn_model_clean, newdata = cleanData)


table(predictionResult_clean, cleanData$Disease)


# k=7
Pval[1:7,]
knn_model_clean <- train(Disease ~ FEV1 + FVC + Age + Resting_SaO2 + Race + HaveCough + HR, data = cleanData, 
                         method = "knn", 
                         trControl = fitControl,
                         tuneGrid = data.frame(k=7),
                         tuneLength=10)


predictionResult_clean <- predict(knn_model_clean, newdata = cleanData)


table(predictionResult_clean, cleanData$Disease)


# k=9
Pval[1:9,]
knn_model_clean <- train(Disease ~ FEV1 + FVC + Age + Resting_SaO2 + Race + HaveCough + HR + BMI + Weight_KG, data = cleanData, 
                         method = "knn", 
                         trControl = fitControl,
                         tuneGrid = data.frame(k=9),
                         tuneLength=10)


predictionResult_clean <- predict(knn_model_clean, newdata = cleanData)


table(predictionResult_clean, cleanData$Disease)



# k=11
Pval[1:11,]
knn_model_clean <- train(Disease ~ FEV1 + FVC + Age + Resting_SaO2 + Race + HaveCough + HR + BMI + Weight_KG + sysBP + Gender, data = cleanData, 
                         method = "knn", 
                         trControl = fitControl,
                         tuneGrid = data.frame(k=11),
                         tuneLength=10)


predictionResult_clean <- predict(knn_model_clean, newdata = cleanData)


table(predictionResult_clean, cleanData$Disease)


# ---------------------------------------------

# 7. 여러가지 model predicting 방법을 이용해보기 (knn 제외 9가지)



# knn 이용
knn_model <- train(Disease ~ FEV1 + FVC + Age, data = cleanData, 
                      method = "knn", 
                      trControl = fitControl,
                      tuneLength=10)


predictionResult <- predict(knn_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)





# ranger 이용

library("ranger")


ranger_model <- train(Disease ~ FEV1 + FVC + Age, data = cleanData, 
                   method = "ranger", 
                   trControl = fitControl,
                   tuneLength=10)


predictionResult <- predict(ranger_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)






# LogitBoost 이용


LogitBoost_model<- train(Disease ~ FEV1 + FVC + Age, data = cleanData, 
                      method = "LogitBoost", 
                      trControl = fitControl,
                      tuneLength=10)


predictionResult <- predict(LogitBoost_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)






# wsrf 이용
# install.packages("wsrf")
library("wsrf")



wsrf_model<- train(Disease ~ FEV1 + FVC + Age, data = cleanData, 
                         method = "wsrf", 
                         trControl = fitControl,
                         tuneLength=10)


predictionResult <- predict(wsrf_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)





#C5.0 이용

#install.packages("C50")
library("C50")

C50_model<- train(Disease ~ FEV1 + FVC + Age, data = cleanData, 
                   method = "C5.0", 
                   trControl = fitControl,
                   tuneLength=10)


predictionResult <- predict(C50_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)





#randomForest 이용

# install.packages("randomForest")
# install.packages("inTrees")
library("inTrees")
library("randomForest")


randomForest_model<- train(Disease ~ FEV1 + FVC + Age, data = cleanData,
                  method = "rf",
                  trControl = fitControl,
                  tuneLength=10)


predictionResult <- predict(randomForest_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)






# treebag 이용


treebag_model<- train(Disease ~ FEV1 + FVC + Age, data = cleanData, 
                           method = "treebag", 
                           trControl = fitControl,
                           tuneLength=10)


predictionResult <- predict(treebag_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)





# fda 이용

install.packages("fda")
library("fda")

# fda 모델링 (트리 수와 깊이 제한)

fda_model<- train(Disease ~ FEV1 + FVC + Age, data = cleanData,
                      method = "fda",
                      trControl = fitControl,
                      tuneLength=10)


predictionResult <- predict(fda_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)




# Earth 이용

library("earth")
earth_model<- train(Disease ~ FEV1 + FVC + Age, data = cleanData,
                       method = "earth",
                       trControl = fitControl,
                       tuneLength=10)


predictionResult <- predict(earth_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)





# rpart1SE 이용

rpart1SE_model<- train(Disease ~ FEV1 + FVC + Age, data = cleanData, 
                       method = "rpart1SE", 
                       trControl = fitControl,
                       tuneLength=10)


predictionResult <- predict(rpart1SE_model, newdata = cleanData)

table(predictionResult, cleanData$Disease)






resamps <- resamples(list(Ranger = ranger_model, C50 = C50_model, LogitBoost = LogitBoost_model, Earth = earth_model,
                          RandomForest = randomForest_model, Rpart1SE = rpart1SE_model, TreeBag = treebag_model, WSRF = wsrf_model,
                          KNN = knn_model, FDA = fda_model))
dotplot(resamps, metric = "Accuracy")



# -----------------------------------------
# 8. 7에서 가장 잘 나온 모델과 knn k=3 모델과 비교 이후 testset 을 예측하기, (step4)

#ranger
ranger_prediction <- predict(ranger_model, newdata = testset)

View(data.frame(ranger_prediction))



#knn-3

knn_prediction <- predict(knn_model, newdata = testset)

View(data.frame(knn_prediction))





# tip : 선형 회귀 모델에서 모델을 평가하려면 원래 있던 feature 수의 +2가 필요하다






save.image("Q1.Rdata")

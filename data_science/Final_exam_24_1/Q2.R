## asssignemnt 3rd
## 2번문제

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


data <-data.frame(fread("Q2.tsv"))

setwd(WORK_DIR)



#---------------------------------------------------------------

# 2. 데이터 클렌징



cleanData <- data[,2:ncol(data)]
rownames(cleanData) <- data$sampleName

cleanData<- cbind(cleanData, Disease = sub("_.*", "", data$sampleName))



# 데이터 클렌징
randomIdx <- sample(1:nrow(cleanData)) # 하나의 값이 아닌 여러 값을 가지는 matrix 이다.
cleanData <- cleanData[randomIdx,]
cleanData$Disease <- factor(cleanData$Disease)
dim(cleanData)


# 데이터 셔플

randomIdx <- sample(1:nrow(cleanData))
cleanData <- cleanData[randomIdx,]
dim(cleanData)


# pvalue 를 통한 filtered method 이용

Pval <- data.frame()

for (i in 1:(ncol(cleanData)-1)){
  Pval[i,1]<-kruskal.test(cleanData[,i] ~ cleanData$Disease, )$p.value
  Pval[i,2]<-colnames(cleanData[i])
  
  
}
colnames(Pval)<-c("Pvalue", "Feature")
Pval <- Pval[order(Pval[,1]),] # ordering 
print(Pval)
View(Pval)



# 2. Pvalue 가 작은 순서대로 ordering


Pval <- Pval[order(Pval[,1]),] # ordering 
print(Pval)
View(Pval)



# -------------------------------------------------------

# 모델 생성 

# knn 사용


fitControl <- trainControl(
  method = "cv",
  number = 10)


selected_features <- as.character(Pval$Feature[1:100])  # select 를 통해서 data를 축소


rf_model <- train(Disease ~ ., data = cleanData[, c(selected_features, "Disease")], 
                  method = "rf", 
                  trControl = fitControl)
print(rf_model)



# wrapper 이용해보기 




step(rf_model, direction = "forward")

















# Rshiny 이용해보기


library(shiny)
library(bslib)




# Part 1 : ui --------------------------
# 업로드 구현, prediction 모델, output

options(shiny.maxRequestSize = 100*1024^2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Random Forest Model with Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload TSV File",
                accept = c("text/tsv", "text/tab-separated-values", "text/plain", ".tsv")),
      numericInput("numFeatures", "Number of Features:", 100, min = 1, max = 1000),
      actionButton("runModel", "Run Model")
    ),
    
    mainPanel(
      textOutput("summary"),
      tableOutput("pvalTable"),
      plotOutput("modelPlot")
    )
  )
)


# Part 2 : server --------------------------------
# 모델 구현 


server <- function(input, output) {
  observeEvent(input$runModel, {
    req(input$file1)
    
    # 데이터 불러오기
    data <- fread(input$file1$datapath)
    
    cleanData <- data[,2:ncol(data)]
    rownames(cleanData) <- data$sampleName
    cleanData <- cbind(cleanData, Disease = sub("_.*", "", data$sampleName))
    
    set.seed(123)
    randomIdx <- sample(1:nrow(cleanData))
    cleanData <- cleanData[randomIdx,]
    cleanData$Disease <- factor(cleanData$Disease)
    
    Pval <- data.frame()
    for (i in 1:(ncol(cleanData)-1)) {
      Pval[i,1] <- kruskal.test(cleanData[,i] ~ cleanData$Disease)$p.value
      Pval[i,2] <- colnames(cleanData[i])
    }
    colnames(Pval) <- c("Pvalue", "Feature")
    Pval <- Pval[order(Pval[,1]),]
    selected_features <- as.character(Pval$Feature[1:input$numFeatures])
    
    modelData <- cleanData[, c(selected_features, "Disease")]
    
    fitControl <- trainControl(method = "cv", number = 10)
    rf_model <- train(Disease ~ ., data = modelData, 
                      method = "rf", 
                      trControl = fitControl)
    
    output$summary <- renderText({
      paste("Model Accuracy:", round(max(rf_model$results$Accuracy), 4))
    })
    
    output$pvalTable <- renderTable({
      head(Pval, 20)
    })
    
    output$modelPlot <- renderPlot({
      plot(rf_model)
    })
  })
}


shinyApp(ui = ui, server = server) # 이걸로 서버 실행 




#runExample("09_upload")     # file upload wizard, 이거 중요(기말)
#runExample("10_download")   # file download wizard, 이거 중요 (기말)



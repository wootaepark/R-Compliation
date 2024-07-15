# 농사로 api를 이용하여 얻은 csv 파일을 가져와서 
# 데이터를 정제하기 



DATA_DIR <- "C:\\DataScience_R_Compliation\\myPlanterior\\data"
WORK_DIR <-"C:\\DataScience_R_Compliation\\myPlanterior"

setwd(DATA_DIR)
# load("plantDetailSampling.Rdata")

data <- read.csv('plantDetailData_copied_1.csv')
cleanedData <- read.csv('plantRecommandData_cleaned.csv')

View(data)



setwd(DATA_DIR)
save.image('plantDetailSampling.Rdata')

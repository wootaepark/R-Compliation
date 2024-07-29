setRepositories(ind = 1:8)
DATA_DIR <- "C:\\DataScience_R_Compliation\\myPlanterior\\data"
WORK_DIR <- "C:\\DataScience_R_Compliation\\myPlanterior"

setwd(DATA_DIR)
load("plantRecommend.Rdata")


library(plumber)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(factoextra)

# ---- 핵심 코드 -----

# ---- 1. 데이터 입력 -----

originData <- read.csv('0728_cleanData.csv')
View(originData)

cleanedData <- data.frame(
  content_number = originData$content_number,
  plant_name = originData$plant_name,
  height = originData$max_height,
  level = as.factor(originData$level),
  water_need = as.factor(originData$water_need_name),
  purpose_1 = as.factor(originData$purpose_1),
  purpose_2 = as.factor(originData$purpose_2),
  temperature = as.factor(originData$temperature),
  sunright_1 = as.factor(originData$sunright_1),
  sunright_2 = as.factor(originData$sunright_2),
  sunright_3 = as.factor(originData$sunright_3)

)
View(cleanedData)

# 2. ---- 데이터 수정 (샘플링) ----

###################### NA 의 경우 0으로 대체함 ###################

# # 2-1 : 물 주기 수치화
mapping <- c("드물게"=10, "가끔"=20, "자주"=30)
cleanedData <- cleanedData %>%
  mutate(water_need = recode(water_need, !!!mapping))
#
# # 2-2 : 난이도 수치화
mapping <- c("초보자"=100, "경험자"=200, "전문가"=300)
cleanedData <- cleanedData %>%
  mutate(level = recode(level, !!!mapping))
#
# # 2-3 : 식물 목적 수치화
mapping <- c("잎보기식물"=10, "꽃보기식물"=20, "잎&꽃보기식물"=30,
             "선인장다육식물"=40, "열매보기식물"=50)
cleanedData <- cleanedData %>%
  mutate(purpose_1 = recode(purpose_1, !!!mapping)) %>%
  mutate(purpose_1 = as.numeric(purpose_1)) %>%
  mutate(purpose_1 = ifelse(is.na(purpose_1), 0, purpose_1)) %>%
  mutate(purpose_2 = recode(purpose_2, !!!mapping)) %>%
  mutate(purpose_2 = as.numeric(purpose_2)) %>%
  mutate(purpose_2 = ifelse(is.na(purpose_2), 0, purpose_2))
#
#
#
# # 2-4 : 요구 광도 수치화
mapping <- c("낮은 광도(300~800 Lux)"=10, "중간 광도(800~1,500 Lux)"=20,
             "높은 광도(1,500~10,000 Lux)"= 30)
cleanedData <- cleanedData %>%
  mutate(sunright_1 = recode(sunright_1, !!!mapping)) %>%
  mutate(sunright_1 = as.numeric(sunright_1)) %>%
  mutate(sunright_1 = ifelse(is.na(sunright_1), 0, sunright_1)) %>%
  mutate(sunright_2 = recode(sunright_2, !!!mapping)) %>%
  mutate(sunright_2 = as.numeric(sunright_2)) %>%
  mutate(sunright_2 = ifelse(is.na(sunright_2), 0, sunright_2)) %>%
  mutate(sunright_3 = recode(sunright_3, !!!mapping)) %>%
  mutate(sunright_3 = as.numeric(sunright_3)) %>%
  mutate(sunright_3 = ifelse(is.na(sunright_3), 0, sunright_3))
#
# # 온도 요구 수치화
#
mapping <- c("10~15"=10, "10~16"=10,"16~20" =20,
             "21~25"= 30, "21~26" = 30)
cleanedData <- cleanedData %>%
mutate(temperature = recode(temperature, !!!mapping)) %>%
  mutate(temperature = as.numeric(temperature)) %>%
  mutate(temperature = ifelse(is.na(temperature), 0, temperature))


# 사용자 입력 (예시)
user_input <- data.frame(
  level = 100,
  water_need = 20,
  purpose = 50,
  temperature = 20,
  sunright = 20
)



# 유사도 계산 함수
calculate_similarity <- function(data, user_input) {
  data %>%
    rowwise() %>%
    mutate(
      level_similarity = ifelse(level == user_input$level, 1, 0),
      water_similarity = ifelse(water_need == user_input$water_need, 1, 0),
      purpose_similarity = max(ifelse(purpose_1 == user_input$purpose, 1, 0), ifelse(purpose_2 == user_input$purpose, 1, 0)),
      temperature_similarity = ifelse(temperature == user_input$temperature, 1, 0),
      sunright_similarity = max(ifelse(sunright_1 == user_input$sunright, 1, 0), ifelse(sunright_2 == user_input$sunright, 1, 0), ifelse(sunright_3 == user_input$sunright, 1, 0)),
      total_similarity = 6 * level_similarity + 3 * purpose_similarity + 1 * water_similarity + 1 * temperature_similarity + 1 * sunright_similarity,
      similarity_percentage = (total_similarity / 12) * 100
    ) %>%
    arrange(desc(total_similarity), desc(similarity_percentage)) %>%
    select(content_number, plant_name, similarity_percentage, level, water_need, purpose_1, purpose_2, temperature, sunright_1, sunright_2, sunright_3) %>%
    head(5)
}

# 유사도 계산 및 추천 식물 출력
recommended_plants <- calculate_similarity(cleanedData, user_input)
print(recommended_plants)


setwd(DATA_DIR)
save.image("plantRecommend.Rdata")





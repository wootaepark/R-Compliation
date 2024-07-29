# 식물을 군집화 하는 코드



setRepositories(ind = 1: 8)
DATA_DIR <- "C:\\DataScience_R_Compliation\\myPlanterior\\data"
WORK_DIR <- "C:\\DataScience_R_Compliation\\myPlanterior"

setwd(DATA_DIR)
#load("plantClustering.Rdata")
#setwd(WORK_DIR)

library(plumber)
library(ggplot2)
library(ggfortify)
library(dplyr)
# ---- 핵심 코드 -----



# ---- 1. 데이터 입력 -----

originData <- read.csv('0724_cleanData.csv')
View(originData)

cleanedData <- 
  data.frame(content_number = originData$content_number,
             plant_name = originData$plant_name,
             height = originData$max_height, 
             scent = as.factor(originData$scent), 
             level = as.factor(originData$level), 
             water_need = as.factor(originData$water_need_name),
             purpose_1 = as.factor(originData$purpose_1),
             purpose_2 = as.factor(originData$purpose_2),
             leaf_shape = as.factor(originData$leaf_shape),
             leaf_color_1 = as.factor(originData$leaf_color_1),
             leaf_color_2 = as.factor(originData$leaf_color_2),
             leaf_color_3 = as.factor(originData$leaf_color_3),
             leaf_color_4 = as.factor(originData$leaf_color_4)
)
#View(cleanedData)

# 2. ---- 데이터 수정 (샘플링) ----

###################### NA 의 경우 0으로 대체함 ###################

# 2-1 : 물 주기 수치화
mapping <- c("드물게"=10,"가끔" = 20, "자주" = 30)
cleanedData <- cleanedData %>%
  mutate(water_need = recode(water_need, !!!mapping))
#cleanedData$scent <- mapping[cleanedData$scent]

# 2-2 : 난이도 수치화 
mapping <- c("초보자" = 100, "경험자" = 200, "전문가" = 300)
cleanedData <- cleanedData %>%
  mutate(level = recode(level, !!!mapping))

#cleanedData$level <- mapping[cleanedData$level]

# 2-3 : 식물 목적 수치화
mapping <- c("잎보기식물" = 10, "꽃보기식물" = 20, "잎&꽃보기식물" = 30,
             "선인장다육식물" = 40, "열매보기식물" = 50)
cleanedData <- cleanedData %>%
  mutate(purpose_1 = recode(purpose_1, !!!mapping)) %>%
  mutate(purpose_1 = as.numeric(purpose_1)) %>%
  mutate(purpose_1 = ifelse(is.na(purpose_1), 0, purpose_1))
cleanedData <- cleanedData %>%
  mutate(purpose_2 = recode(purpose_2, !!!mapping)) %>%
  mutate(purpose_2 = as.numeric(purpose_2)) %>%
  mutate(purpose_2 = ifelse(is.na(purpose_2), 0, purpose_2))


# 2-4 : 잎 모양 수치화
mapping <- c("둥금" = 1, "뾰족" = 2)
cleanedData <- cleanedData %>%
  mutate(leaf_shape = recode(leaf_shape, !!!mapping))


# 2-5 : 잎 색상 수치화
mapping <- c("녹색" = 10, "은색" = 20, "빨강색" = 30, "노란색" = 40, "혼합색" = 50, "기타" = 60)


cleanedData <- cleanedData %>%
  mutate(leaf_color_1 = recode(leaf_color_1, !!!mapping)) %>%
  mutate(leaf_color_1 = ifelse(is.na(leaf_color_1), 0, leaf_color_1))

cleanedData <- cleanedData %>%
  mutate(leaf_color_2 = recode(leaf_color_2, !!!mapping)) %>%
  mutate(leaf_color_2 = ifelse(is.na(leaf_color_2), 0, leaf_color_2))

cleanedData <- cleanedData %>%
  mutate(leaf_color_3 = recode(leaf_color_3, !!!mapping)) %>%
  mutate(leaf_color_3 = ifelse(is.na(leaf_color_3), 0, leaf_color_3))

cleanedData <- cleanedData %>%
  mutate(leaf_color_4 = recode(leaf_color_4, !!!mapping)) %>%
  mutate(leaf_color_4 = ifelse(is.na(leaf_color_4), 0, leaf_color_4))

# 2-6 : 향기 수치화
mapping <- c("없음" = 10, "거의 없음" = 20, "약함" =30, "중간" = 40, "강함" = 50)
cleanedData <- cleanedData %>%
  mutate(scent = recode(scent, !!!mapping)) %>%
  mutate(scent = ifelse(is.na(scent), 0, scent))

#View(cleanedData)



# ----- 3. clustering 진행 -------

# 범주형 열을 제거하고 수치형 데이터만 선택
numeric_data <- cleanedData[,3:ncol(cleanedData)]
numeric_data[is.na(numeric_data)] <- 0




# # 평균 구하기 함수

mean_nonzero <- function(x) {
  nonzero_values <- x[x != 0]  # 0이 아닌 값들만 선택
  if (length(nonzero_values) == 0) {
    return(NA)  # 모든 값이 0인 경우 NA 반환
  }
  return(mean(nonzero_values))  # 0이 아닌 값들의 평균 반환
}




# # # 각 행에 대해 평균 계산 (purpose)
numeric_data$purpose_avg <- apply(numeric_data[, c("purpose_1", "purpose_2")], 1, mean_nonzero)

# 각 행에 대해 평균 계산 (leaf_color)
numeric_data$leaf_color_avg <- apply(numeric_data[, c("leaf_color_1", "leaf_color_2",
                                                      "leaf_color_3", "leaf_color_4")], 1, mean_nonzero)

# 아래 numeric 행제거 코드는 반드시 순차적으로 진행되어야 한다.(주석 해제 금지)

numeric_data <- numeric_data[,-c(5:6)]
numeric_data <- numeric_data[,-c(6:9)]



# numeric 완성 (여기까지 코드가)




attributes <- names(numeric_data)  # numeric_data의 열 이름 가져오기



# 5. 새로운 데이터 포인트 정의 및 가중치 적용 (선택된 열만)
new_point <- data.frame(height = 150, scent = 20, level = 100, water_need = 10,
                        leaf_shape = 1,purpose_avg = 50, leaf_color_avg = 30 )
# 거의 없음 ,  초보자,  드물게, 둥금, 열매보기, 










# 데이터 스케일링 (K-means는 스케일링에 민감함)
scaled_data <- scale(numeric_data)

mean_data <- attr(scaled_data, "scaled:center")
sd_data <- attr(scaled_data, "scaled:scale")


scaled_new_point <- scale(new_point, center = mean_data, scale = sd_data)



# level이 같은 데이터 필터링
level_match_data <- numeric_data[numeric_data$level == new_point$level &
                      numeric_data$scent == new_point$scent |
                        numeric_data$leaf_shape == new_point$leaf_shape,]

if (nrow(level_match_data) == 0) {
  # level이 같은 데이터가 없으면 결과 없음
  closest_points <- data.frame()
} else {
  # 유클리드 거리 계산 함수
  euclidean_distance <- function(a, b) {
    sqrt(sum((a - b)^2))
  }
  
  # 새로운 데이터 포인트와 각 데이터 포인트 간의 거리 계산
  distances_to_points <- apply(level_match_data, 1, function(point) euclidean_distance(scaled_new_point, point))
  
  # 가장 가까운 5개 데이터 포인트 선택
  closest_points_indices <- order(distances_to_points)[1:9]
  closest_points <- originData[rownames(numeric_data) %in% rownames(level_match_data), ][closest_points_indices, ]
}
# 결과 출력
print(closest_points)







# --------------------

# # K-means 군집화
# set.seed(123)  # 재현성을 위해 시드 설정
# kmeans_result <- kmeans(scaled_data, centers = 10)  # 40개의 클러스터로 군집화
# 
# # 군집화 결과를 데이터에 추가
# cleanedData$cluster <- kmeans_result$cluster
# 
# # 결과 출력
# #View(cleanedData)
# 
# 
# # PCA 수행
# pca <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
# 
# #pca_loadings <- data.frame(pca$rotation)
# #print(pca_loadings)
# 
# # PCA 결과를 데이터 프레임에 추가
# pca_data <- data.frame(pca$x[, 1:2])  # 첫 두 주성분 선택
# pca_data$cluster <- as.factor(cleanedData$cluster)
# 
# # 2D 플롯 생성
# ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
#   geom_point(size = 3) +
#   labs(title = "K-means Clustering Results (PCA)",
#        x = "Principal Component 1",
#        y = "Principal Component 2") +
#   theme_minimal()
# 
# 
# 
# 
# # ----------- 4. 데이터 입력하고 유사도 가장 높은 것 5개 추출 ------------
# 
# # 새로운 데이터 포인트
# new_point <- c(height = 50, scent = 30, level = 100, water_need = 20, 
#                leaf_shape = 1, purpose_avg = 30, leaf_color_avg = 20)
# 
# new_point <- new_point * weight_factors
# 
# 
# 
# 
# # 유클리드 거리 계산 함수
# euclidean_distance <- function(a, b) {
#   sqrt(sum((a - b)^2))
# }
# 
# # 클러스터 중심
# centers <- kmeans_result$centers
# 
# # 새로운 데이터 포인트와 각 클러스터 중심 간의 거리 계산
# distances <- apply(centers, 1, function(center) euclidean_distance(new_point, center))
# 
# # 가장 가까운 클러스터 찾기
# closest_cluster <- which.min(distances)
# 
# # 클러스터에 속하는 데이터 포인트 찾기
# cluster_data <- numeric_data[kmeans_result$cluster == closest_cluster, ]
# 
# # 새로운 데이터 포인트와 각 데이터 포인트 간의 거리 계산
# distances_to_points <- apply(cluster_data, 1, function(point) euclidean_distance(new_point, point))
# 
# # 가장 가까운 5개 데이터 포인트 선택
# closest_points_indices <- order(distances_to_points)[1:5]
# closest_points <- cluster_data[closest_points_indices, ]
# 
# # 결과 출력
# print(closest_points)


# ---- 데이터 저장 -----


setwd(DATA_DIR)
save.image("plantClustering_0728.Rdata")

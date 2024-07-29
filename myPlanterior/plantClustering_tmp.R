# 필요한 패키지 로드
library(dplyr)
library(caret)

DATA_DIR <- "C:\\DataScience_R_Compliation\\myPlanterior\\data"
setwd(DATA_DIR)

# 원본 데이터 로드
originData <- read.csv('0724_cleanData.csv')

# 데이터 전처리
cleanedData <- data.frame(
  content_number = originData$content_number,
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

# NA 값을 적절히 처리하고 범주형 변수 수치화
mapping_water <- c("드물게" = 10, "가끔" = 20, "자주" = 30)
mapping_level <- c("초보자" = 100, "경험자" = 200, "전문가" = 300)
mapping_purpose <- c("잎보기식물" = 10, "꽃보기식물" = 20, "잎&꽃보기식물" = 30, "선인장다육식물" = 40, "열매보기식물" = 50)
mapping_leaf_shape <- c("둥금" = 1, "뾰족" = 2)
mapping_scent <- c("없음" = 10, "거의 없음" = 20, "약함" = 30, "중간" = 40, "강함" = 50)
mapping_leaf_color <- c("녹색" = 10, "은색" = 20, "빨강색" = 30, "노란색" = 40, "혼합색" = 50, "기타" = 60)

cleanedData <- cleanedData %>%
  mutate(water_need = recode(water_need, !!!mapping_water)) %>%
  mutate(level = recode(level, !!!mapping_level)) %>%
  mutate(purpose_1 = recode(purpose_1, !!!mapping_purpose)) %>%
  mutate(purpose_2 = ifelse(is.na(purpose_2), 0, recode(purpose_2, !!!mapping_purpose))) %>%
  mutate(scent = recode(scent, !!!mapping_scent)) %>%
  mutate(leaf_color_1 = recode(leaf_color_1, !!!mapping_leaf_color)) %>%
  mutate(leaf_color_2 = recode(leaf_color_2, !!!mapping_leaf_color)) %>%
  mutate(leaf_color_3 = recode(leaf_color_3, !!!mapping_leaf_color)) %>%
  mutate(leaf_color_4 = recode(leaf_color_4, !!!mapping_leaf_color)) %>%
  mutate(leaf_shape = recode(leaf_shape, !!!mapping_leaf_shape))

# 원핫 인코딩
dummy_vars <- dummyVars(~ leaf_color_1 + leaf_color_2 + leaf_color_3 + leaf_color_4, data = cleanedData)
encoded_data <- predict(dummy_vars, newdata = cleanedData) %>% as.data.frame()
encoded_data <- cbind(cleanedData[, c("height", "scent", "level", "water_need", "purpose_1", "purpose_2", "leaf_shape")], encoded_data)

# 평균 계산 함수 정의
mean_nonzero <- function(x) {
  nonzero_values <- x[x != 0]
  if (length(nonzero_values) == 0) return(NA)
  return(mean(nonzero_values))
}

# 평균 열 추가
encoded_data$purpose_avg <- apply(encoded_data[, c("purpose_1", "purpose_2")], 1, mean_nonzero)
encoded_data$leaf_color_avg <- apply(encoded_data[, grep("^leaf_color_", names(encoded_data))], 1, mean_nonzero)

# 새로운 데이터 포인트
new_point <- data.frame(
  height = 150, 
  scent = "없음", 
  level = "초보자", 
  water_need = "드물게", 
  purpose_1 = "잎보기식물", 
  purpose_2 = NA, 
  leaf_shape = "뾰족",
  leaf_color_1 = "녹색", 
  leaf_color_2 = "노란색", 
  leaf_color_3 = NA, 
  leaf_color_4 = NA
)

# 새로운 데이터 포인트 전처리
new_point <- new_point %>%
  mutate(
    water_need = recode(water_need, !!!mapping_water),
    level = recode(level, !!!mapping_level),
    purpose_1 = recode(purpose_1, !!!mapping_purpose),
    purpose_2 = ifelse(is.na(purpose_2), 0, recode(purpose_2, !!!mapping_purpose)),
    scent = recode(scent, !!!mapping_scent),
    leaf_shape = recode(leaf_shape, !!!mapping_leaf_shape)
  )

# 더미 변수 생성
all_colors <- unique(c(levels(cleanedData$leaf_color_1), levels(cleanedData$leaf_color_2), levels(cleanedData$leaf_color_3), levels(cleanedData$leaf_color_4)))
color_columns <- paste0("leaf_color_", 1:4, "_", all_colors)
new_point_dummies <- data.frame(matrix(0, nrow = 1, ncol = length(color_columns)))
names(new_point_dummies) <- color_columns

# 원핫 인코딩된 색상 열 설정
for (i in 1:4) {
  color_col <- paste0("leaf_color_", i, "_", new_point[[paste0("leaf_color_", i)]])
  if (color_col %in% names(new_point_dummies)) {
    new_point_dummies[[color_col]] <- 1
  }
}

# NA를 0으로 대체
new_point_dummies[is.na(new_point_dummies)] <- 0

# 기존 데이터와 동일한 열 구조 맞추기
missing_cols <- setdiff(names(encoded_data), names(new_point_dummies))
new_point_dummies[missing_cols] <- 0
new_point_dummies <- new_point_dummies[names(encoded_data)]

# 데이터 스케일링
scaled_data <- scale(encoded_data)
mean_data <- attr(scaled_data, "scaled:center")
sd_data <- attr(scaled_data, "scaled:scale")

scaled_new_point <- scale(new_point_dummies, center = mean_data, scale = sd_data)

# 유클리드 거리 계산
euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}

# level이 같은 데이터 필터링
level_match_data <- scaled_data[encoded_data$level == new_point$level, ]

if (nrow(level_match_data) == 0) {
  closest_points <- data.frame()
} else {
  distances_to_points <- apply(level_match_data, 1, function(point) euclidean_distance(scaled_new_point, point))
  closest_points_indices <- order(distances_to_points)[1:5]
  closest_points <- originData[encoded_data$level == new_point$level, ][closest_points_indices, ]
}

# 결과 출력
print(closest_points)

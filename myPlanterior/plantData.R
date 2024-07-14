


# 0. -------------- 기본 개발 환경 설정 ---------------------

#setRepositories(ind = 1:8)

#install.packages("dotenv")
#install.packages("xml2")
#install.packages("dplyr")
#install.packages("purrr")
#install.packages("httr")
library(dotenv)
library(xml2)
library(dplyr)
library(httr)
library(purrr)


#load("plantData.Rdata")

DATA_DIR <- "C:\\DataScience_R_Compliation\\myPlanterior\\data"
WORK_DIR <-"C:\\DataScience_R_Compliation\\myPlanterior"


# Rdata 불러오기
setwd(DATA_DIR)
load("plantData.Rdata")


# 1. ---------------------------- 이미지 URL 데이터 -----------------------------



# 데이터 api 로 가져오기 (.env 파일을 이요하기위해 wd 변경)

setwd(WORK_DIR)
dotenv::load_dot_env(file=".env")

api_key <- Sys.getenv("API_KEY")

response <- GET("http://api.nongsaro.go.kr/service/garden/gardenList"
                 , query = list(apiKey = api_key, numOfRows = 300))


xml_data <- content(response, as = "text")
xml_parsed <- read_xml(xml_data)








# XML 데이터를 데이터 프레임으로 변환하는 함수 정의
parse_item <- function(item) {
  컨텐츠번호 <- xml_text(xml_find_first(item, "cntntsNo"))
  식물명 <- xml_text(xml_find_first(item, "cntntsSj"))
  파일경로 <- strsplit(xml_text(xml_find_first(item, "rtnFileCours")), "\\|")[[1]]
  파일구분코드 <- strsplit(xml_text(xml_find_first(item, "rtnFileSeCode")), "\\|")[[1]]
  파일순번 <- strsplit(xml_text(xml_find_first(item, "rtnFileSn")), "\\|")[[1]]
  저장파일주소 <- strsplit(xml_text(xml_find_first(item, "rtnFileUrl")), "\\|")[[1]]
  이미지구분코드 <- strsplit(xml_text(xml_find_first(item, "rtnImgSeCode")), "\\|")[[1]]
  저장파일명 <- strsplit(xml_text(xml_find_first(item, "rtnStreFileNm")), "\\|")[[1]]
  썸네일파일명 <- strsplit(xml_text(xml_find_first(item, "rtnThumbFileNm")), "\\|")[[1]]
  썸네일파일주소 <- strsplit(xml_text(xml_find_first(item, "rtnThumbFileUrl")), "\\|")[[1]]
  
  # 각 리스트의 최대 길이 계산
  max_len <- max(
    length(파일경로), length(파일구분코드), length(파일순번), length(저장파일주소),
    length(이미지구분코드), length(저장파일명), length(썸네일파일명), length(썸네일파일주소)
  )
  
  # 각 벡터의 길이를 최대 길이로 맞춤
  tibble(
    컨텐츠번호 = rep(컨텐츠번호, max_len),
    식물명 = rep(식물명, max_len),
    파일경로 = rep(파일경로, length.out = max_len),
    파일구분코드 = rep(파일구분코드, length.out = max_len),
    파일순번 = rep(파일순번, length.out = max_len),
    저장파일주소 = rep(저장파일주소, length.out = max_len),
    이미지구분코드 = rep(이미지구분코드, length.out = max_len),
    저장파일명 = rep(저장파일명, length.out = max_len),
    썸네일파일명 = rep(썸네일파일명, length.out = max_len),
    썸네일파일주소 = rep(썸네일파일주소, length.out = max_len)
  )
}


xml_to_df <- function(xml_parsed) {
  items <- xml_find_all(xml_parsed, "//item")
  data <- map_df(items, parse_item)
  return(data)
}

imgData <- xml_to_df(xml_parsed)
print(imgData)

View(imgData)

#write.csv(imgData, "plantImgData.csv",row.names = T)



setwd(DATA_DIR)
save.image("plantData.Rdata")




# 0. -------------- 기본 개발 환경 설정 ---------------------

# setRepositories(ind = 1:8)

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
#load("plantDetailData.Rdata")


# 1. ---------------------------- 이미지 URL 데이터 -----------------------------

plant_detail_data <- data.frame()





# 데이터 api 로 가져오기 (.env 파일을 이용하기위해 wd 변경)

setwd(WORK_DIR)
dotenv::load_dot_env(file=".env")

api_key <- Sys.getenv("API_KEY")








# 새로운 생물 코드 실행 시 여기부터 실행하기기

new_response <- GET("http://api.nongsaro.go.kr/service/garden/gardenDtl"
                , query = list(apiKey = api_key, cntntsNo=14697))


new_xml_data <- content(new_response, as = "text")
new_xml_parsed <- read_xml(new_xml_data)


parse_item <- function(item) {
  tibble(
    컨텐츠번호 = xml_text(xml_find_first(item, "cntntsNo")),
    식물학_명 = xml_text(xml_find_first(item, "plntbneNm")),
    식물영_명 = xml_text(xml_find_first(item, "plntzrNm")),
    식물_유통명 = xml_text(xml_find_first(item, "distbNm")),
    과_명 = xml_text(xml_find_first(item, "fmlNm")),
    과_코드명 = xml_text(xml_find_first(item, "fmlCodeNm")),
    원산지정보 = xml_text(xml_find_first(item, "orgplceInfo")),
    자문정보 = xml_text(xml_find_first(item, "adviseInfo")),
    이미지_평가_링크_경로 = xml_text(xml_find_first(item, "imageEvlLinkCours")),
    성장높이정보 = xml_text(xml_find_first(item, "growthHgInfo")),
    성장넓이정보 = xml_text(xml_find_first(item, "growthAraInfo")),
    잎형태정보 = xml_text(xml_find_first(item, "lefStleInfo")),
    향기코드 = xml_text(xml_find_first(item, "smellCode")),
    향기코드_명 = xml_text(xml_find_first(item, "smellCodeNm")),
    독성정보 = xml_text(xml_find_first(item, "toxctyInfo")),
    번식시기정보 = xml_text(xml_find_first(item, "prpgtEraInfo")),
    기타시기정보 = xml_text(xml_find_first(item, "etcEraInfo")),
    관리수준코드 = xml_text(xml_find_first(item, "managelevelCode")),
    관리수준코드명 = xml_text(xml_find_first(item, "managelevelCodeNm")),
    성장속도코드 = xml_text(xml_find_first(item, "grwtveCode")),
    성장속도명 = xml_text(xml_find_first(item, "grwtveCodeNm")),
    성장온도코드 = xml_text(xml_find_first(item, "grwhTpCode")),
    성장온도코드명 = xml_text(xml_find_first(item, "grwhTpCodeNm")),
    겨울최저온도코드 = xml_text(xml_find_first(item, "winterLwetTpCode")),
    겨울최저온도코드명 = xml_text(xml_find_first(item, "winterLwetTpCodeNm")),
    습도코드 = xml_text(xml_find_first(item, "hdCode")),
    습도코드명 = xml_text(xml_find_first(item, "hdCodeNm")),
    비료정보 = xml_text(xml_find_first(item, "frtlzrInfo")),
    토양정보 = xml_text(xml_find_first(item, "soilInfo")),
    물공급봄코드 = xml_text(xml_find_first(item, "watercycleSprngCode")),
    물공급봄코드명 = xml_text(xml_find_first(item, "watercycleSprngCodeNm")),
    물공급여름코드 = xml_text(xml_find_first(item, "watercycleSummerCode")),
    물공급여름코드명 = xml_text(xml_find_first(item, "watercycleSummerCodeNm")),
    물공급가을코드 = xml_text(xml_find_first(item, "watercycleAutumnCode")),
    물공급가을코드명 = xml_text(xml_find_first(item, "watercycleAutumnCodeNm")),
    물공급겨울코드 = xml_text(xml_find_first(item, "watercycleWinterCode")),
    물공급겨울코드명 = xml_text(xml_find_first(item, "watercycleWinterCodeNm")),
    병충해_관리정보 = xml_text(xml_find_first(item, "dlthtsManageInfo")),
    특별_관리정보 = xml_text(xml_find_first(item, "speclmanageInfo")),
    기능성정보 = xml_text(xml_find_first(item, "fncltyInfo")),
    화분직경_대_정보 = xml_text(xml_find_first(item, "flpodmtBigInfo")),
    화분직경_중_정보 = xml_text(xml_find_first(item, "flpodmtMddlInfo")),
    화분직경_소_정보 = xml_text(xml_find_first(item, "flpodmtSmallInfo")),
    가로_대_정보 = xml_text(xml_find_first(item, "WIDTH_BIG_INFO")),
    가로_중_정보 = xml_text(xml_find_first(item, "widthMddlInfo")),
    가로_소_정보 = xml_text(xml_find_first(item, "widthSmallInfo")),
    세로_대_정보 = xml_text(xml_find_first(item, "vrticlBigInfo")),
    세로_중_정보 = xml_text(xml_find_first(item, "vrticlMddlInfo")),
    세로_소_정보 = xml_text(xml_find_first(item, "vrticlSmallInfo")),
    높이_대_정보 = xml_text(xml_find_first(item, "hgBigInfo")),
    높이_중_정보 = xml_text(xml_find_first(item, "hgMddlInfo")),
    높이_소_정보 = xml_text(xml_find_first(item, "hgSmallInfo")),
    볼륨_대_정보 = xml_text(xml_find_first(item, "volmeBigInfo")),
    볼륨_중_정보 = xml_text(xml_find_first(item, "volmeMddlInfo")),
    볼륨_소_정보 = xml_text(xml_find_first(item, "volmeSmallInfo")),
    가격_대_정보 = xml_text(xml_find_first(item, "pcBigInfo")),
    가격_중_정보 = xml_text(xml_find_first(item, "pcMddlInfo")),
    가격_소_정보 = xml_text(xml_find_first(item, "pcSmallInfo")),
    관리요구코드 = xml_text(xml_find_first(item, "managedemanddoCode")),
    관리요구코드명 = xml_text(xml_find_first(item, "managedemanddoCodeNm")),
    분류코드 = xml_text(xml_find_first(item, "clCode")),
    분류코드명 = xml_text(xml_find_first(item, "clCodeNm")),
    생육형태코드 = xml_text(xml_find_first(item, "grwhstleCode")),
    생육형태코드명 = xml_text(xml_find_first(item, "grwhstleCodeNm")),
    실내정원구성코드 = xml_text(xml_find_first(item, "indoorpsncpacompositionCode")),
    실내정원구성코드명 = xml_text(xml_find_first(item, "indoorpsncpacompositionCodeNm")),
    생태코드 = xml_text(xml_find_first(item, "eclgyCode")),
    생태코드명 = xml_text(xml_find_first(item, "eclgyCodeNm")),
    잎마크코드 = xml_text(xml_find_first(item, "lefmrkCode")),
    잎마크코드명 = xml_text(xml_find_first(item, "lefmrkCodeNm")),
    잎색상코드 = xml_text(xml_find_first(item, "lefcolrCode")),
    잎색상코드명 = xml_text(xml_find_first(item, "lefcolrCodeNm")),
    발화계절코드 = xml_text(xml_find_first(item, "ignSeasonCode")),
    발화계절코드명 = xml_text(xml_find_first(item, "ignSeasonCodeNm")),
    꽃색상코드 = xml_text(xml_find_first(item, "flclrCode")),
    꽃색상코드명 = xml_text(xml_find_first(item, "flclrCodeNm")),
    과일계절코드 = xml_text(xml_find_first(item, "fmldeSeasonCode")),
    과시계절코드명 = xml_text(xml_find_first(item, "fmldeSeasonCodeNm")),
    과일색코드 = xml_text(xml_find_first(item, "fmldecolrCode")),
    과일생코드명 = xml_text(xml_find_first(item, "fmldecolrCodeNm")),
    번식방법코드 = xml_text(xml_find_first(item, "prpgtmthCode")),
    번식방법코드명 = xml_text(xml_find_first(item, "prpgtmthCodeNm")),
    광요구도코드 = xml_text(xml_find_first(item, "lighttdemanddoCode")),
    광요구도코드명 = xml_text(xml_find_first(item, "lighttdemanddoCodeNm")),
    배치장소코드 = xml_text(xml_find_first(item, "postngplaceCode")),
    배치장소코드명 = xml_text(xml_find_first(item, "postngplaceCodeNm")),
    병충해코드 = xml_text(xml_find_first(item, "dlthtsCode")),
    병충해코드명 = xml_text(xml_find_first(item, "dlthtsCodeNm")),
    


  )
}

# XML 데이터를 데이터프레임으로 변환하는 함수
xml_to_df_new <- function(xml_parsed) {
  items <- xml_find_all(xml_parsed, "//item")
  data <- map_df(items, parse_item)
  return(data)
}

# 새로운 데이터프레임 생성
new_plant_detail_data <- xml_to_df_new(new_xml_parsed)


# 기존 데이터프레임에 새로운 데이터 추가
plant_detail_data <- bind_rows(plant_detail_data, new_plant_detail_data)



# 데이터프레임 출력
#print(plant_detail_data)
View(plant_detail_data)


setwd(DATA_DIR)
save.image("plantDetailData.Rdata")


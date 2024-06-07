######################################
## Lecture for Data Mining course
## Implemented by Minseok Seo
## 2024-04-29 ~ 
## linear, logistic, poisson
######################################

setRepositories(ind = 1:8)

#install.packages("mosaic")
#install.packages("ggfortify")
#install.packages("huxtable")
#install.packages("jtools")
#install.packages("latex2exp")
#install.packages("pubh")

library(MASS)
library(car)
library(broom)
library(tidyverse)
library(mosaic)
library(ggfortify)
library(huxtable)
library(jtools)
library(latex2exp)
library(pubh)
library(sjlabelled)
library(sjPlot)
library(sjmisc)


WORK_DIR <- "C:\\DataScience_R_Compliation\\data_science\\Rdata"
setwd(WORK_DIR)

# ---------------------- ex1 통계적 추정 -----------------------



# step 1 == categorical RV 를 숫자 -> 문자로 변환 (라벨 추가)


data(birthwt, package="MASS")
# 또는 data("birthwt")

birthwt <- birthwt %>% 
  mutate(
    smoke = factor(smoke,labels = c("Non-somker", "Smoker")),
    race = factor(race, labels = c("white", "African-Amerian", "other"))
  ) %>% 
  var_labels(
    bwt = 'Birth weight (g)',
    smoke = 'Smoking status',
    race = 'Race'
  )

View(birthwt)
head(birthwt)


# step 2 == 여러 툴을 이용하여 그래프 및 수치를 통한 그룹별 차이 통게 알아보기

birthwt %>% 
  group_by(race,smoke) %>% 
  summarise(
    n = n(), # 개수 세기
    Mean = mean(bwt, na.rm = TRUE), # na.rm 은 NA 값을 remove 함
    SD = sd(bwt, na.rm = TRUE),
    Median = median(bwt,na.rm = TRUE),
    CV = rel_dis(bwt)
    # CV (Coefficient of Variation) : 변동계수
    
  ) %>% 
  as_hux() %>% theme_pubh(1) # as_hux 는 huxtable 객체로 변환, 출력 형태 변경

birthwt %>% 
  box_plot(bwt~smoke, fill = ~race) %>% 
  axis_labs()


# 아래 코드를 통해서, 각 그룹별 차이 알 수 있다. (그래프 보다, 수치적으로)

birthwt %>% 
  box_plot(bwt ~ smoke, fill ~smoke)

t.test(bwt ~ smoke, data = birthwt)

birthwt %>% 
  gen_bst_df(bwt ~ race|smoke) %>% 
  as_hux() %>% theme_pubh(1)

str(birthwt) # 객체의 구조를 간단하게 보여주는 함수 




# --------------------- ex2) 선형회귀 --------------------------

model_norm <- lm(bwt ~ smoke + race , data = birthwt) # 모델 생성


# 모델 정리 
model_norm %>% summary()
model_norm %>% Anova()

model_norm %>% 
  summ(cofint = TRUE, model.info = TRUE) # 신뢰구간, 모델 정보를 포함



model_norm %>% 
  glm_coef(labels = model_labels(model_norm)) %>% 
  as_hux() %>% set_align(everywhere, 2:3, "right") %>% 
  theme_pubh(1) %>% 
  add_footnote(get_r2(model_norm), font_size = 9) # 결정계수 R2 추가


model_norm %>% glance() # 모델의 요약 통계

model_norm %>% 
  plot_model("pred", terms = ~race|smoke, dot.size=1.5, title="")


emmip(model_norm, smoke~race) %>% 
  gf_labs(y = get_label(birthwt$bwt), x="Race", col = "Smoking Status") + theme_classic()



save.image("section1_LinearModeling.Rdata")










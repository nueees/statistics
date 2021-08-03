# 1. Logistic Regression - 날씨 관련 요인 변수로 비(rain) 유무 예측 
# 1-(1) weather.csv 파일을 가져와서 데이터 구조를 파악한다. 
# - 문자형 변수 중 Date, WindGustDir, Windr, RainToday 변수를 제거한다.
# 1-(2) 변수 선택 및 더미 변수 생성
# - RainTomorrow 변수는 종속변수(y)가 되고, 로지스틱 회귀분석 결과 (0 or 1) 에 맞도록 더미변수로 생성한다.

library(dplyr);library(magrittr);library(reshape2);library(ggplot2)
library(zoo);library(lubridate);library(caret);library(tidyverse);
library(skimr);library(recipes);

rm(list=ls())
setwd("D:/ADP실기/5월빅분기실기/분류_날씨예측_0530/")
read.csv("weather.csv") %>% 
  dplyr::select(-c("Date", "WindGustDir","WindDir", "RainToday" )) -> weather

weather  %<>%  janitor::clean_names()  
# weather %>% str()
weather$rain_tomorrow  %<>% as.factor() 

weather$rain_tomorrow <- as.factor(weather$rain_tomorrow)
weather$rain_tomorrow <- weather$rain_tomorrow  %>%  as.factor() 
weather$rain_tomorrow  %<>% as.factor() 

# weather$rain_tomorrow 
# 1-(3) 학습데이터와 검정데이터 생성
# - train data와  test data를 7:3 비율로 한다.

# reciepe를 이용한 전처리 (결측치 포함)
weather %>%  is.na() %>%  sum() 
weather %>%  skimr::skim()

weather$sunshine         %>%  is.na() %>%  which()
weather$wind_gust_speed  %>%  is.na() %>%  which()

weather$sunshine  %>%  head(228) %>% tail(10)

rec <- recipe(rain_tomorrow ~ .,data = weather) %>%  
  step_impute_linear(sunshine, impute_with = imp_vars(sunshine)) %>% 
  step_impute_linear(wind_gust_speed, impute_with = imp_vars(wind_gust_speed)) %>% 
  # step_scale(all_predictors()) %>%
  # step_center(all_predictors()) %>% 
  prep(weather)

weather2 <- bake(rec, new_data = weather )

weather$sunshine  %>%  head(228) %>% tail(10)
weather2$sunshine  %>%  head(228) %>% tail(10)

inTrain <- createDataPartition(
  y = weather$rain_tomorrow,
  ## the outcome data are needed
  p = .7,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

training   <- weather2[ inTrain,]
testing    <- weather2[-inTrain,]

nrow(training)
nrow(testing)

weather$rain_tomorrow %>% table() %>% prop.table()
training$rain_tomorrow %>%  table() %>% prop.table()
testing$rain_tomorrow %>%  table() %>% prop.table()
# imbalanced data 이네.. 



ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)


set.seed(123)
glmFit <- train(
  rain_tomorrow ~ .,
  data = training,
  method = "glm",
  tuneLength = 15,
  trControl = ctrl,
  metric = "ROC"
)
glmFit # 학습 ROC 0.86

## ----plot----------------------------------------
# ggplot(glmFit)

## ----glmPred-----------------------------------------
glmClasses <- predict(glmFit, newdata = testing)
str(glmClasses)
glmProbs <- predict(glmFit, newdata = testing, type = "prob")
head(glmProbs)


## ----plsCM-------------------------------------------
confusionMatrix(data = glmClasses, testing$rain_tomorrow)

## ----ROCR이용-------------------------------------------
library(ROCR)
# glmRoc1 <- prediction(glmProbs$Yes, as.numeric(testing$rain_tomorrow) -1 )
glmRoc <- prediction(glmProbs$Yes, as.numeric(testing$rain_tomorrow))

glmRoc %>%  performance(., "tpr", "fpr")    %>% plot()
glmRoc %>%  performance(., "acc", "cutoff") %>% plot()

performance(glmRoc, "auc")@y.values # [1] 0.8614035

# pROC를 이용해야 함..



# 1-(4) 앞서 선택한 10개의 날씨 요인 변수를 활용하여 로지스틱 회귀모델을 생성한다.(train data 활용)
# 1-(5) 로지스틱 회귀모델로 예측치를 생성한다.(시그모이드 함수 0.5를 기준으로 rain 유무 판단)
# 1-(6) Confusion Matrix 활용한 분류정확도 계산
# - 모델의 예측치와 test data의 y 변수를 이용하여 Confusion Matrix를 생성하고, 이를 토대로 분류정확도를 계산한다.
# 1-(7) ROC curve를 이용한 모델 평가 
# -ROC Curve를 시각화하여 모델을 평가한다.(pROC 패키지 활용해보기)
# **성명.r 또는 성명.pdf 파일이 생성되도록 코드를 제출한다.


??caret


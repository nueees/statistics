library(dplyr); library(magrittr); library(ggplot2); library(reshape2); library(caret);
library(caretEnsemble);library(janitor);library(recipes); 

# 1. 자동차사고 데이터를 SVM, rf 두가지로 모델링: "accidentsnn.csv" 파일사용 
rm(list=ls())
setwd("D:/ADP실기/5월빅분기실기/분류_유니버셜은행/")
data <- read.csv("accidentsnn.csv") %>% clean_names()
data %>%  glimpse()

# *****RandomForest
# 1.1 전처리
# y: MAX_SEV_IR 상해/치명 여부: 0 무상해,1 상해, 2 치명 (target) >> factor화
data$max_sev_ir %<>%  as.factor()
# 1.2 훈련/검증용 데이터 분할 
# train.d : test.d = 7 : 3 (y클래스 비율에 따라 층화추출)

??caret

createDataPartition(max_sev_ir, )


set.seed(107)
inTrain <- createDataPartition(
  y = data$max_sev_ir,
  ## the outcome data are needed
  p = .7,
  ## The percentage of data in the
  ## training set
  list = FALSE
)


training <- data[ inTrain,]
testing  <- data[-inTrain,]

rfFit <- train(
  max_sev_ir ~ .,
  data = training,
  method = "rf",
  ## Center and scale the predictors for the training
  ## set and all future samples.
  # preProc = c("center", "scale")
)

rfFit 
rfFit %>%  plot()


rfClasses <- predict(rfFit, newdata = testing)
str(rfClasses)
#>  Factor w/ 2 levels "M","R": 2 1 1 1 2 2 1 2 2 2 ...
# plsProbs <- predict(plsFit, newdata = testing, type = "prob")
# head(plsProbs)
data.frame(
  predict = rfClasses, 
  actual  = testing$max_sev_ir
) %>% write.csv(., "rf_예측결과.csv")

confusionMatrix(data = rfClasses, testing$max_sev_ir)

# 1.3 랜덤포레스트 모델 구축
# 1.4 검증용 데이터로 예측하고, 혼동행렬로 결과 확인
# 1.5 rownames = c("predict", "actual")인 표를 작성하여 rf_예측결과.csv파일로 제출
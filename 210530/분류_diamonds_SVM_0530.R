# 2. Support Vector Machine -  ggplot2 패키지에 내장된 diamonds 데이터의 caret, x, y, z , depth, table 변수들을 기준으로 cut을 예측하는 SVM 모델을 생성하고 예측 결과를 평가한다. 
# (** 각 컬럼의 의미는 help(diamonds) 조회하여 참고 가능)
# - cut : quality of the cut (Fair, Good, Very Good, Premium, Ideal)
# - carat : weight of the diamond (0.2–5.01)
# - x : length in mm (0–10.74)
# - y : width in mm (0–58.9)
# - z : depth in mm (0–31.8)
# - depth : total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)
# - table: width of top of diamond relative to widest point (43–95)
# 
# 2-(1) ggplot2 패키지에 내장된 diamonds 데이터를 불러온다. 
library(dplyr);library(plyr);library(magrittr);library(caret);
rm(list=ls())
data(diamonds, package = "ggplot2")

# 2-(2) diamonds 데이터에서 cut 변수의 값을 기준으로 하여, 0 또는 1 값을 갖는 cut_status라는 파생변수를 생성하고 Factor 타입으로 변환한다. (cut의 값이 ”Premium” 또는 “Ideal” 일 경우는 cut_status가 1이 되고, cut의 값이 “Fair”, “Good”, “Very Good”일 경우 cut_status가 0이 된다.)
diamonds %>%  str()
diamonds %>%  colnames()

diamonds$cut_status <- 
ifelse(diamonds$cut == "Premium" | diamonds$cut == "Ideal", "PI", "FGV")
diamonds$cut_status %<>% as.factor()

# 2-(3) set.seed(1234)로 지정하고 train data와 test data를 7:3 비율로 생성한다.

set.seed(1234) 
inTrain <- createDataPartition(
  y = diamonds$cut_status,
  ## the outcome data are needed
  p = .7,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

training   <- diamonds[ inTrain,]
testing    <- diamonds[-inTrain,]

nrow(training)
nrow(testing)

diamonds  %>%  is.na() %>%  sum()

diamonds$cut_status %>% class()

diamonds$cut_status %>% table() %>% prop.table()
training$cut_status %>%  table() %>% prop.table()
testing$cut_status %>%  table() %>% prop.table()
# imbalanced data 이네.. 



# 2-(4) e1071 패키지로  caret, x, y, z , depth, table 변수들을 활용하여 cut_status를 예측하는 SVM 모델로 예측치를 생성한다

ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)
training %>%  colnames()
library(e1071)
set.seed(123)
svmFit <- train(
  cut_status ~ carat + x + y + z + depth + table,
  data = training,
  method = "svmLinear2",
  # tuneLength = 15,
  trControl = ctrl,
  metric = "ROC"
)
svmFit # 학습 ROC 
svmFit %>% plot() 

2^(-5:0)
names(getModelInfo())

# 2-(5) 모델의 예측치와 test data의 종속변수를 활용하여 Confusion Matrix를 생성하고, 분류정확도를 평가한다.
# 2-(6) ROC Curve를 시각화하여 모델을 평가한다.
# 
# **성명.r 또는 성명.pdf 파일이 생성되도록 코드를 제출한다.
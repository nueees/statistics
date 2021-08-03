library(dplyr); library(magrittr); library(tibble); library(caret)
rm(list=ls())
setwd("D:/ADP실기/5월빅분기실기/회귀_와인품질_0523")
wine <- read.csv("winequality-red.csv")
wine %>%  str()
# 레드와인의 11가지 화학성분들을 기록한 데이터로, 변수는 아래와 같음
# 1 - fixed acidity(고정산도)
# 2 - volatile acidity(휘발성산)
# 3 - citric acid(구연산)
# 4 - residual sugar(잔류당)
# 5 - chlorides(염화물)
# 6 - free sulfur dioxide(유리 이산화황)
# 7 - total sulfur dioxide(총 이산화황)
# 8 - density(밀도)
# 9 - pH(산도)
# 10 - sulphates(황산염)
# 11 - alcohol(알콜도수)
# Output variable (based on sensory data):
#   12 - quality (와인품질, score between 0 and 10)
wine %<>% add_column(index = 1:nrow(wine)) 
wine %<>% janitor::clean_names()
wine %>%  colnames() # indx, 목적변수, 명, 서, 등, 비
wine <- wine[, c(13,12,1:11)] 
wine %>%  skimr::skim()
library(funModeling)
wine %>%  plot_num()

idx <- createDataPartition(wine$quality, p = 0.8, list = FALSE)
train <-  wine[idx,  ]
test  <-  wine[-idx, ]

wine %>%  dim()
train %>%  dim()
test  %>%  dim()


trainControl(method="repeatedcv", number = 10 , repeats = 10) -> control1
# 전처리는 여기서 한 꺼 번에 해 보자. 
train[,3:13] %>% preProcess(., method = c("center", "scale")) -> preProcValues 
preProcValues %>%  predict(., train[,3:13] ) -> train[,3:13] 
preProcValues %>%  predict(.,  test[,3:13])  -> test[,3:13]

wine %>%  colnames()
train %>%  head()

###### 전체 변수 중 몇 개의 변수를 사용하는 것이 좋을까? rfe로 알아보자. 
rfeControl(functions = rfFuncs, # random forest 를 이용하여 변수 선택 하고자 함. 
           method = "cv", number = 10) -> control 
set.seed(123)
rfe(train[, 3:13], train[,2], # x, y 
    size = c(1:11), # x변수가 8개 이니까.. 
    rfeControl = control 
) -> rfeResult # 11개 다 사용하라고 하네요.  0.5880
rfeResult
rfeResult %>%  predictors()
# [1] "alcohol"              "sulphates"            "volatile_acidity"     "total_sulfur_dioxide"
# [5] "density"              "chlorides"            "citric_acid"          "fixed_acidity"       
# [9] "free_sulfur_dioxide"  "p_h"                  "residual_sugar" 
rfeResult %>%  plot()
################################

train %>% head()
test %>%  head()
set.seed(123)
trainControl(method="repeatedcv", number = 10 , repeats = 10) -> control1
train(quality ~ . , data = train[,-c(1)], method = "lm", trControl = control1 ) -> lmFit1
lmFit1
lmFit1 %>% varImp()
lmFit1 %>% summary()
lmFit1 %>% predict(., newdata = test) -> pred_lm 
library(forecast)
pred_lm %>% accuracy(., test$quality) # 0.71085 
# postResample(pred = pred_lm, obs = test$quality)

# 우선 기본 선형회귀부터 시작하자. 
set.seed(123)
train(quality ~ . , data = train[,-c(1)], method = "lm", trControl = control1, 
      preProcess=c("center", "scale")
) -> lmFit1
lmFit1
lmFit1 %>% varImp()
lmFit1 %>% summary()
lmFit1 %>% predict(., newdata = test) -> pred_lm 
library(forecast)

pred_lm %>% accuracy(., test$quality)
# 첫 번째 선현회귀모델 RMSE 0.6702843 

# 
# 1. train, test set으로 나누고
# 2. train set으로 와인품질 예측 모형(선형회귀, 규제-라쏘,릿지,엘라스틱넷- 각각)을 만든 후
# 3. 성능이 우수한 모델을 선택하고
# 4. test set을 적용하여 얻은 와인품질 예측값을 csv파일로 생성하시오.
# (스터디니까 소스코드도 성명_모델링_mmdd로 제출)
# (유의사항) 
# 적절한 데이터 전처리, Feature Engineering, 분류 알고리즘 사용, 초매개변수 최적화, 모형 앙상블 등이 수반되어야 한다.
# 성명. csv파일이 만들어지도록 코드를 제출한다.
# 제출한 모델의 성능은 RMSE 평가지표에 따라 채점한다.

# 두 번째 분석모형은 규제-라소 method = "lasso", library(elasticnet), tune para = fraction
set.seed(123)
train(quality ~ . , data = train[,-c(1)], method = "lasso", trControl = control1 
      ) -> lassoFit1
lassoFit1 # fraction = 0.9일 때 RMSE 0.64 최대 
lassoFit1 %>%  plot()
lassoFit1 %>% varImp()
lassoFit1 %>% summary()
lassoFit1 %>% predict(., newdata = test) -> pred_lasso 
pred_lasso %>% accuracy(., test$quality)
# 두 번째 lasso 회귀모델 0.7085596 

# 라쏘 fraction 튜닝해 볼까..
train(quality ~ . , data = train[,-c(1)], method = "lasso", trControl = control1, 
      preProcess=c("center", "scale"), 
      tuneGrid = expand.grid(fraction = c(0.88, 0.89, 0.90, 0.91, 0.92) )) -> lassoFit2
lassoFit2 # fraction = 0.92일 때 RMSE 0.6344921 최소  
lassoFit2 %>%  plot()
lassoFit2 %>% varImp()
lassoFit2 %>% summary()
lassoFit2 %>% predict(., newdata = test) -> pred_lasso2 
pred_lasso2 %>% accuracy(., test$quality) 
#                 ME      RMSE       MAE      MPE     MAPE
# Test set 0.02399904 0.6682763 0.5099849 -1.07672 9.382188

# 두 번째 분석모형은 규제-Ridge method = "ridge", library(elasticnet), tune para = lambda
set.seed(123)
train(quality ~ . , data = train[,-c(1)], method = "ridge", trControl = control1
) -> ridgeFit1

ridgeFit1 # lambda   = 1e-04일 때 RMSE 0.6461260 최대 
ridgeFit1 %>%  plot()
ridgeFit1 %>% varImp()
ridgeFit1 %>% summary()
ridgeFit1 %>% predict(., newdata = test) -> pred_ridge 
pred_ridge %>% accuracy(., test$quality)
# 두 번째 lasso 회귀모델 RMSE 0.6702771 # 더 나쁘네..
# 혹시 모르니 좀 더 세밀히 튜닝은 생략.. 

# 세 번째 분석모형은 규제-ElasticNet method = "enet", library(elasticnet), tune para = fraction, lambda
set.seed(123)
train(quality ~ . , data = train[,-c(1)], method = "enet", trControl = control1, 
      preProcess=c("center", "scale")
) -> enetFit1
enetFit1 #  fraction = 1 and lambda = 1e-04.일 때 RMSE 0.6461260 최대 
enetFit1 %>%  plot()
enetFit1 %>% varImp()
enetFit1 %>% summary()
enetFit1 %>% predict(., newdata = test) -> pred_enet 
pred_enet %>% accuracy(., test$quality)
# 두 번째 lasso 회귀모델 RMSE 0.6702771 # 똑 같네..
# 혹시 모르니 좀 더 세밀히 튜닝은 생략.. 

# 나무 계열(rpart) 한 번 시도 해 보자. 
set.seed(123)
train(quality ~ . , data = train[,-c(1)], method = "rpart", trControl = control1
) -> rpartFit1
rpartFit1 #  cp = 0.03586804 일 때 RMSE 0.7233148 최소  
rpartFit1 %>%  plot()
rpartFit1 %>% varImp()
rpartFit1 %>% summary()
rpartFit1 %>% predict(., newdata = test) -> pred_rpart
pred_rpart %>% accuracy(., test$quality) # 0.7071944

# 끝판왕 rf
# set.seed(123)
# library(doParallel)
# cl <- makePSOCKcluster(5)
# registerDoParallel(cl)
# train(quality ~ . , data = train[,-c(1)], method = "rf", trControl = control1, 
#       preProcess=c("center", "scale")
# ) -> rfFit1
# stopCluster(cl)
# rfFit1 #  mtry = 6 일 때 0.5752599 최소  
# rfFit1 %>%  plot()
# rfFit1 %>% varImp()
# rfFit1 %>% summary()
# rfFit1 %>% predict(., newdata = test) -> pred_rf
# pred_rf %>% accuracy(., test$quality) # 0.5937146

cbind(test$quality,pred_lm, pred_lasso, pred_lasso2, 
      pred_ridge, pred_enet, pred_rpart
      #, pred_rf
      ) -> result 

write.csv(result , "result_박정희.csv")

train %>%  dim()
test %>%  dim()


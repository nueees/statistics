# installed.packages() # 설치된 패키지 리스트 
# library(help = "caret") # 패키지내의 함수 리스트 
# 시험 환경 단축키 : ctrl + Alt 전체 입력 처리, ctrl + / : 전체 주석 처리 
options(scipen=100) # 근데.이게 안 먹는 거 같은데.. ㅠㅠㅠ
library(dplyr); library(caret); library(recipes); library(magrittr); 
rm(list=ls())
getwd()
setwd("C:/Users/Administrator/Documents/R/BDA/210613")

# read.csv() 안 되면, data.table::fread() 

left_join(
read.csv("X_train.csv") ->  X_train, 
read.csv("y_train.csv") ->  y_train ) %>%  
  mutate(index = "train") -> train 

read.csv("X_test.csv")  %>%  
  mutate(index = "test") ->  X_test

bind_rows(train, X_test) -> full 

full$gender   <-  ifelse(full$gender   == "1", "남성", "여성")
full$gender %<>%  as.factor()
full$index  %<>%  as.factor()

# recipe에 0으로 대체하는 건 찾을 수 없음.. ㅠㅠ 다 있는데.
full$환불금액 <-  ifelse(full$환불금액  %>% is.na(), 0, full$환불금액 )
full$환불금액 %>% is.na() %>%  sum()
# cbind(full$환불금액,full2$환불금액 ) %>%  View()
# cbind(full$환불금액,full2$환불금액 ) %>%  plot()

# 회귀로 바꾸기 위해서.. 총구매액을 예측해 보겠음. 
full %>% summary()
  recipe(총구매액 ~ ., data = full) %>%
    # step_impute_linear(환불금액, impute_with = imp_vars(환불금액)) %>%  # 선형회귀로 대체
    # step_knnimpute(환불금액) %>% 
    # step_lowerimpute(환불금액) %>%  
    # step_rm(index,cust_id ) %>% 
    step_modeimpute(gender) %>%  # gender는 최빈값으로 결측 보정 
    step_center( all_numeric(), -cust_id, -총구매액) %>%
    step_scale( all_numeric(), -cust_id, -총구매액) %>%  
    step_YeoJohnson( all_numeric(), -cust_id, -총구매액) %>% 
    # log/root < boxcox < 요한슨..
    # step_zv(all_numeric(), -cust_id) %>%
    #step_dummy(all_nominal(), -gender, -index, one_hot = TRUE) %>% 회귀는 자동으로 해 줌 
    prep() %>%  
    juice() -> full2
full2 %>% summary()
  full2 %>%  filter(index == "train") %>%  select(-index) -> train2
  full2 %>%  filter(index == "test")  %>%  select(-index) -> test2
  
  full2 %>%  is.na() %>%  sum()
  
  # conrol은 공통으로 (10 fold, repeated 3 times)
  ctrl <- trainControl(
    method = "repeatedcv",
    repeats = 3    
    )
  
  ##### lm 
  modelLookup("lm")
  #getModelInfo("lm")
  # model parameter     label forReg forClass probModel
  # 1   glm parameter parameter   TRUE     TRUE      TRUE
  set.seed(123)
  train(
    총구매액 ~ .,
    data = train2,
    method = "lm",
    trControl = ctrl,
    metric = "RMSE" # default가 RMSE 임. 
  ) -> lmFit 
  lmFit
  # RMSE       Rsquared  MAE     
  # 110103085  0.547667  63323691
  
  
  
  lmFit %>%  varImp()
  
  lmPred <- predict(lmFit, test2)
  lmPred %>% head()
  summary(lmPred)
 
  
  #### knn 
  modelLookup("knn")
  # model parameter      label forReg forClass probModel
  # 1   knn         k #Neighbors   TRUE     TRUE      TRUE
  set.seed(123)
  train(
    총구매액 ~ .,
    data = train2,
    method = "knn",
    trControl = ctrl, 
    metric ="Rsquared" # metric을 바꿀 수 있음 보여 주려고..
  ) -> knnFit 
  knnFit # 형편 없군..
  # k  RMSE       Rsquared     MAE      
  # 5  172853329  0.013059131  105818246
  # 7  171032819  0.007356625  105614333
  # 9  169694173  0.006670841  104901439
  
  # knnFit %>%  varImp()
  
  knnPred <- predict(knnFit, test2)
  # 그냥 무식하게 하나로 찍는 거네.. 
  knnPred %>% head()
 
  str(train2)
  rpartFit2 <- train(
    총구매액 ~ .,
    data = train2,
    method = "rpart",
    trControl = ctrl,
    metric = "RMSE"
  )
  rpartPred2 <- predict(rpartFit2, test2)
  rpartPred2 %>% head()
  #### rpart는 hold out 해 보자. 
  # metric 할 때 주의할게 있으니..
  
  isTrain <- createDataPartition(train2$총구매액, p = 0.7, list=FALSE)
  train2[isTrain,] -> training
  train2[-isTrain,] -> testing 
  
  library(rpart)
  set.seed(123)
  rpartFit <- train(
    총구매액 ~ .,
    data = training,
    method = "rpart",
    trControl = ctrl,
    metric = "RMSE"
  )
  rpartFit 
  # cp          RMSE       Rsquared   MAE     
  # 0.07864909  120127839  0.5034439  64945052
  # 0.14561804  128720078  0.4287761  68749090
  # 0.36505686  154220849  0.3235296  90901059
  rpartFit %>%  plot()
  rpartFit %>%  predict(testing) -> rpartPred 
  rpartPred %>% head()
  testing$총구매액 %>% head()
  rpartPred %>% caret::RMSE(testing$총구매액)
  rpartPred %>% caret::MAE(testing$총구매액)
  rpartPred %>% caret::R2(testing$총구매액)
  
  # 나머지는 분류처럼 여러 모델 그냥 돌리면 됨. 
  
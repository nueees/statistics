library(dplyr); library(magrittr); library(ggplot2); library(reshape2); library(caret);
library(caretEnsemble);library(janitor);library(recipes); 

rm(list=ls())
setwd("D:/ADP실기/5월빅분기실기/분류_유니버셜은행/")
read.csv("UniversalBank.csv") %>% 
  clean_names() %>%  
  select(-c("id", "zip_code")) -> data 

data$personal_loan       %<>%  as.factor()
data$education           %<>%  as.factor()
data$securities_account  %<>%  as.factor()
data$cd_account          %<>%  as.factor()
data$online              %<>%  as.factor()
data$credit_card         %<>%  as.factor()


recipe(personal_loan ~ ., data = train) %>%  
  step_scale(all_numeric()) %>%  
  step_center(all_numeric()) %>% 
  # step_BoxCox(all_numeric()) %>% 
  step_dummy(all_nominal(), -personal_loan, one_hot=FALSE) %>%  
  prep() %>%  
  juice() -> data2

# 2.4 훈련/검증용 데이터 분할
# 검증데이터30%, 필요시 y클래스 비율에 따라 층화추출


createDataPartition(data2$personal_loan, 
                    p = 0.7, 
                    list=FALSE) -> isTrain

data[isTrain,  ] -> train 
data[-isTrain, ] -> test


# library(funModeling)
# data %>%  plot_num()
# featurePlot(x = data2[,-7], 
#             y = data2$personal_loan, 
#             plot ="pairs", 
#             auto.key=list(columns = 3))


# 2.5 여러분류모델 구축: LR, tree, KNN, major voting
# major voting - "SOFT"

names(getModelInfo())
modelLookup("lda")

# ROC로 튜닝하기 위해서..
ctrl <- trainControl(
  # method = "repeatedcv", 
  # repeats = 3,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

caret::getModelInfo() %>%  names()
# 어떤 모델은 1,0 안 먹는 게 있어서.. 1은 positvie, 0은 negative 로 변경
train$personal_loan = ifelse(train$personal_loan== 1, "Yes", "No")

# glm : LR이 뭔지 모르겠음. 
# train %>%  str()
train(personal_loan ~ ., 
      data = train,
      method = "lda", 
      tuneLength  = 5, 
      trControl = ctrl,
      metric = "ROC"
) -> model_lda

#model_lda %>% plot() # : Tuning Parameter 없음 

# modelLookup("rpart")
train(personal_loan ~ ., 
      data = train,
      method = "rpart", 
      tuneLength  = 5
) -> model_rpart

model_rpart %>%  plot()


# modelLookup("knn")

train(personal_loan ~ ., 
      data = train,
      method = "knn", 
      tuneLength  = 5, 
      trControl = ctrl,
      metric = "ROC"
) -> model_knn

model_knn %>%  plot()




# 2.6 모델검정
# AUC검정, ROC곡선그리기 - 모든 모델의 값들을 다 표기하여, 성능개선이 얼마나 이루어 졌는지 표현

list( model_lda = model_lda, 
      model_rpart = model_rpart, 
      model_knn = model_knn) -> models

# models %>%  resamples() %>%  bwplot() 이게 안 먹히네
# metric을 ROC로 하면 .. reamples가 안 먹힌다는 건데. ㅠㅠ


model_lda    %>%  predict(., newdata = test, type = "prob") -> pred_lda
model_rpart  %>%  predict(., newdata = test, type = "prob") -> pred_rpart
model_knn    %>%  predict(., newdata = test, type = "prob") -> pred_knn


library(ROSE)
roc.curve(test$personal_loan, pred_lda[,2])
roc.curve(test$personal_loan, pred_rpart[,2], add.roc=TRUE)
roc.curve(test$personal_loan, pred_knn[,2], add.roc=TRUE)



# 2. 유니버셜은행 데이터를 여러 모델을(로지스틱회귀, 의사결정나무, KNN, Major voting)을 사용하여 분류하고, 모델성능을 비교 검증하는 코드를 작성 (큰 문제 2번은 확정, 밑에 소분류는 수정 중..)
# 2.1 "UniversalBank.csv" 파일사용, 여러 독립변수에 따른 개인대출제안 수락여부를 나타내고 있다.   
# 2.2 독립변수 X- 제외항목:ID, ZIPCode,PersonalLoan, 목적변수 y- PersonalLoan(개인대출 수락시1, 거절시0)
# 보기좋게 데이터 프레임 정리, 
# 2.3 전처리
# Education변수-OneHotEncoding
# 1,2,3 -> underGrad, Grad, advanced 팩터화 한후 가변수처리 진행
# 2.4 훈련/검증용 데이터 분할
# 검증데이터30%, 필요시 y클래스 비율에 따라 층화추출
# 2.5 여러분류모델 구축: LR, tree, KNN, major voting
# major voting - "SOFT"
# 2.6 모델검정
# AUC검정, ROC곡선그리기 - 모든 모델의 값들을 다 표기하여, 성능개선이 얼마나 이루어 졌는지 표현
# 2.7 최적화
# 하이퍼파라미터 튜닝(이걸 마지막에 하는게 맞나??)
# 그리드 서치를 사용한 ML 모델세부튜닝(각모델의 파라미터는 외우셨는지..)



# 2.2 독립변수 X- 제외항목:ID, ZIPCode,PersonalLoan, 목적변수 y- PersonalLoan(개인대출 수락시1, 거절시0)
# 보기좋게 데이터 프레임 정리, 
# 2.3 전처리
# Education변수-OneHotEncoding
# 1,2,3 -> underGrad, Grad, advanced 팩터화 한후 가변수처리 진행

# 2.7 최적화
# 하이퍼파라미터 튜닝(이걸 마지막에 하는게 맞나??)
# 그리드 서치를 사용한 ML 모델세부튜닝(각모델의 파라미터는 외우셨는지..)
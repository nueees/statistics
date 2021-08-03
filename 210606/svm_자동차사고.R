library(dplyr); library(magrittr); library(ggplot2); library(reshape2); library(caret);
library(caretEnsemble);library(janitor);library(recipes); 

# *****SVM
# 1.1 전처리
data %>%  glimpse()
# ALCHL_I: 음주여부 1 있음 2 없음 -> 1/0(yes/no) 변경
data$alchl_i %<>%  as.factor()
# PROFIL_I_R: 도로정보 0 기타 1 level1 -> 0/1(etc/level1)
data$profil_i_r %<>%  as.factor()
# SUR_COND: 도로의 노면 상태 1 건조 2 젖음 3 눈 4진흙 9 모름 ->1/2/3/4/9(dry/wet/snow/muddy/non)
data$sur_cond %<>%  as.factor()

# 나중에 recipe에서 수행하겠음. 
data$veh_invl %>%  glimpse()
data$veh_invl %>%  table()


# MAX_SEV_IR 상해/치명 여부: 0 무상해,1 상해, 2 치명 (target)
data$max_sev_ir %>%  glimpse()




# 1.2 훈련/검증용 데이터 분할
# 검증데이터30%, 필요시 y클래스 비율에따라 층화추출

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

# recipe로 전처리 

# VEH_INVL: 관련된 차량의 수  -> 표준화
# >> X: 범주형, 서열형 변수는 원핫인코딩을 통해 가변수화
# >> y: SVM에선 원핫인코딩없이 변수 하나로 처리할 것임 -> 0/1/2
recipe(max_sev_ir ~ ., data = training) %>%  
  step_dummy(all_predictors(), -all_numeric(), one_hot = TRUE) %>%  
  step_scale(veh_invl) %>%  
  prep() -> trained_rec 

training2 <- juice(trained_rec)
testing2  <- bake(trained_rec, new_data =  testing)


caret::getModelInfo() %>% names()
caret::modelLookup("svmRadial")

svmFit <- train(
  max_sev_ir ~ .,
  data = training2,
  method = "svmRadial",
  ## Center and scale the predictors for the training
  ## set and all future samples.
)

svmFit 
svmFit %>%  plot()

svmClasses <- predict(svmFit, newdata = testing2)
str(svmClasses)
#>  Factor w/ 2 levels "M","R": 2 1 1 1 2 2 1 2 2 2 ...
# plsProbs <- predict(plsFit, newdata = testing, type = "prob")
# head(plsProbs)

confusionMatrix(data = svmClasses, testing$max_sev_ir)


data.frame(
  predict = svmClasses, 
  actual  = testing$max_sev_ir
) %>% write.csv(., "svm_예측결과.csv")
weather <-  read.csv("weather.csv")
# 1. 날씨 관련 요인 변수로 비(rain) 유무 예측
# 1-(1) weather.csv 파일을 가져와서 데이터 구조를 파악한다.
dim(weather)
str(weather)
head(weather)

# 1-(2) 변수 선택 및 더미 변수 생성
# 문자형 변수 중 Date, WindGustDir, Windr, RainToday 변수를 제거한다.
# RainTomorrow 변수는 종속변수(y)가 되고, 로지스틱 회귀분석 결과 (0 or 1) 에 맞도록 더미변수로 생성한다.
weather_df <- weather[,-c(1,6,8,14)]
str(weather_df)

weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow =='No'] <- 0

str(weather_df$RainTomorrow)
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)

# 1-(3) 학습데이터와 검정데이터 생성
# train data와  test data를 7:3 비율로 한다.

idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]

dim(train)
dim(test)

# 1-(4) 앞서 선택한 10개의 날씨 요인 변수를 활용하여 로지스틱 회귀모델을 생성한다.(train data 활용)
weather_model <- glm(RainTomorrow ~ ., family = "binomial", data = train)   # y값이 0 or 1 이므로 binomial
weather_model
summary(weather_model)

# 1-(5) 로지스틱 회귀모델로 예측치를 생성한다.(시그모이드 함수 0.5를 기준으로 rain 유무 판단)

pred <- predict(weather_model, newdata = test, type = "response")
pred

result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred

table(result_pred)

# 1-(6) 모델 평가 
# 모델의 예측치와 test data의 y 변수를 이용하여 Confusion Matrix를 생성하고, 이를 토대로 분류정확도를 계산한다.
table(result_pred, test$RainTomorrow)
(87+7)/(87+7+10+5)

# 1-(7) ROC curve를 이용한 모델 평가 
# ROC Curve를 시각화하여 모델을 평가한다.(pROC 패키지 활용해보기)
library(pROC)
ROC <- roc(test$RainTomorrow, pred, levels = c(0,1), direction = "<")
plot.roc(ROC, col="royalblue",  
         print.auc=TRUE, 
         max.auc.polygon=TRUE,
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")


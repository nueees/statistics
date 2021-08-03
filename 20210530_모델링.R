# 1. Logistic Regression - 날씨 관련 요인 변수로 비(rain) 유무 예측 
# 1-(1) weather.csv 파일을 가져와서 데이터 구조를 파악한다. 
getwd()
setwd("C:/Users/Administrator/Documents/R/BDA/210530")
weather <- read.csv("weather.csv", stringsAsFactors=TRUE, encoding="UTF-8")
str(weather)
# 1-(2) 변수 선택 및 더미 변수 생성
# - 문자형 변수 중 Date, WindGustDir, Windr, RainToday 변수를 제거한다.
# - RainTomorrow 변수는 종속변수(y)가 되고, 로지스틱 회귀분석 결과 (0 or 1) 에 맞도록 더미변수로 생성한다.


wthr <- select(weather,-c("Date","WindGustDir","WindDir","RainToday"))
str(wthr)
wthr1 <- transform(wthr, RainTomorrow.D=ifelse(RainTomorrow=="Yes",1,0))
wthr1$RainTomorrow.D <- as.factor(wthr1$RainTomorrow.D)
wthr1 <- select(wthr1,-c("RainTomorrow"))
str(wthr1)
summary(wthr1)
# 결측치처리
wthr1 <- na.omit(wthr1)
summary(wthr1)


# 1-(3) 학습데이터와 검정데이터 생성
# - train data와  test data를 7:3 비율로 한다.
parts <- createDataPartition(wthr1$RainTomorrow.D, times = 1, p = 0.7, list = FALSE)
nrow(parts)
wthr.train <- wthr1[parts,]
wthr.test <- wthr1[-parts,]
nrow(wthr.train)
nrow(wthr.test)
# 1-(4) 앞서 선택한 10개의 날씨 요인 변수를 활용하여 로지스틱 회귀모델을 생성한다.(train data 활용)
str(wthr.train)
wthr1.m1 <- glm(RainTomorrow.D~., data=wthr.train, family=binomial)
summary(wthr1.m1)
#변수선택
wthr1.m2 <- step(wthr1.m1, direction="both")
summary(wthr1.m2)
#이탈도 확인
anova(wthr1.m2, test="Chisq")


# 1-(5) 로지스틱 회귀모델로 예측치를 생성한다.(시그모이드 함수 0.5를 기준으로 rain 유무 판단)

wthr1.pd1 <- predict(wthr1.m1, newdata=wthr.test[1:10], type="response")
wthr1.pd2 <- predict(wthr1.m2, newdata=wthr.test[1:10], type="response")
table(wthr.test$RainTomorrow.D, wthr1.pd1>0.5)
table(wthr.test$RainTomorrow.D, wthr1.pd2>0.5)


# 1-(6) Confusion Matrix 활용한 분류정확도 계산
# - 모델의 예측치와 test data의 y 변수를 이용하여 Confusion Matrix를 생성하고, 이를 토대로 분류정확도를 계산한다.

library(caret)

wthr1.pd1 <- as.data.frame(wthr1.pd1)
wthr1.pd2 <- as.data.frame(wthr1.pd2)

wthr1.pd1 <- wthr1.pd1 %>% mutate( grade <- ifelse( wthr1.pd1>0.5,1,0 ))
wthr1.pd2 <- wthr1.pd2 %>% mutate( grade <- ifelse( wthr1.pd2>0.5,1,0 ))

confusionMatrix(wthr.test$RainTomorrow.D, as.factor(wthr1.pd1$grade))
confusionMatrix(wthr.test$RainTomorrow.D, as.factor(wthr1.pd2$grade))



# 1-(7) ROC curve를 이용한 모델 평가 
# -ROC Curve를 시각화하여 모델을 평가한다.(pROC 패키지 활용해보기)
install.packages("pROC")
library(pROC)

wth1.pd1.roc <- roc(wthr.test$RainTomorrow.D, wthr1.pd1$grade)
wth1.pd2.roc <- roc(wthr.test$RainTomorrow.D, wthr1.pd2$grade)

plot.roc(wth1.pd1.roc)
plot.roc(wth1.pd2.roc)

# 2. Support Vector Machine -  ggplot2 패키지에 내장된 diamonds 데이터의 caret, x, y, z , depth, table 변수들을 기준으로 cut을 예측하는 SVM 모델을 생성하고 예측 결과를 평가한다. 
# (** 각 컬럼의 의미는 help(diamonds) 조회하여 참고 가능)
# - cut : quality of the cut (Fair, Good, Very Good, Premium, Ideal)
# - carat : weight of the diamond (0.2–5.01)
# - x : length in mm (0–10.74)
# - y : width in mm (0–58.9)
# - z : depth in mm (0–31.8)
# - depth : total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)
# - table: width of top of diamond relative to widest point (43–95)

install.packages("ggplot2")
library(ggplot2)
install.packages("e1071")
library(e1071)
install.packages("dplyr")
library(dplyr)

# 2-(1) ggplot2 패키지에 내장된 diamonds 데이터를 불러온다. 
# NA 확인
str(diamonds)
diamonds[!complete.cases(diamonds),]
# 2-(2) diamonds 데이터에서 cut 변수의 값을 기준으로 하여, 0 또는 1 값을 갖는 cut_status라는 파생변수를 생성하고 Factor 타입으로 변환한다. (cut의 값이 ”Premium” 또는 “Ideal” 일 경우는 cut_status가 1이 되고, cut의 값이 “Fair”, “Good”, “Very Good”일 경우 cut_status가 0이 된다.)

unique(diamonds$cut)

diamonds.2 <- diamonds %>% mutate(cut.grade=ifelse((cut=="Premium"|cut=="Ideal"),1,0))
str(diamonds.2)

diamonds.2 <- select(diamonds.2,-c("cut"))

# 2-(3) set.seed(1234)로 지정하고 train data와 test data를 7:3 비율로 생성한다.

install.packages("caret")
library(caret)
dtrainidx <- createDataPartition(diamonds.2$cut.grade, times=1, p=0.7, list=FALSE)
dtrain <- diamonds.2[dtrainidx,]
dtest <- diamonds.2[-dtrainidx,]
nrow(dtrain)
nrow(dtest)

# 2-(4) e1071 패키지로  caret, x, y, z , depth, table 변수들을 활용하여 cut_status를 예측하는 SVM 모델로 예측치를 생성한다

str(dtrain)
# gamma는 초평면의 기울기, cost는 과적합에 따른 비용
tune.svm(cut.grade~., data=dtrain, gamma=2^(-1:1), cost=2^(2:4))

dia.m <- svm(cut.grade~., data=dtrain, gamma=2^1, cost=2^2)
# 2-(5) 모델의 예측치와 test data의 종속변수를 활용하여 Confusion Matrix를 생성하고, 분류정확도를 평가한다.
yhat_test <- predict(dia.m, dtest[,1:9])
#diatable <- table(real=dtest[,10], predict=yhat_test)
confusionMatrix(real=dtest[,10], predict=yhat_test)

#wthr1.pd1 <- wthr1.pd1 %>% mutate( grade <- ifelse( wthr1.pd1>0.5,1,0 ))
#confusionMatrix(wthr.test$RainTomorrow.D, as.factor(wthr1.pd2$grade))
confusionMatrix(real=dtest[,10], predict=yhat_test)



# 2-(6) ROC Curve를 시각화하여 모델을 평가한다.
ROC(test=yhat_test, stat=dtest[,10], plot="ROC", AUC=T, main="diamonds SVM")


# **성명.r 또는 성명.pdf 파일이 생성되도록 코드를 제출한다.
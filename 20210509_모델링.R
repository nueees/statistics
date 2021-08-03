# 파일명 "Ch1106.교육수요자 만족도"에 저장된 데이터세트를 불러와
# train: test = 8:2로 홀드아웃 검증용 데이터셋를 만들고 train데이터를 이용하여
# '만족도'변수에 영향을 미치는 나머지 변수들에 대한 회귀모델을 두가지 버전으로 작성하고, (1 - 모든변수포함 모델, 2 - 변수 선택법에 따른 모델)
# 관련된 검증을 실시하여 표기하며(수치, 그래프)
# 두 회귀모형의 평가지표를 구하고(수치, 그래프)
# 더 예측력이 좋은 모델에 대한 위의 해석을 첨부하여 pdf로 제출하라.
# 
# R하는 사람은 참고....>>>>>
#   1. 데이터를 불러와서 구조를 확인 : read.csv(), str()

stsfd.df <- read.csv("Ch1106.교육수요자 만족도.csv", stringsAsFactors = FALSE)
str(stsfd.df)
# 2. 기본 통계치의 확인 : library(psych) describe(), {pairs.panels()-상관계수 대략 확인}
# - 기술통계치의 확인 (평균, 분산 등)
# - 시각화를 통한 개략적 데이터 분포 확인
describe(stsfd.df)
sapply(stsfd.df,var)

# 3. 홀드아웃용 교차검증 데이터셋 만들기 : library(caret), createDataPartition()
# - train: test=8:2 
# - setseed통해 모두 고정값 사용
set.seed(1)
stsfd.idx <-createDataPartition(stsfd.df$전반적만족도, times=1, p=0.8)
length(stsfd.idx$Resample1)
stsfd.train <- stsfd.df[stsfd.idx$Resample1,]
stsfd.test <- stsfd.df[-stsfd.idx$Resample1,]

# 4. train 회귀분석 모델을 두가지 버전 작성 : library(car)
# - 표준화 회귀계수의 이용 : library(lm.beta)
# - train model 1- 모든 변수를 포함
# - train model 2- 변수선택법(stepwise) : step( , direction = "both", )

stsfd.lm1 <- lm(stsfd.train$전반적만족도~., data=stsfd.train)
summary(stsfd.lm1)

step(stsfd.lm1, direction="both")
stsfd.lm2 <- lm(stsfd.train$전반적만족도~전공교육+홍보지역사회협력+이미지,data=stsfd.train)
summary(stsfd.lm2)

# 5. 회귀분석 가정 검정
par(mfrow=c(2,2))
plot(stsfd.lm2)
install.packages("gvlma")
library(gvlma)
gvlma(stsfd.lm2) #Skewness,Kurtosis->정규성,Link Function->선형성,Heteroscedasticity->이분산성

# # 등분산성: Scale-Location, ncvTest
ncvTest(stsfd.lm2) # p값 > 0.05 등분산성 만족
spreadLevelPlot(stsfd.lm2) #왜 고르지 않게 나오나..


# # 정규성: Nomal Q-Q, shapiro.test
qqnorm(stsfd.lm2$residuals)
qqline(stsfd.lm2$residuals)
shapiro.test(stsfd.lm2$residuals) # p값 > 0.05 정규성 만족

# # 선형성: Residuals vs Fitted, 


# # 독립성: durbinWatsonTest
durbinWatsonTest(stsfd.lm2)
#xtabs(stsfd.lm2$call$formula, data=stsfd.lm2$call$data)

# # 이상치검정 : Residuals vs Leverage(cook's distance) 4/n-k-1
outlierTest(stsfd.lm2)


 
# 6. 그래프 그리기 : plot
# - 실제값: plot, model1&2: 색 옵션을 다르게한 line으로 표현


par(mfrow=c(2,2))
library(ggplot2)
plot(stsfd.lm1, 1, col="green")
par(new=TRUE)
plot(stsfd.lm2, 1, col="red")
# 좀 이상한데 나중에 수정

# 7. 예측모형 평가지표 구하기 : library(forecast), accuracy()
# - model 1&2의 회귀모형 평가지표 ME, RMSE, MAE, MPE등을 구해본다

library(forecast)
pred.stsfd <- predict(stsfd.lm2,stsfd.test)
plot(stsfd.train$전반적만족도)
lines(stsfd.lm1$fitted.values, col="green")
par(new=TRUE)
lines(stsfd.lm2$fitted.values, col="red")
accuracy(stsfd.lm1)
accuracy(stsfd.lm2)

#덜했음...추가 필요

# 8. 더 예측력이 좋은 모델에 대한 해석을 첨부하여 pdf로 제출
# accuracy로 확인하면 MAE와 MASE가 lm2가 조금 더 좋음


pdf("20210509_모델링.pdf")
def.off


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
# 

wine <- read.csv("winequality-red.csv", sep=";", stringsAsFactors=TRUE, encoding="UTF-8")
head(wine)
str(wine)
#summary(wine)

# 1. train, test set으로 나누고
install.packages("caret")
library(caret)
trainidx <- createDatatrainidxition(wine$quality, times=1, p=0.7, list=FALSE)
train <- wine[trainidx,]
test <- wine[-trainidx,]
nrow(train)
nrow(test)

# 2. train set으로 와인품질 예측 모형(선형회귀, 규제-라쏘,릿지,엘라스틱넷- 각각)을 만든 후
install.packages("glmnet")
library(glmnet)
# grid <- 10^seq(10,-2, length=100)
# glmnet(x.train, y.train, alpha=0, lambda=grid)

x.train <- train[,-12]
y.train <- train[,12]
x.test <- test[,-12]
y.test <- test[,12]
train.cv <- cv.glmnet(x=as.matrix(x.train), y=y.train)
lambda.min <- train.cv$lambda.min
train.glmnet <- glmnet(x=x.train, y=y.train, lamda=lambda.min)

train.ridge <- glmnet(x=x.train, y=y.train, alpha=0, family="gaussian")
train.lasso <- glmnet(x=x.train, y=y.train, alpha=1, family="gaussian")
train.elnet <- glmnet(x=x.train, y=y.train, alpha=.5, family="gaussian")

# 10-fold cv for each alpha=0,0.1,...,0.9,,1.0
for(i in 0:10){
  assign(paste("fit",i,sep=""), cv.glmnet(x=x.train, y=y.train, type.measure="mse", alpha=i/10, family="gaussian"))
}

par(mfrow=c(3,2))
plot(train.lasso, xvar="lambda")
plot(fit10, main ="LASSO")
plot(train.ridge, xvar="lambda")
plot(fit0, main ="RIDGE")
plot(train.elnet, xvar="lambda")
plot(fit5, main ="Elastic Net")
help("predict.glmnet")
yhat0 <- predict.glmnet(train.ridge, s=fit0$lambda.1se, newx=as.matrix(x.test))
# yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
# yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
# yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
# yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(train.elnet, s=fit5$lambda.1se, newx=as.matrix(x.test))
# yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
# yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
# yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
# yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(train.lasso, s=fit10$lambda.1se, newx=as.matrix(x.test))

mse0 <- mean((y.test - yhat0)^2)
# mse1 <- mean((y.test - yhat1)^2)
# mse2 <- mean((y.test - yhat2)^2)
# mse3 <- mean((y.test - yhat3)^2)
# mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
# mse6 <- mean((y.test - yhat6)^2)
# mse7 <- mean((y.test - yhat7)^2)
# mse8 <- mean((y.test - yhat8)^2)
# mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)




##정희님 방법
install.packages("elasticnet")
library(elasticnet)
train.lasso <- train(quality~., data=train, method="lasso")
train.ridge <- train(quality~., data=train, method="ridge")
train.enet <-  train(quality~., data=train, method="enet")



# 3. 성능이 우수한 모델을 선택하고
# 라쏘(train.lasso)가 0.4038168로 우수
lasso_coef <- predict(train.lasso, s=fit10$lambda.1se, type="coefficients")

str(lasso_coef)
lasso_coef[-1,1]
attributes(lasso_coef[-1,1])$names

lasso_coef[, 1]
names(lasso_coef[-1, 1])

# x <- names(lasso_coef[-1, 1])
# class(x)
# as.list(x)
# lm(y.test~as.list(x), date=x.test)

test <- lm(y.test~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=x.test)

# 4. test set을 적용하여 얻은 와인품질 예측값을 csv파일로 생성하시오.








# (스터디니까 소스코드도 성명_모델링_mmdd로 제출)
# (유의사항) 
# 적절한 데이터 전처리, Feature Engineering, 분류 알고리즘 사용, 초매개변수 최적화, 모형 앙상블 등이 수반되어야 한다.
# 성명. csv파일이 만들어지도록 코드를 제출한다.
# 제출한 모델의 성능은 RMSE 평가지표에 따라 채점한다.
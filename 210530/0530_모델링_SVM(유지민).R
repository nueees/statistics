# 2. Support Vector Machine -  ggplot2 패키지에 내장된 diamonds 데이터의 caret, x, y, z , depth, table 변수들을 기준으로 cut을 예측하는 Support Vector Machine 모델을 생성하고 예측 결과를 평가한다. 
# 2-(1) ggplot2 패키지에 내장된 diamonds 데이터를 불러온다. 
library(ggplot2)
data("diamonds")
help(diamonds)
df <- diamonds
str(df)


# 2-(2) diamonds 데이터에서 cut 변수의 값을 기준으로 하여, 0 또는 1 값을 갖는 cut_status라는 파생변수를 생성하고 Factor 타입으로 변환한다.
# (cut의 값이 ”Premium” 또는 “Ideal” 일 경우는 cut_status가 1이 되고, cut의 값이 “Fair”, “Good”, “Very Good”일 경우 cut_status가 0이 된다.)
df$cut_status <- NA
df$cut_status[df$cut == "Premium"|df$cut == "Ideal"] = 1
df$cut_status[df$cut == "Fair"|df$cut == "Good"|df$cut == "Very Good"] = 0

class(df$cut_status)
df$cut_status <- as.factor(df$cut_status)
levels(df$cut_status)
table(df$cut_status)

# 2-(3) set.seed(1234)로 지정하고 train data와 test data를 7:3 비율로 생성한다.
# 방법 1 
set.seed(1234)
idx <- sample(nrow(df), nrow(df)*0.7)
train <- df[idx, ]
test <- df[-idx, ]

nrow(train) + nrow(test)
dim(train)
dim(test)

# 방법 2 - caret 패키지 이용
library(caret)

trainData <- createDataPartition( y = df$cut_status, p = 0.7, list = FALSE)
train <- df[trainData, ]
test <- df[-trainData, ]
dim(train)
dim(test)

# 2-(4) e1071 패키지로  caret, x, y, z , depth, table 변수들을 활용하여 
# cut_status를 예측하는 SVM 모델로 예측치를 생성한다
library(e1071)
svm.model <- svm(cut_status ~ carat + depth + table + x + y + z,
                 data = train)
summary(svm.model)

# 커널에 따른 기본형(radial), 선형(linear), 다항식(polynomia)에 대한 parameter 튜닝 
result <- tune.svm(cut_status~., data = train, gamma = 2^(-5:0), cost = 2^(0:4),
                   kernel = "radial") 
result$best.parameters

result1 <- tune.svm(cut_status~., data = train, cost = 2^(0:4),
                   kernel = "linear") 
result1$best.parameters

result2 <- tune.svm(cut_status~., data = train, cost = 2^(0:4), degree = 2:4,
                    kernel = "polynomia") 
result2$best.parameters

# 2-(5) 모델의 예측치와 test data의 종속변수를 활용하여 Confusion Matrix를 생성하고, 분류정확도를 평가한다.
pred <- predict(svm.model, train)
table(pred, train$cut_status)

t.result <- predict(svm.model, test)
table(t.result, test$cut_status)

confusionMatrix(t.result, test$cut_status)

# 2-(6) ROC Curve를 시각화하여 모델을 평가한다.
library(pROC)

pre <- as.numeric(predict(svm.model, df))
svmROC <-roc(df$cut_status, pre, levels = c(0,1), direction = "<")
plot.roc(svmROC, 
         print.auc = T, 
         col="red",
         max.auc.polygon=TRUE,
         print.auc.y = .3,
         legacy.axes = TRUE,
         auc.polygon=TRUE,
         auc.polygon.col="yellow",
         lty=2)



getwd()
setwd("C:/Users/Administrator/Documents/R/BDA/210613")

X_train <- read.csv("X_train.csv", stringsAsFactors=TRUE, header=TRUE,encoding="EUC-KR")
Y_train <- read.csv("Y_train.csv", stringsAsFactors=TRUE, encoding="UTF-8")
X_test <- read.csv("X_test.csv", stringsAsFactors=TRUE, encoding="EUC-KR")

str(X_train)
str(Y_train)
str(X_test)

install.packages("caret")
library(caret)
library(dplyr)
library(plyr)

X_train.0 <- X_train
X_train.0[is.na(X_train.0$환불금액),"환불금액"] <- 0
str(X_train.0)

X_train.0[,2:4] %>% filter(.<0)


X_train.0$총구매액 <- ifelse(X_train.0$총구매액<0, NA, X_train.0$총구매액)
X_train.0[is.na(X_train.0$총구매액),"총구매액"] <- 0

sum(is.na(X_train.0$총구매액))


train.0 <- left_join(x=X_train.0,y=Y_train,by=c("cust_id"))
str(train.0)
colSums(is.na(X_train.0))
colSums(is.na(Y_train))
colSums(is.na(train.0))
colSums(is.na(X_test))

train.0$gender <- as.factor(train.0$gender)
train.0$gender = ifelse(train.0$gender== 1, "Yes", "No")

#library(caret)
nearZeroVar(train.0)

str(train.0)
summary(train.0)

unique(train.0$주구매지점)
summary(train.0$주구매지점)



recipe(gender~., data=train.0) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_BoxCox(all_numeric()) %>%
  step_zv(all_numeric()) %>%
  step_dummy(all_nominal(), -gender, -index, one_hot=TRUE) %>%
  prep() %>%
  juice() -> train.1






#############################################################
# 분류일 떄
fitCtrl4Roc <- trainControl(classProbs=TRUE, summaryFunction=twoClassSummary)
fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=5)
#rf_fit <- train(gender~., data=train.0, method="rf",trControl=fitCtrl) # randomForest 넘 오래걸림 안됨.
warnings()

glm_fit <- train(gender~., data=train.0, method="glm", preProcess = c("center","scale"), tuneLength=10,trControl=fitCtrl4Roc, metric="ROC")
plot(glm_fit)

# lda_fit <- train(gender~., data=train.0, method="lda", preProcess = c("center","scale"), tuneLength=10,trControl=fitCtrl4Roc, metric="ROC")
# plot(lda_fit)

# gbm_fit <- train(gender~., data=train.0, method="gbm", preProcess = c("center","scale"), tuneLength=10, trControl=fitCtrl4Roc, metric="ROC")
# plot(gbm_fit)

rpart_fit <- train(gender~., data=train.0[2:11], method="rpart", preProcess = c("center","scale"), tuneLength=10, trControl=fitCtrl4Roc, metric="ROC", na.action=na.omit) # ROC 0.62
plot(rpart_fit)

# knn_fit <- train(gender~., data=train.0, method="knn", preProcess = c("center","scale"), tuneLength=10, trControl=fitCtrl4Roc, metric="ROC")
# plot(knn_fit)

svm_fit <- train(as.factor(gender)~., data=train.0[2:12], method="lssvmradial", preProcess = c("center","scale"), tuneLength=10, trControl=fitCtrl4Roc, metric="ROC")
# plot(svm_fit)

str(train.0)
# 회귀일 떄





getwd()
setwd("C:/Users/Administrator/Documents/R/BDA/210606")
# 1. 자동차사고 데이터를 SVM, rf 두가지로 모델링: "accidentsnn.csv" 파일사용 
car <- read.csv("accidentsnn.csv", stringsAsFactors = TRUE, encoding = "UTF-8")
head(car)
str(car)

# *****RandomForest
# 1.1 전처리
# y: MAX_SEV_IR 상해/치명 여부: 0 무상해,1 상해, 2 치명 (target) >> factor화
unique(car$MAX_SEV_IR)
car$MAX_SEV_IR<-factor(car$MAX_SEV_IR, labels=c("무상해","상해","치명"), levels=c(0,1,2))
str(car)

# 1.2 훈련/검증용 데이터 분할 
# train.d : test.d = 7 : 3 (y클래스 비율에 따라 층화추출)

car.parts <- createDataPartition(car$MAX_SEV_IR, p=.7, list=FALSE)
car.train <- car[car.parts,]
car.test <- car[-car.parts,]


# 1.3 랜덤포레스트 모델 구축

rf.car <- randomForest(MAX_SEV_IR~., data=car.train)

# 1.4 검증용 데이터로 예측하고, 혼동행렬로 결과 확인

actual.car <- as.factor(car.test[,5])
nrow(car.test)
pred.car<-predict(rf.car, newdata=car.test[,-5], type="class")
predict.car<-as.factor(pred.car)
nrow(predict.car)
confusionMatrix(data=predict.car, reference=actual.car)

# Prediction  Reference
#             무상해 상해 치명
# 무상해         153    0   13
# 상해             1   85   19
# 치명            11    4   12        
# Accuracy : 0.8389  

# 1.5 rownames = c("predict", "actual")인 표를 작성하여 rf_예측결과.csv파일로 제출

#car.t <- cbind(predict.car, actual.car)
#table(car.t)


# *****SVM
# 1.1 전처리
# ALCHL_I: 음주여부 1 있음 2 없음 -> 1/0(yes/no) 변경
# PROFIL_I_R: 도로정보 0 기타 1 level1 -> 0/1(etc/level1)
# SUR_COND: 도로의 노면 상태 1 건조 2 젖음 3 눈 4진흙 9 모름 ->1/2/3/4/9(dry/wet/snow/muddy/non)
# VEH_INVL: 관련된 차량의 수  -> 표준화
# MAX_SEV_IR 상해/치명 여부: 0 무상해,1 상해, 2 치명 (target)
# >> X: 범주형, 서열형 변수는 원핫인코딩을 통해 가변수화
# >> y: SVM에선 원핫인코딩없이 변수 하나로 처리할 것임 -> 0/1/2
car.2 <- car
str(car.2)
car.2$ALCHL_I <- factor(car.2$ALCHL_I, labels=c(1,0), levels=c(1,2))
car.2$PROFIL_I_R <- factor(car.2$PROFIL_I_R, labels=c(0,1), levels=c(0,1))
car.2$SUR_COND <- factor(car.2$SUR_COND, labels=c("건조","젖음","눈","진흙","모름"), levels=c(1,2,3,4,9))

#car.2$VEH_INVL <- preProcess(car.2$VEH_INVL, method=c("center","scale"))
car.2$VEH_INVL <- scale(car.2$VEH_INVL, center=TRUE, scale=TRUE)
dm.car.2 <-dummyVars(MAX_SEV_IR~., data=car.2)
dm.car.2.df <- as.data.frame(predict(dm.car.2, car.2))
str(dm.car.2.df)
car.3 <- cbind(dm.car.2.df, MAX_SEV_IR=car.2[,5])
str(car.3)
# 1.2 훈련/검증용 데이터 분할
# 검증데이터30%, 필요시 y클래스 비율에따라 층화추출

parts.car.3 <- createDataPartition(car.3$MAX_SEV_IR, p=.7, list=FALSE)
train.car.3 <- car.3[parts.car.3,]
test.car.3 <- car.3[-parts.car.3,]


# 1.3 SVM모델구축
# kernel = 'rbf' random_state=1, gamma 0.2, c=1.0 (python 용;;)
install.packages("e1071")
library(e1071)

#par(mfrow=c(1,1))
#plot(MAX_SEV_IR~., data=train.car.3)
str(train.car.3)
tune.svm(MAX_SEV_IR~., data=train.car.3, gamma=10^(-5:-1), cost=10^(1:3))
# gamma=0.1, cost=10
sv.car.3 <- svm(MAX_SEV_IR~., data=train.car.3, type="C-classification", kernel="radial", gamma=0.1, cost=10)
summary(sv.car.3)

# 1.4 검증용 데이터로 예측 
# y: class label 3개 인것 고려, 결과는 class로 표시하여 confusion matrix 작성
str(test.car.3)
pred.test.3 <- predict(sv.car.3, newdata=test.car.3[,-11])
library(caret)
confusionMatrix(pred.test.3, reference=test.car.3[,11])

# Prediction  Reference
#             무상해 상해 치명
# 무상해         165    0   16
# 상해             0   84   17
# 치명             0    5   11
# Accuracy : 0.8725       

library(ROCR)
#pred.svm.roc <- prediction(as.numeric(pred.test.3), as.numeric(test.car.3[,11])) # 클래스 3개라서 안됨...

car.3.t <- table(as.numeric(pred.test.3), as.numeric(test.car.3[,11]))
sum(car.3.t)
car.3.정분류율 <- sum ( car.3.t[row(car.3.t)==col(car.3.t)] ) / sum(car.3.t)

# 1.5 rownames = c("predict", "actual")인 표를 작성하여 svm_예측결과.csv파일로 제출



# 2. 유니버셜은행 데이터를 여러 모델을(로지스틱회귀, 의사결정나무, KNN, Major voting)을 사용하여 분류하고, 모델성능을 비교 검증하는 코드를 작성 (큰 문제 2번은 확정, 밑에 소분류는 수정 중..)

bank <- read.csv("UniversalBank.csv", stringsAsFactors = TRUE, encoding = "UTF-8")
head(bank)
str(bank)
# 2.1 "UniversalBank.csv" 파일사용   
# 2.2 X- drop:ID, ZIPCode,PersonalLoan, y- PersonalLoan
# 2.3 전처리

unique(bank$PersonalLoan)
bank$PersonalLoan <- as.factor(bank$PersonalLoan)

unique(bank$Education)
bank$Education <- factor(bank$Education, levels=c(1,2,3), labels=c("Under","Grad","Adv"))
str(bank$Education)

# Education변수-OneHotEncoding
# install.packages("caret")
# library(caret)
# help(dummyVars)
str(bank)
class(bank$Education)
dum.edu <- dummyVars(~Education, data=bank)
bank.2 <-  cbind(bank[,-8], predict(dum.edu, newdata=bank) )
str(bank.2)
head(bank.2)

# 2.4 훈련/검증용 데이터 분할
# 검증데이터30%, 필요시 y클래스 비율에 따라 층화추출

parts <- createDataPartition(bank.2$PersonalLoan, p=0.7, list=FALSE)
nrow(parts)

train.b <- bank.2[parts,]
test.b <- bank.2[-parts,]

prop.table(table(train.b$PersonalLoan))
prop.table(table(test.b$PersonalLoan))

# 2.5 여러분류모델 구축: LR, tree, KNN, major voting
# major voting - "SOFT"

names(getModelInfo())
modelLookup("glm")
modelLookup("knn")
modelLookup("rpart")


install.packages("rpart")
library(rpart)
#rpart.bank <- rpart(PersonalLoan~., data=train.b )
library(caret)

??trainControl()
tc.bank <- trainControl(method="cv", number=10)
#, repeats=5)
#, summaryFunction=twoClassSummary, classProbs=TRUE)
# glm.bank <- train(PersonalLoan~., data=train.b, method="glm", trControl=tc.bank)
knn.bank <- train(PersonalLoan~., data=train.b, method="knn", trControl=tc.bank)
rpart.bank <- train(PersonalLoan~., data=train.b, method="rpart", trControl=tc.bank)
#, tuneLegth=15, metric="ROC")



install.packages("randomForest")
library(randomForest)
rf.bank <- randomForest(PersonalLoan~., data=train.b )
rf.bank$importance

actual.test.b<-as.factor(test.b[,9])
nrow(test.b)
pred.rf.bank <- predict(rf.bank, newdata=test.b[,-9], type="class")
predict.rf.bank <- as.factor(pred.rf.bank)
length(pred.rf.bank)
confusionMatrix(data=predict.rf.bank, reference=actual.test.b)

bank.t <- cbind(predict.rf.bank, as.data.frame(actual.test.b))
table(bank.t)

# 2.6 모델검정
# AUC검정, ROC곡선그리기 - 모든 모델의 값들을 다 표기하여, 성능개선이 얼마나 이루어 졌는지 표현

library(ROCR)
pred.rf.bank.roc <- prediction(as.numeric(predict.rf.bank), as.numeric(actual.test.b))
plot(performance(pred.rf.bank.roc,"tpr","fpr"))
abline(a=0,b=1,lty=2,col="black")



# 2.7 최적화
# 하이퍼파라미터 튜닝(이걸 마지막에 하는게 맞나??)
# 그리드 서치를 사용한 ML 모델세부튜닝(각모델의 파라미터는 외우셨는지..)






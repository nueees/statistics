#### 0605_모델링 주제 ####
# 하나의 데이터를 RandomForest, SVM으로 예측 및 분류하기
# 여러 분류모델을 다시 앙상블(ex.하드보팅)하여 분류하기
#### 0605_모델링 문제 ####
# 1. 하나의 데이터를 RandomForest, SVM으로 예측 및 분류하기
#### 랜덤포레스트 ####
library(randomForest); library(caret)
accident <- read.csv("accidentsnn.csv", header = TRUE)
sum(!complete.cases(accident)) # 결측치 없음
head(accident)
colnames(accident)
# X: "ALCHL_I"  "PROFIL_I_R" "SUR_COND" "VEH_INVL"
# y: "MAX_SEV_IR"

# 전처리
# X: "ALCHL_I"  "PROFIL_I_R" "SUR_COND" "VEH_INVL"
# "ALCHL_I" 2/1 -> 1/0(labels=yes/no) 변경
accident$ALCHL_I <- ifelse(accident$ALCHL_I==2, 1, 0)
accident$ALCHL_I <- factor(accident$ALCHL_I, levels = c(1,0), labels = c("yes", "no"))
# 나머지 범주형X전부 팩터화
accident$PROFIL_I_R <- factor(accident$PROFIL_I_R, levels = c(0,1), labels = c("etc", "level1")) 
accident$SUR_COND <- factor(accident$SUR_COND, levels = c(1,2,3,4,9), labels = c("dry", "wet","snow","muddy","non"))
unique(accident$SUR_COND)
table(as.factor(accident$SUR_COND))
accident <- accident[complete.cases(accident), ]
# 수치형X 표준화
accident$VEH_INVL <- as.vector(scale(accident$VEH_INVL, center = TRUE, scale = TRUE))

head(accident, 10)

# y: "MAX_SEV_IR" 팩터화
accident$MAX_SEV_IR <- factor(accident$MAX_SEV_IR, levels = c(0,1,2), labels = c("무상해","상해","치명"))
head(accident, 10)

# 데이터 분할 train:test = 3:7, library(caret)
table(accident$MAX_SEV_IR) # 불균형 데이터 확인
# 무상해   상해   치명 
#   551    299    149  >>> stratified sampling!
set.seed(137) # 시드고정
parts <- createDataPartition(accident$MAX_SEV_IR, p=0.7)
table(accident[parts$Resample1, "MAX_SEV_IR"])      
# 551*0.7  
# 274*0.7
# 146*0.7  #대충맞네..
train.d <- accident[parts$Resample1,] # 701 obs.
test.d <- accident[-parts$Resample1,] # 298 obs.

sum(!complete.cases(train.d))
sum(!complete.cases(test.d))

# 랜덤포레스트 모델구축
(m <- randomForest(MAX_SEV_IR ~., data = train.d))
# (m <- randomForest(train.d[,1:4], train.d[,5]))

# Call:
#   randomForest(x = train.d[, 1:4], y = train.d[, 5]) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 14.68%
# Confusion matrix:
#        무상해 상해 치명 class.error
# 무상해    386    0    0  0.00000000
# 상해        0  181   11  0.05729167
# 치명       38   51   14  0.86407767

# 검증용 데이터의 y값 예측 후 결과평가
results <- predict(m, newdata = test.d)
caret::confusionMatrix(results, test.d[,5])
#           Reference
# Prediction 무상해 상해 치명
#    무상해    164    0   20
#    상해        0   81   13
#    치명        1    8   11
# 예측이 무상해인데 실제가 치명이면 진짜 치명적인데... 이거 어케 해결해야됨??(라쏘 릿지?)

#### SVM ####
library(e1071)
rm(list=ls()); ls()
accident <- read.csv("accidentsnn.csv", header = TRUE)

# 전처리
# X: "ALCHL_I"  "PROFIL_I_R" "SUR_COND" "VEH_INVL"
# "ALCHL_I" 2/1 -> 1/0(labels=yes/no) 변경
accident$ALCHL_I <- ifelse(accident$ALCHL_I==2, 1, 0)
accident$ALCHL_I <- factor(accident$ALCHL_I, levels = c(1,0), labels = c("yes", "no"))
# 나머지 범주형X전부 팩터화
accident$PROFIL_I_R <- factor(accident$PROFIL_I_R, levels = c(0,1), labels = c("etc", "level1")) 
accident$SUR_COND <- factor(accident$SUR_COND, levels = c(1,2,3,4,9), labels = c("dry", "wet","snow","muddy","non"))
unique(accident$SUR_COND)
table(as.factor(accident$SUR_COND))
accident <- accident[complete.cases(accident), ]
# 수치형X 표준화
accident$VEH_INVL <- as.vector(scale(accident$VEH_INVL, center = TRUE, scale = TRUE))

head(accident, 10)

# y: "MAX_SEV_IR" 팩터화
accident$MAX_SEV_IR <- factor(accident$MAX_SEV_IR, levels = c(0,1,2), labels = c("무상해","상해","치명"))
head(accident, 10)

# 데이터 분할 train:test = 3:7, library(caret)
table(accident$MAX_SEV_IR) # 불균형 데이터 확인
# 무상해   상해   치명 
#   551    299    149  >>> stratified sampling!
set.seed(137) # 시드고정
parts <- createDataPartition(accident$MAX_SEV_IR, p=0.7)
table(accident[parts$Resample1, "MAX_SEV_IR"])      
# 551*0.7  
# 274*0.7
# 146*0.7  #대충맞네..
train.d <- accident[parts$Resample1,] # 701 obs.
test.d <- accident[-parts$Resample1,] # 298 obs.

sum(!complete.cases(train.d))
sum(!complete.cases(test.d))

# SVM 모델구축
(m <- svm(MAX_SEV_IR ~., data = train.d))
# Call:
#   svm(formula = MAX_SEV_IR ~ ., data = train.d)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# 
# Number of Support Vectors:  254

# 그리드 탐색을 사용한 파라미터 튜닝 
# tune(svm, MAX_SEV_IR ~., data = train.d, gamma = 2^(-1:1), cost=2^(2:4))

# 검증용 데이터의 y값 예측 후 결과평가
results <- predict(m, newdata = test.d)
caret::confusionMatrix(results, test.d[,5])
# Confusion Matrix and Statistics
# 
#            Reference
# Prediction 무상해 상해 치명
#   무상해    161    0   16
#   상해        1   78   14
#   치명        3   11   14
####




#### 2. 유니버셜 은행 데이터: LR, decision tree, knn, Major voting(voting = Soft) 으로 분류하여, 단일모델 대비 앙상블에서 모델성능 향상이 있는지 확인 ####
# 목표: 유니버셜 은행고객의 인구통계학적 특성을 이용하여, 은행고객의 개인신용대출 수락여부를 예측하고자 함 

rm(list=ls())
library(caret)

# 데이터 확인
universal_bank <- read.csv("UniversalBank.csv", header = TRUE)
str(universal_bank) # 5000obs. 14 vars
#### 변수정보 ####
# ID: Customer ID
# Age: Customer's Age in completed year
# Experience: #years of professional experience
# Income: Annual income of the customer($000)
# ZIPCode: Home Address ZIP code
# Family: Family size (dependents) of the customer
# CCAvg: Avg. Spending on Credit Cards per month ($000)
# Education: Education Level. 1:Undergrad, 2:Graduate, 3: Advanced/Professional
# Mortgage: Value of house mortgage if any.($000)
# Personal Loan: Did this customer accept the personal loan offered in the last compaign?
# Securities Account: Does the customer have a Securities account with the bank?
# CD Account: Does the custmer have a Certificate of Deposit(CD) account with the bank?
# Online: Does the customer use internet banking facilities?
# CreditCard: Does the customer use a credit card issued by UniversalBank?
###########################################################
# 2.1 "UniversalBank.csv" 파일사용   
# 2.2 y- PersonalLoan, X- drop:ID, ZIPCode,PersonalLoan
# 2.3 전처리, # Education(factor var)-> OneHotEncoding
# 2.4 훈련/검증용 데이터 분할: 검증데이터30%, 필요시 y클래스 비율에 따라 층화추출
str(universal_bank) # 5000obs. 14 vars
sum(!complete.cases(universal_bank)) # 0, 결측치 없음
# 필요시 na.omit(universal_bank)
summary(universal_bank)
# X: drop- ID, ZIPCode, # Education(factor var) -> OneHotEncoding
# y: PersonalLoan(factor 0/1 = no/yes)
bank <- universal_bank[,c(2,3,4,6:14)] 
bank$Education <- factor(bank$Education, levels = c(1,2,3),
                         labels = c("Undergrad","Graduate","Advanced/Professional"))
bank$PersonalLoan <- factor(bank$PersonalLoan,
                            levels = c(0,1), labels = c("No","Yes"))

# 데이터분할
set.seed(9999)
train_idx <- createDataPartition(bank$PersonalLoan, p = 0.7, list = FALSE)
train.d <- bank[train_idx,]
test.d <- bank[-train_idx,]
#NROW(train.d); NROW(test.d) #train:test = 7:3

names(getModelInfo()) #"knn" "glm" "rpart"
modelLookup("knn")

# 2.5 여러분류모델 구축: LR, tree, KNN, major voting("soft")
#### 결정나무: library(caret), rpart(No preProcess)
ctrl <- trainControl(method = "cv", number = 10) #summaryFunction = multiCalssSummary
fit.cv.tr <- train(PersonalLoan ~., data= train.d, method= "rpart",
                   trControl = ctrl, 
                   #preProcess = c("center","scale"),
                   #tuneGrid = data.frame(cp=0.05))
                   tuneLength = 30) #metric = "Kappa"
pred.tr <- predict(fit.cv.tr, test.d)
confusionMatrix(test.d$PersonalLoan, data = pred.tr,
                mode = "everything", positive = "Yes")

#비교를 위한 담기
tr.cfm <- table(test.d$PersonalLoan, pred.tr)
Perf.Table[1,] <- perf_eval(tr.cfm)
Perf.Table

#### 로지스틱회귀: library(caret), method = "glm"
fit.cv.log <- train(PersonalLoan ~., data= train.d, method= "glm",
                    trControl = ctrl, 
                    tuneLength = 15)
pred.log <- predict(fit.cv.log, test.d)
confusionMatrix(test.d$PersonalLoan, data = pred.log,
                mode = "everything", positive = "Yes")

#비교를 위한 담기
log.cfm <- table(test.d$PersonalLoan, pred.log)
Perf.Table[2,] <- perf_eval(log.cfm)
Perf.Table

#### KNN : library(caret), method = "knn"
fit.cv.knn <- train(PersonalLoan ~., data= train.d, method= "knn",
                    trControl = ctrl,
                    preProcess = c("center","scale"),
                    tuneLength = 15)
pred.knn <- predict(fit.cv.knn, test.d)
confusionMatrix(test.d$PersonalLoan, data = pred.knn,
                mode = "everything", positive = "Yes")

#비교를 위한 담기
knn.cfm <- table(test.d$PersonalLoan, pred.knn)
Perf.Table[3,] <- perf_eval(knn.cfm)
Perf.Table

# 2.6 모델검정
# AUC검정, ROC곡선그리기 - 모든 모델의 값들을 다 표기하여, 성능개선이 얼마나 이루어 졌는지 표현
perf_eval <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR 단순정확도
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate 균형정확도
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

Perf.Table <- matrix(0, nrow = 3, ncol = 6)
rownames(Perf.Table) <- c("Decision Tree", "Logistic R", "KNN")
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")


library(pROC)




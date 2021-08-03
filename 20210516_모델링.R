setwd("C:/Users/Administrator/Documents/R/BDA/210516")
# train(Y값 존재), test(Y값 미존재) 데이터를 받아 모델링하기
test <- read.csv("test_set.csv", stringsAsFactors=TRUE)
train <- read.csv("train_set.csv", stringsAsFactors=TRUE)
testY <- read.csv("test_Y.csv", stringsAsFactors=TRUE)

# train, test 를 하나의 데이터로 묶어서 전처리하기 
str(test)
str(testY)
str(train)

test2 <- cbind(test,testY)

all <- rbind(test2,train)
str(all)
unique(all$income)

colSums(is.na(all))

# 분류 앙상블(의사결정나무, 배깅, 부스팅, 랜덤포레스트) AUROC 기준으로 비교하고 하이퍼라라메터 튜닝하기 
install.packages("rpart")
library(rpart)

# 1. train_set 데이터를 이용하여 'income' 상태를 분류하는 의사결정나무를 만든 뒤, 모델의 비용복잡도에 기준해 가지치기(Pruning) 과정을 수행한 최종 모델을 만드시오.
str(train)

incm.1.m <- rpart(income~., data=train)
incm.1.m
library(rpart.plot)
#prp(incm.1.m)
#prp(incm.1.m, cex=1)
#prp(incm.1.m, type=4) #type 0~5
prp(incm.1.m, extra=2) #extra 0~11

incm.1.m$cptable
incm.1.m$variable.importance
incm.1.m$control
# minsplit - 분기를 시도하기 위해서 각 노드에 포함되어야 하는 관측치의 최소 갯수
# cp - 복잡도 파라미터(complexity parameter). 최소 cp 의 값 이상으로 전체의 lack of fit 을 감소시키지 않는다면 그 분기는 시도되지 않는다.
# minbucket - terminal node 에서 가져야하는 관측치의 최소 갯수. 낮으면 복잡도 증가
# maxdepth - 최종 나무에서 노드들이 가질 수 있는 최대 깊이

incm.1_2.m <- rpart(income~., data=train, control=rpart.control(minbucket=2))
prp(incm.1_2.m, extra=2) 


# pruning 안해도 최적임 4개분리

#idx.min<-which.min(incm.1.m$cptable[,"xerror"])
#cp<-incm.1.m$cptable[idx.min,"CP"]
#incm.1.m.prune <- prune(incm.1.m, cp)
#incm.1.m.prune
#rpart.plot(incm.1.m.prune)
#plotcp(incm.1.m.prune) # tree size 5

# 2. 랜덤포레스트를 만들고 변수 중요도(feature importance)를 bar graph로 시각화하시오. 포레스트를 만들 때 다음 조건을 참고하시오. 단, AUC 그래프/AUROC를 참고하여 random forest의 트리 개수, 각 트리의 깊이, 노드의 split을 결정하는 최소 크기, 하위 leaf가 만들어지는 최소한의 샘플 데이터 수(min_sample_split)를 튜닝하는 과정을 포함할 것.


install.packages("randomForest")
library(randomForest)
str(train)

sqrt(15) # mtry 3.87개 (3,4)

incm.2.m <- randomForest(income~.,data=train, ntree=10, mtry=sqrt(15)) 
#mtry : 각 노드를 자식 노드로 나누는 기준을 정할 때 고려할 변수의 개수(minsplit) 분류에선 sqrt(변수개수), 회귀에선 (변수개수/3)

#incm.2.m$confusion
incm.2.m$importance
#incm.2.m$err.rate
varImpPlot(incm.2.m)

# predict 매칭 에러나서 데이터 보정
#test3 <- rbind(train[1, ] , test2)
#test3 <- test3[-1,]

# 테스트데이터로 예측
#incm.2.pred <- predict(incm.2.m, test3[,-15])
#incm.2.t <- table(incm.2.pred, test3[,15])
# 정확도
#sum(incm.2.t[1,1],incm.2.t[2,2])/sum(incm.2.t)

# 변수 튜닝

incm.2.pgrid <- expand.grid(ntree=c(10,20), mtry=c(3,4))

incm.2.pgrid[1,"ntree"]
incm.2.pgrid[1,"mtry"]

tun.fun.2<- foreach(g=1:nrow(incm.2.pgrid), .combine=rbind) %do% {
  tun.m.2 <- randomForest(income~.,data=train, ntree=incm.2.pgrid[g,"ntree"], mtry=incm.2.pgrid[g,"mtry"])
  tun.pred.2 <- predict(tun.m.2, test3[,-15])
  tun.t.2 <- table(tun.pred.2, test3[,15])
  tun.prec.2 <- sum(tun.t.2[1,1],tun.t.2[2,2])/sum(tun.t.2)
  return(data.frame(g=g, precision=tun.prec.2))
}

incm.2.pgrid[2,] # 가장 성능 좋게함


##########################배깅,부스팅 테스트#############################

install.packages("ipred")
library(ipred)

incm.bag <- ipred::bagging(income~.,data=train, nbagg=15, coob=TRUE) #nbagg bootstrap수
incm.bag$err #오분류율

install.packages("adabag")
library(adabag)


incm.boo <- boosting(income~.,data=train, boos=TRUE, mfinal=15) #boos 가중치사용
incm.boo$weights #각 나무별 가중치
set.seed(1234)
incm.Idx<- sample(1:nrow(train),15)
incm.boo$votes[incm.Idx,] #랜덤으로 선택한 15개 투표결과

incm.boo$importance
which.max(incm.boo$importance)

incm.boo.pred <- predict(incm.boo, newdata=test, type="class")
incm.boo.pred <- as.factor(incm.boo.pred$class)
confusionMatrix(data=incm.boo.pred, reference=testY$income)
library(ROCR)

help(prediction)
incm.boo.roc <- prediction(as.numeric(incm.boo.pred), as.numeric(testY$income))
help(performance)
incm.boo.perf <- performance(incm.boo.roc,"tpr","fpr")
plot(incm.boo.perf)
incm.boo.perf@y.values

incm.boo.pred.t <- table(incm.boo.pred$class, testY[,1]) 

sum(incm.boo.pred.t[1,1]+incm.boo.pred.t[2,2])/sum(incm.boo.pred.t)

############################테스트 끝############################







# 3. 가장 성능이 좋은 모델을 이용하여 test_set 데이터의 income 을 분류하고 csv 파일로 제출할 것 (제출명 : 이름_0516모델링.csv)
# validation set의 크기는 자유롭게 결정

incm.2.t.m <- randomForest(income~.,data=train, ntree=incm.2.pgrid[2,"ntree"], mtry=incm.2.pgrid[2,"mtry"]) 

incm.2.t.pred <- predict(incm.2.t.m, test3[,-15])

# 정확도
#incm.2.t.t <- table(incm.2.t.pred, test3[,15])
#sum(incm.2.t.t[1,1],incm.2.t.t[2,2])/sum(incm.2.t.t)

test.3 <- cbind(test, incm.2.t.pred)
test.3 <- cbind(test.3, test3[,15])

write.csv(test.3, file = "조시은_0516모델링.csv")
head(test.3)



# 최종 모델을 pipeline 형태로 구현하여, test set 분류 시 별도의 전처리 과정 없이 함수 하나로 이루어질 수 있도록 코딩



# 코드는 pdf파일, 혹은 html 파일로 업로드 (제출명 : 이름_0516모델링코드)




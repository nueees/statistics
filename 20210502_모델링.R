# train.csv, test.csv 를 로딩하여 의사결정나무를 이용하여 학습하고 최고 성능을 만들고 평가하시오. 최고 성능의 예측모델로 test.csv 파일의 class 를 예측하고 이를 파일로 제출하시오 
# 제출파일은 본인이름(예시 홍길동)을 이용하여 본인 폴더에 제출
# 홍길동_test.csv, 홍길동_모델링.R, 홍길동_모델링.pdf (또는 DOC) 

# "* 학습용(train), 검증용(test) CSV 파일이 주어지고 의사결정나무 알고리즘을 이용하여 분류하고 이에 대한 성능을 평가
# (train 에는 목적변수가 있으나, 검증용에는 목적변수 없음) 
# * test 의 목적변수를 예측하여 csv 파일로 제출하기 "

getwd()

#1) 전처리
train.df <- read.csv("train.csv", header=TRUE, stringsAsFactors = TRUE,)
str(train.df)
head(train.df)
train.df$class <- as.factor(train.df$class)
test.df <- read.csv("test.csv", header=TRUE, stringsAsFactors = TRUE,)
str(test.df)
head(test.df)

#2)  rpart, tree, ctree 모형생성

install.packages("rpart")
library(rpart)
library(rpart.plot)

m1 <- rpart(class~., data=train.df)
plot(m1)
text(m1, cex=1.5)
prp(m1, type=2 )

install.packages("tree")
library(tree)
m2 <- tree(class~., data=train.df)
plot(m2)
text(m2, cex=1.5)

cv.m2<-cv.tree(m2)
plot(cv.m2)
prune.m2<-prune.misclass(cv.m2, best=2)


install.packages("party")
library(party)
m3 <- ctree(class~., data=train.df)
m3
plot(m3)


#3) 예측

levels(train.df$class)

rpart.pred <- predict(m1, newdata = test.df, type="class")
cbind(test.df, rpart.pred)

tree.pred <- predict(m2, newdata = test.df, type="class")
cbind(test.df, tree.pred)

ctree.pred <- predict(m3, newdata = test.df, type="response")
cbind(test.df, ctree.pred)

library(caret)
#confusionMatrix(rpart.pred, c(1,1,1,0,0,0,0,0,0,0))


#4) randomForest

library(randomForest)

m3 <- randomForest(class ~ ., data = train.df, importance=TRUE)
m3
varImpPlot(m3)
rdfrst.pred <- predict(m3, newdata = test.df, type="class")

importance(m3)

test.df<-cbind(test.df, rdfrst.pred)

# 검증
t <- table( actual, rdfrst.pred)  > confusionMatrix(t)




###############################################################################


#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')

#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)

getwd()
mushrooms <- read.csv ("C:/Users/Administrator/Documents/R/BDA/210502/mushrooms.csv")
str(mushrooms)
head(mushrooms)

mushrooms$type <- as.factor(mushrooms$type)
# help(colnames)
# m0 <- matrix(NA, 4, 0)
# rownames(m0)
# 
# m3 <- cbind(1, 1:4)
# colnames(m2, do.NULL = FALSE)
# colnames(m2, do.NULL = TRUE)
# colnames(m2) <- c("x","Y")
# rownames(m2) <- rownames(m2, do.NULL = FALSE, prefix = "Obs.")
# m2
# colnames(m3) <- colnames(m3, do.NULL = FALSE, prefix = "Col.")
# m3

library(dplyr)
mushrooms <- rename(mushrooms, "class"="type")
names(mushrooms)
sum(complete.cases(mushrooms)==FALSE)

# no significant eliminate
mushrooms$veil_type <- NULL

# EDA
table(mushrooms$class,mushrooms$odor)
table(mushrooms$class,mushrooms$cap_color)
t1<-table(mushrooms$class,mushrooms$spore_print_color)
t1==0
mushrooms.bak <- mushrooms
number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class,col)
  sum(t == 0)
})

# 0 많은 애 순서대로 보자
mush_order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[mush_order]

help(par)
par(mar=c(12,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")

# 학습/테스트 데이터 나눔

set.seed(12345)
mush_idx <- sample(1:nrow(mushrooms), size = mushrooms*0.8, replace = FALSE)
nrow(mushrooms)
length(mush_idx)
mush_train <- mushrooms[mush_idx,]
nrow(mush_train)
mush_test <- mushrooms[-mush_idx,]
nrow(mush_test)


# 의사결정나무 모델 생성
install.packages("rpart")
library(rpart)
library(rpart.plot)
str(mush_train)
help(rpart)

mush.m1 <- rpart(class~., data=mush_train)

rpart.plot(mush.m1)
prp(mush.m1, type=1)

library(randomForest)

mush.m3 <- randomForest(class ~ ., data = mush_train, importance=TRUE)
mush.m3
varImpPlot(mush.m3)
mush.m3.pred <- predict(mush.m3, newdata = mush_test[-1], type="class")
importance(mush.m3)

# 검증
install.packages("caret")
library(caret)

t<-table(mush_test$class, mush.m3.pred)
confusionMatrix(t)





###############################################################################


getwd()
winequalityWhite <- read.csv ("C:/Users/Administrator/Documents/R/BDA/210502/winequality-white.csv")

str(winequalityWhite)
head(winequalityWhite)
install.packages("dplyr")
library(dplyr)
distinct(winequalityWhite,quality)
table(winequalityWhite$quality)
help(transform)
winequalityWhite1<-transform( winequalityWhite,
  quality={ifelse(winequalityWhite$quality>=7,"EXCELLENT"
          , ifelse(winequalityWhite$quality>=6 & winequalityWhite$quality<7,"GOOD"
          , "BAD") )}) 
winequalityWhite1$quality <- as.factor( winequalityWhite1$quality)
str(winequalityWhite1)


heart<-read.csv('C:/Users/Administrator/Downloads//Heart.csv', stringsAsFactors=TRUE)
str(heart)
head(heart)
library(caret)

set.seed(1234)
heart.idx <- createDataPartition(y=heart$AHD, p=0.7, list=TRUE)
heart.idx <- createDataPartition(y=heart$AHD, p=0.7, list=FALSE)
heart.train <- heart[heart.idx,]
nrow(heart.train)
heart.test <- heart[-heart.idx,]
nrow(heart.test)

install.packages("tree")
library(tree)
help(tree)
heart.m1 <- tree(AHD~., data=heart.train)
plot(heart.m1) 
text(heart.m1)

# cross validation
help(cv.tree)
cv.heart.m1<-cv.tree(heart.m1)
plot(cv.heart.m1)

# prune
help(prune.misclass)
prune.heart.m1<-prune.misclass(heart.m1, best=4)
plot(prune.heart.m1)
text(prune.heart.m1, pretty=0)

# prediction
prune.heart.m1.pred <- predict(prune.heart.m1, newdata = heart.test, type="class" )
prune.heart.m1.pred
library(caret)
confusionMatrix(prune.heart.m1.pred, heart.test$AHD)


install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

heart.m2 <- rpart(AHD~., data=heart.train)
rpart.plot(heart.m2)

# prune
help(printcp)
printcp(heart.m2)
heart.m2$cptable
plotcp(heart.m2)

class(heart.m2$cptable)

heart.m2$cptable[,"xerror"]
which.min(heart.m2$cptable[,"xerror"])
heart.m2$cptable[which.min(heart.m2$cptable[,"xerror"]),"CP"]

prune.heart.m2<-prune(heart.m2, cp= heart.m2$cptable[which.min(heart.m2$cptable[,"xerror"]),"CP"])
plot(prune.heart.m2)
text(prune.heart.m2)

# prediction
heart.m2.pred <- predict(heart.m2, newdata = heart.test, type="class" )
heart.m2.pred
library(caret)
confusionMatrix(heart.m2.pred, heart.test$AHD)

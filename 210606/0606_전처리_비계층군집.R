# 2. 비계층적 군집 분석 - diamonds 데이터의 표본을 추출하여 k-means 군집 분석을 수행한다.
# 2-1. ggplot2에 내장된 diamonds 데이터를 불러오고 데이터 규모, 구조 등을 파악한다.
rm(list = ls())
library(ggplot2)
data("diamonds")
dim(diamonds)
head(diamonds)

# 2-2. diamonds 데이터를 대상으로 1000개 표본 추출하고 price, carat, depth, table 변수만 선택하여 "mydia" 라는 데이터를 생성한다.
idx <- sample(1:nrow(diamonds), 1000)
train <- diamonds[idx,]
dim(train)
head(train)

mydia <- train[c("price","carat","depth","table")]
head(mydia)

#### 계층적 군집분석 시도
# result <- hclust(dist(mydia), method = "average")
# result
# plot(result, hang = -1)
# rect.hclust(result, k = 3)


# 2-3. 위에서 생성한 mydia 데이터를 가지고 군집 수를 3개로 하여 K-means 군집분석을 수행한다. 군집분석 결과로 얻은 cluster 컬럼은 mydia의 파생 변수로 추가한다.
# (빅분기 지정패키지에는 없지만 NbClust를 활용하여 최적 군집 수를 직접 찾아본다.)
result <- kmeans(mydia, 3)
names(result)
table(result$cluster)

head(mydia)
mydia$cluster <-  result$cluster
head(mydia)

install.packages("NbClust")
library(NbClust)
nc <- NbClust(mydia[ ,-5], min.nc = 2, max.nc = 10, method = "kmeans")

# 2-4. 피어슨 상관분석을 통해 가장 상관관계 높은 두 변수를 찾아, cluster 별로 다른 색으로 표현하여 plot 시각화한다.
cor(mydia[ ,-5], method = "pearson")  #상관관계 높은 두 변수 : carat, price
plot(mydia)
plot(mydia[ ,-5], col = result$cluster)  

# install.packages("mclust")
# install.packages("corrgram")
# library(mclust)
# library(corrgram)
# corrgram(mydia[ , -5], upper.panel = panel.conf)

plot(mydia$carat, mydia$price, col = mydia$cluster)
# 중심점 추가 (col : 점 색상, pch: 중심점 모양, cex: 중심점 크기)
points(result$centers[ ,c("carat", "price")],
     col = c(3,1,2), pch = 8, cex = 2)


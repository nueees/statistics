# 1. 계층적 군집분석 - 신입사원의 면접시험 결과를 군집분석한다.
getwd()
setwd("C:/Users/Administrator/Documents/R/BDA/210606")
# 1-1. interview.csv 파일을 불러오고 데이터 구조를 파악한다.
interview <- read.csv("interview.csv", stringsAsFactors = TRUE, encoding = "EUC-KR")
interview
str(interview)

# 1-2. "no", "종합점수", "합격여부" 컬럼을 제외하고 면접시험 평가요소인 6개 컬럼만으로 interview_df로 다시 저장하고, 유클리디안 거리를 계산한다. (dist 함수 사용)
library(dplyr)
interv.df <- interview %>% select(-c("no","종합점수","합격여부"))
interv.dist <- dist(interv.df, method="euclidean")

# 1-3. 계층적 군집 분석을 수행하고 plot으로 군집 분석 결과를 확인한다.

interv.single <- hclust(interv.dist, method="single")
plot(interv.single)
interv.complete <- hclust(interv.dist, method="complete")
plot(interv.complete)
interv.average <- hclust(interv.dist, method="average")
plot(interv.average)

# 1-4. 군집의 개수를 3개로 하여 subset을 생성하고 각 subset의 합격여부를 파악한다.

interv.single.3 <- cutree(interv.single, k=3)
plot(interv.single)
rect.hclust(interv.single,k=3,border="red")

interv.4<- interv.df %>% mutate(group = interv.single.3)
str(interv.4)
interv.4$group <- as.factor(interv.4$group)

interv.5 <- interv.4 %>% mutate(합격여부=interview$합격여부)

interv.5 %>% group_by(group) %>% summarise(합격여부)
                                             


# 1-5. 요약통계량(summary)를 통해 각 subset의 종합점수 평균 등의 군집별 특징을 확인한다.
aggregate(.~group, interv.5,  summary)


# 2. 비계층적 군집 분석 - diamonds 데이터의 표본을 추출하여 k-means 군집 분석을 수행한다.
dia <- diamonds
str(dia)
# 2-1. ggplot2에 내장된 diamonds 데이터를 불러오고 데이터 규모, 구조 등을 파악한다.

# 2-2. diamonds 데이터를 대상으로 1000개 표본 추출하고, price, carat, depth, table 변수만 선택하여 "mydia" 라는 데이터를 생성한다.

mydia <- dia[,c("price", "carat", "depth", "table")]
str(mydia)

mydia.parts <- sample(1000, replace=FALSE)

mydia.2 <- mydia[mydia.parts,]
nrow(mydia.2)
str(mydia.2)

# 2-3. 위에서 생성한 mydia 데이터를 가지고 군집 수를 3개로 하여 K-means 군집분석을 수행한다. (빅분기 지정 패키지에는 없지만 NbClust를 활용하여 최적 군집 수를 스스로 찾아본다.) 군집분석 결과로 얻은 cluster 컬럼은 mydia의 파생 변수로 추가한다.

kmeans.dia <- kmeans(mydia.2, centers=3)
mydia.3 <- cbind(mydia.2, kmeans=kmeans.dia$cluster)
#NbClust::NbClust(kmeans.dia, method=)

# 2-4. 피어슨 상관분석을 수행하여 가장 상관관계 높은 두 변수를 찾아, cluster 별로 다른 색으로 표현하여 plot 시각화한다.
cor(mydia.2, method="pearson") #carat & price
library(dplyr)
mydia.4 <- mydia.3 %>% select(c("carat","price","kmeans"))
mydia.4$kmeans <- as.factor(mydia.4$kmeans)
str(mydia.4)
plot(mydia.4[,-3], col=mydia.4$kmeans)




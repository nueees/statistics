#  latitude, Score, Social.support, Healthy.life.expectancy, Freedom.to.make.life.choices, Generosity, Perceptions.of.corruption

# 1. WHR2019 데이터와 WB 데이터를 불러와서 WHR2019 데이터의 'Country or region' 컬럼에 WB 데이터의 'name' 컬럼 기준으로 left join 한 새로운 데이터프레임을 만드시오. (새로운 데이터 프레임의 각 데이터 column name은 기존 두 데이터프레임과 동일하게 할 것)


setwd("C:/Users/Administrator/Documents/R/BDA/210509")

wb <- read.csv("WB.csv", stringsAsFactors = TRUE)
wb2019 <- read.csv("WHR2019.csv", stringsAsFactors = TRUE)
str(wb)
str(wb2019)
head(wb)
head(wb2019)

library(dplyr)
str(wb2019$Country.or.region)
str(wb$name)
unique(wb$name)
unique(wb2019$Country.or.region)

help(left_join)
wb1 <- left_join(wb, wb2019, by=c("name"="Country.or.region"))
head(wb1)
str(wb1)

# 2. 'iso2Code', 'adminregion', 'lendingType', 'capitalCity', 'longtitude' 컬럼은 드롭하고 새 데이터프레임의 컬럼별 데이터 유형과 결측값을 확인하시오.

library(tidyverse)
wb2 <- as.tibble(wb1)
wb2<-wb2 %>% select(-c('iso2Code', 'adminregion', 'lendingType', 'capitalCity', 'longitude'))
str(wb2)
summary(wb2)

# 3. 'latitude' 데이터를 기준으로 범주화하시오. 저위도지역(-30~0, 0~30), 중위도지역(-60~-30, 30~60), 고위도지역(-90~-60, 60~90) 또한 저위도지역과 중위도지역의 행복점수 'Score' 평균에 차이가 있는지 검정하시오.
wb3 <- wb2

boxplot(wb3$latitude)
describe(wb3$latitude)

help(transform)
wb3 <- transform(wb3, latitude.cate = ifelse(wb3$latitude >= -30 & wb3$latitude <= 30, "low",
                                          ifelse(wb3$latitude>=-60 & wb3$latitude <=60, "mid",
                                                 ifelse(wb3$latitude>=-90 & wb3$latitude <=90,"high",""))))
str(wb3$latitude.cate)
select(wb3,c("latitude.cate","latitude"))
wb3$latitude.cate<-type.convert(wb3$latitude.cate, na.strings="NA",)
str(wb3$latitude.cate)

str(wb3$latitude.cate)
wb3.lat <- wb3 %>% select(c("latitude.cate","Score"))


# 평균 차이 검정
str(wb3.lat)

nrow(distinct(wb3.lat,Score))
nrow(distinct(wb3.lat,latitude.cate))

wb3.aov <- aov(Score~latitude.cate, data=wb3.lat)
summary(wb3.aov) # H0 차이가 없다 기각 ->p값이 작아 행복지수 평균차이가 있음

TukeyHSD(wb3.aov) # H0 차이가 있다 -> 어떤 위도간에 행복지수 차이가 큰지 확인


# 4. 'incomeLevel' 범주를 기준으로 각 그룹의 수치형 데이터('Overall rank', 'GDP per capita' 제외)의 기술통계량을 정리하시오.

wb4 <- wb3[1:12]
glimpse(wb4)

wb4<-select(wb4,-c("name","region","Overall.rank","GDP.per.capita"))
str(wb4)
install.packages("doBy")
library(psych)

wb4 <- subset(wb4,incomeLevel=!"Aggregates")

describeBy(wb4~incomeLevel,skew=FALSE) 

# 5. 'incomeLevel'의 도수를 'low income' -> 'high income' 순의 누적막대그래프로 표현하시오.

wb5 <- wb3[1:12]
wb5 <- subset(wb5, incomeLevel %in% grep("income$", incomeLevel, value = TRUE))
# incomeLevel %in% grep("income$", incomeLevel, value = TRUE)
library(dplyr)
select(wb5,incomeLevel,region )
unique(wb5$incomeLevel)
unique(wb5$region)
str(wb5)

help(subset)
library(ggplot2)
ggplot(data=wb5)+ geom_bar(mapping= aes(x=region,fill=incomeLevel))
wb5$incomeLevel<-ordered(wb5$incomeLevel, levels=c("Low income","Lower middle income","Upper middle income","High income"))


# 5. 'Score', 'GDP per capita', 'Social support', 'Healthy life expectancy', 'Freedom to make life choices', 'Generosity', 'Perceptions of corruption'의 7개 변수 분포 그래프를 그리고, 해당 변수 간 상관관계 매트리스를 만드시오.

str(wb5)
wb5.2 <- na.omit(wb5)

cor(wb5.2[,6:12])

plot(wb5.2[,6:12])
library(psych)
cor.plot(wb5.2[,6:12])


# 6. 5에서 'Score'와 가장 상관관계가 높은 데이터의 Scatter plot을 통해 두 변수 간 상관성에 대한 의견을 기술하시오.

## 도시별GDP와 행복지수 및 사회적지지도, 예상수명은 서로간 상관관계가 높다


# 8. Score와 상관관계가 가장 높은 수치형 데이터의 bubble chart를 그리시오. (bubble의 색: 'region', bubble의 상대적 크기: 'Perceptions of corruption')
# GDP.per.capita가 Score와 가장 높은 상관관계
wb8 <- wb5
wb8 <- wb8 %>% select(c(region,Score, GDP.per.capita, Perceptions.of.corruption))
wb8 <- na.omit(wb8)
ggplot(wb8, aes(x=GDP.per.capita, y=Score))+
  geom_point(aes(size=Perceptions.of.corruption, color=region),shape=21)
#symbols(wb8$GDP.per.capita, wb8$Score, circles=wb8$Perceptions.of.corruption)
summary(wb8)
head(wb8)
# 9. 지시에 따른 전처리 사항과 결과를 pdf로 저장하여 제출 (파일이름 : 이름_전처리.pdf)

pdf("20210509조시은_전처리.pdf")

dev.off()



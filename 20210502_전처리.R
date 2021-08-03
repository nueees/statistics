# 1. crime_in_Seoul_incoude_gu_name.csv(서울시 경찰서별 범죄 발생/검거 현황) 파일을 가져와 구조 등 데이터 확인
# 2. 관서별 데이터를 구별로 변경
# 3. 파생변수 추가 : 각 범죄별 검거율 계산(강간검거율, 강도검거율, 살인검거율, 절도검거율, 폭력검거율
# 4. 변수 삭제 : 각 검거 건수
# 5. 변수명 변경 : 강간 발생 -> 강간 등 *발생에서 발생 삭제
# 6. 검거율 100 초과 시 100으로 변경
# 7. 정규화(최소/최대)
# 8. 강도, 살인, 폭력간의 상관관계 확인(매트릭스 및 그래프)
# 9. 범죄 검거 비율(정규화된 검거의 합으로 정렬), 범죄 발생 비율(정규화된 발생 건수로 정렬) 각각 히트맵 그리기
# 홍길동_output.csv, 홍길동_전처리.R, 홍길동_전처리.pdf (또는 DOC) 를 '전처리' 디렉토리에 제출

# * wide 형태의 데이터를 Long 형태로 변환(pivot)하기 
# * 변수가 많은 데이터를 로딩하여 수치형 변수만 뽑아서 상관계수 매트릭스 그리기 


setwd("C:/Users/Administrator/Documents/R/BDA/210502")
getwd()

# 1. crime_in_Seoul_incoude_gu_name.csv(서울시 경찰서별 범죄 발생/검거 현황) 파일을 가져와 구조 등 데이터 확인
crime_seoul <- read.csv("crime_in_Seoul_include_gu_name.csv", header=TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")
crime_seoul
head(crime_seoul)
str(crime_seoul)

# 2. 관서별 데이터를 구별로 변경 (pivot)
library(dplyr)
str(crime_seoul$관서명)
str(crime_seoul$구별)
unique(crime_seoul$관서명)
unique(crime_seoul$구별)


# 관서별(2) remove
pivot.crime_seoul0 <- crime_seoul[3:13]


# 다른방법 melt
library(reshape2)
help(melt)
help(dcast)
pivot.crime_seoul00 <- crime_seoul
pivot.crime_seoul00<-pivot.crime_seoul00[,c(11,1:10)]
str(pivot.crime_seoul00)
항목 <- c("살인.발생", "살인.검거", "강도.발생", "강도.검거", "강간.발생", "강간.검거", "절도.발생", "절도.검거", "폭력.발생", "폭력.검거")
crime.wide <- melt(pivot.crime_seoul00, id.vars=c("구별",항목) )
#crime.long <- dcast(crime.wide, 구별~...) 안됨..
head(crime.wide)


# 다른방법 tidyr
library(tidyr)
help(pivot_longer)

crime <- crime_seoul[3:13]
crime.pivot <- pivot_longer(data=crime, cols=-"구별", names_to="발생", values_to="항목")
str(crime.pivot)
head(crime.pivot)

head(crime.wide)
pivot_wider(pivot.crime_seoul00, id_cols="구별", names_from="항목" )

head(crime.wide)
pivot_longer(crime.wide, cols=-"구별", names_to="발생", values_to="항목" )





head(pivot.crime_seoul00)
#help(group_by)
pivot.crime_seoul0 <- pivot.crime_seoul0 %>% group_by( 살인.발생, 살인.검거, 강도.발생, 강도.검거, 강간.발생, 강간.검거, 절도.발생, 절도.검거, 폭력.발생, 폭력.검거)
head(pivot.crime_seoul0)

pivot.crime_seoul0[,c(11,1:10)]
pivot.crime_seoul <- pivot.crime_seoul0[,c(11,1:10)]
pivot.crime_seoul <- as.data.frame(pivot.crime_seoul)
str(pivot.crime_seoul)


# 3. 파생변수 추가 : 각 범죄별 검거율 계산(강간검거율, 강도검거율, 살인검거율, 절도검거율, 폭력검거율)

str(pivot.crime_seoul)
pivot.crime_seoul[,2:11]
pivot.crime_seoul[,10:11]

pivot.crime_seoul$살인.검거율 <- with( pivot.crime_seoul, 살인.검거율<- 살인.검거/살인.발생 )
pivot.crime_seoul$강도.검거율 <- with( pivot.crime_seoul, 강도.검거율<- 강도.검거/강도.발생 )
pivot.crime_seoul$강간.검거율 <- with( pivot.crime_seoul, 강간.검거율<- 강간.검거/강간.발생 )
pivot.crime_seoul$절도.검거율 <- with( pivot.crime_seoul, 절도.검거율<- 절도.검거/절도.발생 )
pivot.crime_seoul$폭력.검거율 <- with( pivot.crime_seoul, 폭력.검거율<- 폭력.검거/폭력.발생 )
str(pivot.crime_seoul)
#help(prop.table)
#table(prop.pivot.crime_seoul[12:16])

prop.pivot.crime_seoul <- pivot.crime_seoul


# 4. 변수 삭제 : 각 검거 건수

#install.packages('tidyverse')
#library('tidyverse')
#str_match(colnames(crime_seoul), "검거")


prop.pivot.crime_seoul.bak <- prop.pivot.crime_seoul

grep("검거$",colnames(prop.pivot.crime_seoul))
head(prop.pivot.crime_seoul)
rm1p.prop.pivot.crime_seoul <- prop.pivot.crime_seoul[,-c(grep("검거$",colnames(prop.pivot.crime_seoul)))]
head(rm1p.prop.pivot.crime_seoul)



# 6. 검거율 100 초과 시 100으로 변경

str(rm1p.prop.pivot.crime_seoul)
summary(rm1p.prop.pivot.crime_seoul)
rm1p.prop.pivot.crime_seoul$살인.검거율
rm1p.prop.pivot.crime_seoul$강도.검거율
rm1p.prop.pivot.crime_seoul$강간.검거율

# library(magrittr) #pipeline
# 변수명 너무 길어서 6으로 바꿈
crime_seoul6 <- rm1p.prop.pivot.crime_seoul
crime_seoul6$살인.검거율 <- ifelse(crime_seoul6$살인.검거율>1.0, 1.0, crime_seoul6$살인.검거율)
crime_seoul6$강도.검거율 <- ifelse(crime_seoul6$강도.검거율>1.0, 1.0, crime_seoul6$강도.검거율)
crime_seoul6$강간.검거율 <- ifelse(crime_seoul6$강간.검거율>1.0, 1.0, crime_seoul6$강간.검거율)

summary(crime_seoul6)
str(crime_seoul6)
head(crime_seoul6)

crime_seoul6.bak <- crime_seoul6

# 7. 정규화(최소/최대)


crime_seoul7 <- crime_seoul6

살인_min <- min(crime_seoul7$살인.발생)
강도_min <- min(crime_seoul7$강도.발생)
강간_min <- min(crime_seoul7$강간.발생)
절도_min <- min(crime_seoul7$절도.발생)
폭력_min <- min(crime_seoul7$폭력.발생)
살인_max <- max(crime_seoul7$살인.발생)
강도_max <- max(crime_seoul7$강도.발생)
강간_max <- max(crime_seoul7$강간.발생)
절도_max <- max(crime_seoul7$절도.발생)
폭력_max <- max(crime_seoul7$폭력.발생)

summary(crime_seoul7)
help(scale)
crime_seoul7$살인norm <- scale(crime_seoul7$살인.발생, center=살인_min, scale=살인_max-살인_min)
crime_seoul7$강도norm <- scale(crime_seoul7$강도.발생, center=강도_min, scale=강도_max-강도_min)
crime_seoul7$강간norm <- scale(crime_seoul7$강간.발생, center=강간_min, scale=강간_max-강간_min)
crime_seoul7$절도norm <- scale(crime_seoul7$절도.발생, center=절도_min, scale=절도_max-절도_min)
crime_seoul7$폭력norm <- scale(crime_seoul7$폭력.발생, center=폭력_min, scale=폭력_max-폭력_min)

str(crime_seoul7)


# 8. 강도, 살인, 폭력간의 상관관계 확인(매트릭스 및 그래프)

crime_seoul8 <- crime_seoul7
str(crime_seoul8)
crime_seoul8 <- crime_seoul8[,c(12,13,16)]

class(crime_seoul8)

help(cor)
cor(crime_seoul8)
plot(crime_seoul8)
pairs(crime_seoul8, panel=panel.smooth)

# 9. 범죄 검거 비율(정규화된 검거의 합으로 정렬), 범죄 발생 비율(정규화된 발생 건수로 정렬) 각각 히트맵 그리기

str(prop.pivot.crime_seoul)
crime_seoul9 <- prop.pivot.crime_seoul[1:11]
str(crime_seoul9)

install.packages("pheatmap")
library(pheatmap)

#검거비율

hm검거 <- pheatmap(crime_seoul9[c(3,5,7,9,11)], scale="column")


#발생비율


hm발생 <- pheatmap(crime_seoul9[c(2,4,6,8,10)], scale="column")

#############################################################################








#검거비율

살인검거_min <- min(crime_seoul9$살인.검거)
강도검거_min <- min(crime_seoul9$강도.검거)
강간검거_min <- min(crime_seoul9$강간.검거)
절도검거_min <- min(crime_seoul9$절도.검거)
폭력검거_min <- min(crime_seoul9$폭력.검거)
살인검거_max <- max(crime_seoul9$살인.검거)
강도검거_max <- max(crime_seoul9$강도.검거)
강간검거_max <- max(crime_seoul9$강간.검거)
절도검거_max <- max(crime_seoul9$절도.검거)
폭력검거_max <- max(crime_seoul9$폭력.검거)

crime_seoul9$살인검거norm <- scale(crime_seoul9$살인.검거, center=살인검거_min, scale=살인검거_max-살인검거_min)
crime_seoul9$강도검거norm <- scale(crime_seoul9$강도.검거, center=강도검거_min, scale=강도검거_max-강도검거_min)
crime_seoul9$강간검거norm <- scale(crime_seoul9$강간.검거, center=강간검거_min, scale=강간검거_max-강간검거_min)
crime_seoul9$절도검거norm <- scale(crime_seoul9$절도.검거, center=절도검거_min, scale=절도검거_max-절도검거_min)
crime_seoul9$폭력검거norm <- scale(crime_seoul9$폭력.검거, center=폭력검거_min, scale=폭력검거_max-폭력검거_min)

str(crime_seoul9)
#crime_seoul9 <- crime_seoul9[,-c(24:28)]
crime_seoul9_검거 <- crime_seoul9[,c(12:16)]

str(crime_seoul9_검거)
heatmap(crime_seoul9_검거)

#발생건수

살인발생_min <- min(crime_seoul9$살인.발생)
강도발생_min <- min(crime_seoul9$강도.발생)
강간발생_min <- min(crime_seoul9$강간.발생)
절도발생_min <- min(crime_seoul9$절도.발생)
폭력발생_min <- min(crime_seoul9$폭력.발생)
살인발생_max <- max(crime_seoul9$살인.발생)
강도발생_max <- max(crime_seoul9$강도.발생)
강간발생_max <- max(crime_seoul9$강간.발생)
절도발생_max <- max(crime_seoul9$절도.발생)
폭력발생_max <- max(crime_seoul9$폭력.발생)

crime_seoul9$살인발생norm <- scale(crime_seoul9$살인.발생, center=살인발생_min, scale=살인발생_max-살인발생_min)
crime_seoul9$강도발생norm <- scale(crime_seoul9$강도.발생, center=강도발생_min, scale=강도발생_max-강도발생_min)
crime_seoul9$강간발생norm <- scale(crime_seoul9$강간.발생, center=강간발생_min, scale=강간발생_max-강간발생_min)
crime_seoul9$절도발생norm <- scale(crime_seoul9$절도.발생, center=절도발생_min, scale=절도발생_max-절도발생_min)
crime_seoul9$폭력발생norm <- scale(crime_seoul9$폭력.발생, center=폭력발생_min, scale=폭력발생_max-폭력발생_min)

crime_seoul9_발생 <- crime_seoul9[,c(17:21)]

head(crime_seoul9_발생)

#help(prop.table)






########################### ver2 #################################
library(plyr);library(dplyr);library(tidyr);

# 1. crime_in_Seoul_incoude_gu_name.csv(서울시 경찰서별 범죄 발생/검거 현황) 파일을 가져와 구조 등 데이터 확인
crime_seoul <- read.csv("crime_in_Seoul_include_gu_name.csv", header=TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")
crime_seoul
head(crime_seoul)
str(crime_seoul)

str(crime.1)
sapply(crime.1[,"발생검거"])
gsub("발생","##",crime.1[,"발생검거"])

pivot_longer(crime_seoul[2:12], cols = -"관서명")
crime.1 <- pivot_longer(crime_seoul[3:13], cols = -"구별",names_to="발생검거", values_to="건수")


crime.1[grep("강간",crime.1$발생검거),"발생검거"] <- sapply(crime.1[grep("강간",crime.1$발생검거),"발생검거"], function(x) gsub("발생","**",x))

crime.1 %>% filter(grepl("강간",발생검거)) <- sapply(crime.1 %>% filter(grepl("강간",발생검거)), function(x) gsub("발생","**",x))

# crime.1[grep("강간",crime.1$발생검거),]
# 1) crime[grep("강간",crime$발생검거),"발생검거"] 혹은 
# crime %>% filter(grepl("강간",발생검거)) 
# 패턴, 데이터 순서 /  dots는 []나 \\을 씌워줘야 함 / grep은 행 번호/ grepl은 논리값(T F)으로 출력
# 
# 2) 금액df[2:5]<- sapply(금액df[2:5], function(x) gsub(",","",x))
# 기존패턴,바꿀패턴,데이터 순서 / dots는 []나 \\을 씌워줘야 함 / gsub은 전체 데이터 대상, sub은 데이터 내 패턴 여럿있으면 첫번째 만나는 패턴만(1,000,000->1000,000)



crime.2 %>% group_by(구별) %>% summarise(건수=n()) 

crime.3<-crime.2

# crime.3[grep("검거",crime.3$발생검거) &&(crime.3$구별==l구별[1]),] %>% mutate(정규화=scale(건수))
# crime.3[grep("검거",crime.3$발생검거),]
# crime.3[which(crime.3$구별==l구별[1]),]

crime.3 %>% filter(grepl("검거",발생검거))%>% filter(구별==l구별[1])  

# l구별<-levels(crime.3$구별)
# l구별[1]
#  for (i in 1:length(l구별)){
#   tmpX <- paste0("tmp",i)
#   print(tmpX)
#   crime.3[grep("검거",crime.3$발생검거,] %>% mutate(정규화=scale(건수)) 
#   crime.3[crime.3$구별==l구별[i],] %>% mutate(정규화=scale(건수))  
#   tmpX <- tmpX %>% mutate(정규화=as.numeric(scale(건수)))
#   print(tmpX)
# }
#rm(l.crime)
l구별<-levels(crime.3$구별)
l.crime<- as.list(c(1:length(l구별)))
for (i in 1:length(l구별)){
  print(l구별[i])
  l.crime[[i]] <- crime.3 %>% filter(grepl("검거",발생검거))%>% filter(구별==l구별[i])  
  l.crime[[i]] <- l.crime[[i]] %>% mutate(정규화=as.numeric(scale(건수)))
  print(l.crime[[i]])
}
l.crime %>% lapply(count) %>% unlist() %>% sum()
crime.4 <- do.call(rbind.data.frame, l.crime)
str(crime.4)
nrow(crime.4)
crime.5 <- do.call(rbind, lapply(l.crime, data.frame, stringsAsFactors=FALSE))
str(crime.5)
nrow(crime.5)

crime.4 %>% group_by(구별) %>% summarise(발생검거,건수,정규화)



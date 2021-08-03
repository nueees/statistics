library(ggmap)
library(plyr) #rename
library(lubridate) #dateType
library(magrittr) # pipeline

# 1) daily-website-visitors.csv 파일을 로딩하여 기본적인 데이터 타입을 확인하시오
# 2) Date, Page.Loads, Unique.Visits, First.Time.Visits, Returning.Visits 열만 추출해서 output.csv 라는 파일로 저장하시오. 
# 3) 모든 열의 이름을 소문자로 변경하고 점(.)을 언더바(_)로 변경하시오. 
# 4) date라는 열은 날짜형으로 변경하고, 나머지는 수치형으로 변경하시오. 
# 5) NA인 값들은 모두 평균으로 대체하시오. 
# 6) date열에서 년, 월, 일, 요일을 각 변수로 추출하시오. 
# 7) X축을 시간으로 두고, Y축을 4개의 수치형 변수를 점 그래프, 라인 차트로 표현하시오. 
# 8) Returning.Visits 열을 4분위수로 가장 작은 숫자 순으로 A(하위 25%), B(50%), C(75%), D(100%)로 범주형으로 변환하고 Grade라는 변수명을 생성하시오. (기존 Returning.Visits 은 그대로 둘 것) 
# 9) 4개의 수치형 데이터가 각 요일별로 상이한지를 BoxPlot 을 그려서 시각화 하시오. 
# 제출파일은 본인이름(예시 홍길동)을 이용하여 본인 폴더에 제출
# 홍길동_output.csv, 홍길동_전처리.R, 홍길동_전처리.pdf (또는 DOC) 


setwd("C:/Users/Administrator/Documents/R/BDA")
getwd()

#1) 데이터 타입확인
visitors.df <- read.csv("daily-website-visitors.csv", header=TRUE, stringsAsFactors = TRUE,)

class(visitors.df)
nrow(visitors.df)
ncol(visitors.df)
str(visitors.df)

visitors <- visitors.df[,c(4:ncol(visitors.df))]
#2)특정열만 새로 저장
write.csv(visitors, file="output.csv",row.names = T)

dim(visitors)
str(visitors)

#3)컬럼명소문자 변경 .to_
colnames(visitors)
colnames(visitors) <- tolower(colnames(visitors))
help(replace)
library(plyr) #rename
library(dplyr) #rename_with
install.packages("tidyverse") 
library(tidyverse) #str_replace_all

#rename_with(visitors, ~ tolower(gsub(".", "_", .)))
#rename_with(colnames(visitors), ~ gsub(".", "_", .))
colnames(visitors) <- gsub("[.]","_", colnames(visitors))
colnames(visitors)

#4) date 날짜형 나머지 수치형 commma to ""
str(visitors)

#visitors[,"date"] <- as.Date.POSIXct( visitors[,"date"],format="%Y-%m-%d" ) 

library(lubridate) #dateType
library(help=lubridate )

library(magrittr) # pipeline

visitors$date <- mdy(visitors$date)
mdy("5/31/2021")

visitors[,"date"]
str(visitors)
head(visitors)

# 4)commma to ""

visitors$page_loads <- as.numeric(gsub(",","", visitors$page_loads ))
visitors$unique_visits <- as.numeric(gsub(",","", visitors$unique_visits ))
visitors$first_time_visits <- as.numeric(gsub(",","", visitors$first_time_visits ))
visitors$returning_visits <- as.numeric(gsub(",","", visitors$returning_visits ))

visitors_bak <- visitors

# 6) date열에서 년, 월, 일, 요일을 각 변수로 추출하시오. 

head(visitors$date)
v.year <- year(visitors$date)
head(v.year)
v.month  <- month(visitors$date)
head(v.month)
v.day <- day(visitors$date)
head(v.day)
v.wday <- wday(visitors$date)
head(v.wday)

class(v.year)
#visitors <- cbind(visitors, v.year)
visitors <- transform(visitors, year=v.year)
visitors <- transform(visitors, month=v.month)
visitors <- transform(visitors, day=v.day)
visitors <- transform(visitors, wday=v.wday)
#type.convert()

  
# 5) NA인 값들은 모두 평균으로 대체하시오.

str(visitors)
visitors_bak <- visitors
visitors <-visitors_bak

head(visitors)
visitors[1,2] <- as.numeric("")
visitors[2,3] <- as.numeric("")
visitors[3,4] <- as.numeric("")
visitors[4,5] <- as.numeric("")


visitors_bak2 <-visitors


table(is.na(visitors$date))
table(is.na(visitors_2$page_loads))
table(is.na(visitors$unique_visits))
table(is.na(visitors$first_time_visits))
table(is.na(visitors$returning_visits))

library(dplyr) #filter : subset row using column value
library(help=dplyr)
visitors %>% filter(is.na(date))


head(visitors)
visitors$page_loads <- ifelse(!is.na(visitors$page_loads), visitors$page_loads, round(mean(visitors$page_loads,na.rm=TRUE),0))
visitors$unique_visits <- ifelse(!is.na(visitors$unique_visits), visitors$unique_visits, round(mean(visitors$unique_visits,na.rm=TRUE),0))
visitors$first_time_visits <- ifelse(!is.na(visitors$first_time_visits), visitors$first_time_visits, round(mean(visitors$first_time_visits,na.rm=TRUE),0))
visitors$returning_visits <- ifelse(!is.na(visitors$returning_visits), visitors$returning_visits, round(mean(visitors$returning_visits,na.rm=TRUE),0))


head(visitors_2)
str(visitors_2)
visitors_2 <- within(visitors, {
              page_loads <- ifelse(is.na(page_loads), round(mean(page_loads, na.rm=TRUE),0), page_loads)
              })



# 7) X축을 시간으로 두고, Y축을 4개의 수치형 변수를 점 그래프, 라인 차트로 표현하시오. 

visitors_bak3 <-visitors

library(MASS)
install.packages("esquisse")
library(ggplot2)

str(visitors)
dim(visitors)
#plot(Cars93$Length, Cars93$Weight, main="Cars93",xlab="*Length",ylab="*Weight")


ggplot(visitors) +
  aes(x = date, y = page_loads) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()


ggplot(visitors) + aes(x=date, y=page.loads)+ geom_line()
ggplot(visitors) + aes(x=date, y=page.loads)+ geom_point()

# 8) Returning.Visits 열을 4분위수로 가장 작은 숫자 순으로 A(하위 25%), B(50%), C(75%), D(100%)로 범주형으로 변환하고 Grade라는 변수명을 생성하시오. (기존 Returning.Visits 은 그대로 둘 것) 

library(magrittr) # pipeline
returning.visits4 <- quantile(visitors$returning_visits)
returning.visits4 %>% quantile()

quantile(visitors$returning_visits, 0.25)

visitors_1 <- transform( visitors,  returning_visits4 = 
                           {ifelse(returning_visits < quantile(returning_visits, 0.25), "1Q",
                             ifelse(returning_visits >= quantile(returning_visits, 0.25) 
                                    & returning_visits < quantile(returning_visits, 0.50), "2Q",
                              ifelse(returning_visits >= quantile(returning_visits, 0.50) 
                                     & returning_visits < quantile(returning_visits, 0.75), "3Q",
                               ifelse(returning_visits >= quantile(returning_visits, 0.75), "4Q",
                                 "?"))))})

head(visitors_1)
str(visitors_1)
table(visitors_1$returning_visits4)



# 9) 4개의 수치형 데이터가 각 요일별로 상이한지를 BoxPlot 을 그려서 시각화 하시오. 

str(visitors)
head(visitors)



boxplot(page_loads~wday, visitors, main="요일별 방문자", ylab="방문자수")
boxplot(unique_visits~wday, visitors, main="요일별 방문자", ylab="방문자수")
boxplot(first_time_visits~wday, visitors, main="요일별 방문자", ylab="방문자수")
boxplot(returning_visits~wday, visitors, main="요일별 방문자", ylab="방문자수")






############################ ver2 ##################################
library(plyr);library(dplyr);library(magrittr);library(lubridate);library(stringr);
visitors <- read.csv("daily-website-visitors.csv", header=TRUE)
str(visitors)

vst <- visitors %>% select(Date,Page.Loads,Unique.Visits,First.Time.Visits,Returning.Visits)

vst.1 <- vst %>% mutate(mdy=mdy(Date), year=year(mdy), month=month(mdy), day=day(mdy), wday=wday((mdy),label=TRUE)) 
str(vst.1)

vst.1[2:5]<- sapply(vst.1[2:5], function(x) gsub(",","",x))
vst.1[2:5] %>% head()
str(vst.1)
vst.1$Page.Loads <-vst.1$Page.Loads %>% as.numeric()
vst.1$Unique.Visits <-vst.1$Unique.Visits %>% as.numeric()
vst.1$First.Time.Visits <-vst.1$First.Time.Visits %>% as.numeric()
vst.1$Returning.Visits <-vst.1$Returning.Visits %>% as.numeric()

str(vst.1)

length(vst.1$Returning.Visits)
nrow(vst.1$Returning.Visits)

quantile(vst.1$Returning.Visits)
vst.2 <- vst.1 %>% mutate(Grade=ifelse(vst.1$Returning.Visits<=quantile(vst.1$Returning.Visits)[2],"A",
                                       ifelse(vst.1$Returning.Visits<=quantile(vst.1$Returning.Visits)[3],"B",
                                              ifelse(vst.1$Returning.Visits<=quantile(vst.1$Returning.Visits)[4],"C",
                                                     ifelse(vst.1$Returning.Visits<=quantile(vst.1$Returning.Visits)[5],"D","?"))))
)

vst.2$Grade <- vst.2$Grade %>% factor(level=c("A","B","C","D"),labels=c("25%","50%","75%","100%"))

str(vst.2)

for (i in 1:ncol(vst.1)){
  #vst.1[,i] %>% as.numeric
}

# 9) 4개의 수치형 데이터가 각 요일별로 상이한지를 BoxPlot 을 그려서 시각화 하시오. 

vst.2 %>% group_by(wday) %>% summarise(m.Page.Loads=mean(Page.Loads), m.Unique.Visits=mean(Unique.Visits), m.First.Time.Visits=mean(First.Time.Visits), m.Returning.Visits=mean(Returning.Visits))











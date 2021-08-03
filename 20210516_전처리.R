getwd()
setwd("C:/Users/Administrator/Documents/R/BDA/210516")

# 1. 데이터 읽기 및 전처리
# 1-1. subway.csv(main), subway_latlong.csv(sub) 읽어와서 구조 확인

subway <- read.csv("subway.csv", encoding="UTF-8")
str(subway) #236493, 45

subname <- read.csv("subway_latlong.csv")
str(subname)
head(subname, 50)

colSums(is.na(subway))

# 몇개 숫자인데 문자형인거 수정
str(subway)
str(subway[,4:45])

for( col in 4:45 ){
  subway[,col] <- as.integer(subway[,col])
  #print(str(subway[,col]))
}
#warnings()
#sapply(subway,class)
str(subway[,4:45])


# 1-2. income_date를 date타입으로 변경하기
subway[,"income_date"] <- as.character(subway[,"income_date"])
#class(subway[,"income_date"])
subway[,"income_date"] <- as.Date(subway[,"income_date"], format="%Y%m%d")
#class(subway[,"income_date"])
unique(format(subway[,"income_date"],"%Y%m"))

# 1-3. 7월까지 밖에 없는 income_date 2014년도 데이터 제외하기
library(dplyr)
subway.1 <- subway %>% filter(format(subway$income_date,"%Y")!="2014")
#subway.1 <- subset(subway,format(subway$income_date,"%Y")!="2014")

# 1-4. 다른 호선의 같은 역명(stat_name뒤에 괄호)을 하나의 역명으로 처리 ex) 천호(5),천호(8) -> 천호
sort(unique(subway.1[,"stat_name"]))

dupname.idx <- grep("\\(",subway.1$stat_name)
unique(subway.1$stat_name[dupname.idx])
subway.1$stat_name[dupname.idx] <- substr(subway.1$stat_name[dupname.idx], 1, nchar(subway.1[dupname.idx,"stat_name"])-3)
unique(subway.1$stat_name)

# 1-5. income_date에서 추출한 연,월 컬럼 추가

year <- format(subway.1$income_date, "%Y")
month <- format(subway.1$income_date, "%m")


subway.1 <- mutate(subway.1,year,month)
head(subway.1)

# 2. 탑승객 상위 5위 역 구하고 해당 탑승객수 출력 및 호선 정보 출력

# 2-1. stat_name 기준으로 탑승객(on_tot) 상위 5개 출력
subway.2 <- subway.1

subway.2.grp <- subway.2 %>% group_by(stat_name) %>% summarise(on_tot = sum(on_tot, na.rm = TRUE)) 

subway.2.grp %>% top_n(n = 5, wt = on_tot) 

# 2-2. stat_name 기준으로 left join으로 sub파일 내 STATION_NM과 조인해서 역 호선 정보(LINE_NUM) 출력

subway.2.join <- left_join(x=subway.2.grp, y=subname, by=c("stat_name"="STATION_NM"))

subway.2.join <- data.frame(stat_name=subway.2.join$stat_name, line_num=subway.2.join$LINE_NUM, on_tot=subway.2.join$on_tot)

subway.2.join

# 2-3. 노선별로 정렬
#subway.2.byline <- subway.2.join[with(subway.2.join, order(line_num)),]
subway.2.byline <- subway.2.join[order(subway.2.join$line_num),]

# 2-4. 역이름 factor타입으로 변경

subway.2.byline$stat_name <- as.factor(subway.2.byline$stat_name)
str(subway.2.join)

subway.2.join
# 3. 탑승객 수 상위 10개 역 구하고, 역(stat_name)별 탑승객 수(on_tot) 막대그래프 그리기 (노선별로는 색으로 구분)
subway.3 <- subway.2.join
str(subway.2.join)
subway.3 <- subway.3[order(-subway.3$on_tot),]
subway.3 <- subway.3[1:10,]
ggplot(subway.3, aes(x=stat_name, y=on_tot, fill=line_num))+
  geom_bar(stat="identity",colour="black")+
  xlab("상위10개역")+
  scale_y_continuous("탑승객수",lim=c(0,max(subway.3$on_tot)))+scale_fill_discrete("호선")

#help(scale_y_continuous)

# 4. 탑승객 상위 10개역의 2013년도 월별 역별 승객 수 구하고 추이도 그래프 그리기. (x=월month,y=탑승객on_tot, group=역명stat_name->선으로 )
stat.10 <- subway.3$stat_name
subway.4 <- subway.1 #메인데이터

subway.4 <- subset(subway.4, stat_name %in% stat.10 & year=="2013") %>% select(c("stat_name","on_tot","month"))
str(subway.4)
subway.4 <- subway.4 %>% group_by(stat_name,month) %>% summarise(on_tot = sum(on_tot, na.rm=TRUE))

ggplot(subway.4, aes(x=month,y=on_tot,colour=stat_name, group=stat_name))+
  geom_line()+
  geom_point()+
  xlab("2013년")+
  ylab("탑승객수")+
  scale_color_discrete("역명")

# 5. 노선별 평균 지하철 탑승객 수 구하고 파이차트 그리기

subway.5.join <- left_join(x=subway.1, y=subname, by=c("stat_name"="STATION_NM"))

subway.5 <- subway.5.join %>% select(c("stat_name","on_tot","LINE_NUM"))

subway.5 <- subway.5 %>% group_by(LINE_NUM) %>% summarise(on_tot=mean(on_tot))
subway.5 <- subway.5[complete.cases(subway.5),]

#help(pie)
unique(paste0(subway.5$LINE_NUM,"호선"),)
help(pie)
pie(subway.5$on_tot, labels=unique(paste0(subway.5$LINE_NUM,"호선"), main="노선별 평균 지하철 탑승객 수"))

# 6. 노선별 누적 승객 수의 상대 비교하고 영역차트 그리기 (x축 YYYY-MM, y축 누적승객수, fill=호선)
library(ggplot2)

subway.6 <- subway.5.join %>% select(c("on_tot","LINE_NUM","year","month"))
subway.6 <- subway.6[complete.cases(subway.6),]
str(subway.6)
subway.6

yearmonth <- paste(subway.6$year,subway.6$month,sep="-")
subway.6 <- mutate(subway.6,yearmonth)
subway.6$LINE_NUM <- paste0(subway.6$LINE_NUM,"호선")

subway.6 <- subway.6 %>% group_by(LINE_NUM,yearmonth) %>% summarise(on_tot=sum(on_tot, na.rm=TRUE))
tail(subway.6)

ggplot(subway.6, aes(x=yearmonth,y=on_tot, fill=LINE_NUM, group=LINE_NUM))+
  geom_area()

# 2010년도 꺼만 확인
sub.2013 <-subway.6[grep("^2010",subway.6$yearmonth),]
tail(sub.2013)

ggplot(sub.2013, aes(x=yearmonth,y=on_tot, fill=LINE_NUM, group=LINE_NUM))+
  geom_area()



# 7. 시간대별 호선별 평균 탑승객(on_HH컬럼) 수의 상대 비교하고 추이도 그래프 그리기 (x축:탑승시간대(00~24), y축:탑승객수, group=호선)
str(subway.1)

subway.2.join <- left_join(x=subway.1, y=subname, by=c("stat_name"="STATION_NM"))

subway.7 <- subway.2.join

str(subway.7)


# 필요 컬럼 select
grep("^on_", colnames(subway.7))
subway.7 <- select(subway.7,"LINE_NUM",grep("^on_", colnames(subway.7)))
subway.7 <- select(subway.7, -c("on_tot"))
str(subway.7)
head(subway.7)
# 컬럼 pivot

library(tidyr)
subway.7.longer <- pivot_longer(data=subway.7, col=-LINE_NUM, values_to="cnt", names_to="on_HH")
str(subway.7.longer)
subway.7.longer

subway.7.agg <- subway.7.longer %>% group_by(LINE_NUM,on_HH) %>% summarise(cnt.HH=mean(cnt, na.rm=TRUE)) %>% arrange(on_HH)

head(subway.7.agg,20)
str(subway.7.agg)
library(ggplot2)
plot.7 <- ggplot(subway.7.agg, aes(x=on_HH, y=cnt.HH, color=LINE_NUM, group=LINE_NUM))+
  geom_line()+
  geom_point()

plot.7 + xlab("탑승시간대")+ylab("평균탑승객수")+scale_color_discrete("호선")


# 8. Feature Scaling

# 8-1.  Murder, Assault 변수를 z 표준화 후 히스토그램으로 확인 
write.csv(USArrests, file="USArrests.csv")
zArrst <- data.frame(USArrests)
summary(zArrst)
zArrst <- mutate(zArrst, zMurder=scale(Murder), zAssault=scale(Assault))

hist(zArrst$zMurder)
hist(zArrst$zAssault)

# 8-2. Murder 변수를 min max 정규화
nArrst<-data.frame(USArrests)

arrst.min<-min(nArrst$Murder)
arrst.max<-max(nArrst$Murder)

nArrst <- mutate(nArrst, nMurder=scale(NArrst$Murder, center=arrst.min, scale=arrst.max-arrst.min))

hist(nArrst$nMurder)


# html로 니팅? 방법..찾아보기



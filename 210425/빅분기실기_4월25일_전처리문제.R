# 1) daily-website-visitors.csv 파일을 로딩하여 기본적인 데이터 타입을 확인하시오
library(dplyr)
data = read.csv("D:/빅분기실기/전처리/daily-website-visitors.csv")
library(skimr)
data %>% skim()

# 2) Date, Page.Loads, Unique.Visits, First.Time.Visits, Returning.Visits 열만 추출해서 output.csv 라는 파일로 저장하시오. 
data %>%  head()
data_org <-  data # backup
data <-  data_org # recovery 
data <- data %>%  dplyr::select("Date", "Page.Loads", "Unique.Visits", "First.Time.Visits", "Returning.Visits")
data %>%  skim()
data %>%  head()
# 3) 모든 열의 이름을 소문자로 변경하고 점(.)을 언더바(_)로 변경하시오. 
data <-  rename(data, date =Date , page_loads = Page.Loads, unique_visits = Unique.Visits, first_time_visits = First.Time.Visits, returning_visits = Returning.Visits  );
data %>%  head()

# 4) date라는 열은 날짜형으로 변경하고, 나머지는 수치형으로 변경하시오. 
data$date <-  as.character(data$date)
library(lubridate)
data$date <-  mdy(data$date)
data$date %>% class()

data %>%  str()
data$page_loads <-  gsub(',', '', data$page_loads)
data$unique_visits <-  gsub(',', '', data$unique_visits)
data$first_time_visits <-  gsub(',', '', data$first_time_visits)
data$returning_visits <-  gsub(',', '', data$returning_visits)

data %>% head()
data$page_loads <-  as.numeric(data$page_loads)
data$unique_visits <-  as.numeric(data$unique_visits)
data$first_time_visits <-  as.numeric(data$first_time_visits)
data$returning_visits <-  as.numeric(data$returning_visits)



# 5) NA인 값들은 모두 평균으로 대체하시오. 
data %>%  is.na() %>%  sum()

# 6) date열에서 년, 월, 일, 요일을 각 변수로 추출하시오. 
data$year  <- year(data$date)
data$month <- month(data$date)
data$day   <- day(data$date)
data$wday  <- wday(data$date)

data %>%  head()

# 7) X축을 시간으로 두고, Y축을 4개의 수치형 변수를 점 그래프, 라인 차트로 표현하시오.
library(esquisse)
library(ggplot2)

ggplot(data) + aes(x = date, y = page_loads) + geom_line(size = 1L, colour = "#0c4c8a") + theme_minimal()
ggplot(data) + aes(x = date, y = page_loads) + geom_point(size = 1L, colour = "#0c4c8a") + theme_minimal()

ggplot(data) + aes(x = date, y = unique_visits) + geom_line(size = 1L, colour = "#0c4c8a") + theme_minimal()
ggplot(data) + aes(x = date, y = unique_visits) + geom_point(size = 1L, colour = "#0c4c8a") + theme_minimal()


ggplot(data) + aes(x = date, y = first_time_visits) + geom_line(size = 1L, colour = "#0c4c8a") + theme_minimal()
ggplot(data) + aes(x = date, y = first_time_visits) + geom_point(size = 1L, colour = "#0c4c8a") + theme_minimal()


ggplot(data) + aes(x = date, y = returning_visits) + geom_line(size = 1L, colour = "#0c4c8a") + theme_minimal()
ggplot(data) + aes(x = date, y = returning_visits) + geom_point(size = 1L, colour = "#0c4c8a") + theme_minimal()


# 8) Returning.Visits 열을 4분위수로 가장 작은 숫자 순으로 A(하위 25%), B(50%), C(75%), D(100%)로 범주형으로 변환하고 Grade라는 변수명을 생성하시오. (기존 Returning.Visits 은 그대로 둘 것) 

# 9) 4개의 수치형 데이터가 각 요일별로 상이한지를 BoxPlot 을 그려서 시각화 하시오. 
# 제출파일은 본인이름(예시 홍길동)을 이용하여 본인 폴더에 제출
# 홍길동_output.csv, 홍길동_전처리.R, 홍길동_전처리.pdf (또는 DOC) 







data %>%  head()

data$Returning.Visits <- as.numeric(data$Returning.Visits)

data$Returning.Visits %>%  boxplot()


data(iris)

write.csv(iris, file = "alldata.csv")
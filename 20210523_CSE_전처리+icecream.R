setwd("C:/Users/Administrator/Documents/R/BDA/210523")

# 1. "Ch0701.OST.csv"는 한 아이스크림 가게에서 판매한 파인트의 g수를 측정한 데이터이다. 표본조사결과 아이스크림무게가 본사에서 공지한 파인트의 무게 320g와 차이가 나는지 검정하라. 
icecream <- read.csv("Ch0701.OST.csv", stringsAsFactors=TRUE, encoding="UTF-8")
head(icecream)
# 1.1 데이터를 불러와서 구조를 확인하고, 소수점 2자리에서 반올림하고, boxplot, 히스토그램으로 시각화 하라. 
str(icecream)
summary(icecream)

icecream.r <- round(icecream$weight,1)
install.packages("ggplot2")
library(ggplot2);library(dplyr);
boxplot(icecream.r)
hist(icecream.r)


#shapiro.test(icecream.r) #n수 30이상이라 정규성검정 안해도됨

# 1.2 평균 아이스크림 무게가 320g과 같은지 검정하고 그 결과를 print함수로 출력하라.(양측검정, 유의수준 0.05)
icecream.t <- t.test(icecream, alternative="two.sided", mu=290, conf.level=0.95)
print(icecream.t) # 귀무가설(평균320)


### tidyverse로 기본적인 통계검정
icecream %>% 
  t_test(response=weight,alternative="two.sided", mu=290, conf_level=0.95)
# p_value ; 0.00778 귀무 기각

# find the point estimate 추정치 확인
estmt_ice <- icecream %>% specify(response=weight) %>% 
  calculate(stat="mean")
# estmt_ice ; 295 (mean(icecream$weight))


## hypothesize 귀무가설 만들고
null_ice<- icecream %>% specify(response=weight) %>% 
  hypothesize(null="point", mu=290) %>% 
  generate(reps=1000, type="bootstrap") %>% 
  calculate(stat = "mean")

## 그래프로 확인
null_ice %>% visualise()
null_ice %>% visualise()+shade_p_value(obs_stat=estmt_ice, direction="two-sided")
pvalue_ice <- null_ice %>% get_p_value(obs_stat=estmt_ice, direction="two-sided")
# pvalue_ice ; 0.004 귀무가설 기각

# calculate the confidence interval around the point estimate 95신뢰구간se
null_ice %>%
  get_confidence_interval(point_estimate = estmt_ice,
                          # at the 95% confidence level
                          level = .95,
                          # using the standard error
                          type = "se")
# lower_ci ; 291, upper_ci ; 299





# 2. MASS패키지의 cats data는 고양이의 성별Sex, 몸무게Bwt, 심장무게Hwt를 담고 있다. 고양이들의 평균몸무게가 2.6kg인지 아닌지에 대한 통계적 검정을 수행하라.(양측검정, 유의수준 = 0.05)
# 2.1 데이터 불러와서 구조 확인
# 2.2 Bwt(고양이 몸무게) 변수에 대한 정규성 여부 판단
# 2.3 평균 고양이 몸무게에 대한 검정을 수행
# 2.4 결론을 print()함수로 출력하라.
install.packages("MASS")
library(MASS)
str(cats)
shapiro.test(cats$Bwt) #귀무가설(정규성만족) 기각 -> wilcox 
cats.w <- wilcox.test(cats$Bwt, mu=2.6, alternative="two.sided", conf.level=0.95)
print(cats.w) # 귀무가설(평균2.6) 채택


# 3. "lifespan.csv"는 개와 고양이의 수명을 표본추출한 결과이다. 개와 고양이의 수명에 차이가 있는지, 주어진 데이터의 평균의 차이를 이용해 검정하라.
life <- read.csv("lifespan.csv", stringsAsFactors=TRUE, encoding="UTF-8")
head(life)

# 3.1 데이터 구조를 확인
str(life)
# 3.2 두 그룹(dog, cat)의 값을 boxplot 시각화
boxplot(life[,2:3])
# 3.3 정규성 검정
shapiro.test(life$dog) #귀무가설(정규성만족) 기각 -> wilcox, kruskal
shapiro.test(life$cat) #귀무가설(정규성만족) 채택

# 등분산성
var.test(life$dog, life$cat) #귀무가설(등분산성만족) 채택

# 3.4 3.3의 결과에 따라 모수/비모수적 평균차이검정을 시행

life.w <- wilcox.test(life$dog, life$cat)
life.k <- kruskal.test(life$dog, life$cat)
#life.t <- t.test(life$dog, life$cat)

# 3.5 결론을 print()함수로 출력
print(life.t) # 귀무가설(두집단유의하지않다) 채택

dog<- life %>% 
  specify(response = dog) %>% 
  hypothesize(null="point", mu=13)

shapiro.test(dog$dog)





# 4. "Ch0802.PST.csv"는 K제약사에서 개발한 다이어트 약의 효능을 검증하기 위해, 실험자들의 복용전 몸무게와 3개월후 몸무게를 측정한 데이터이다. 새로운 약이 다이어트에 효과가 있는지 검정하라.(유의수준 0.05)
pill <- read.csv("Ch0802.PST.csv", stringsAsFactors=TRUE, encoding="UTF-8")
head(pill)

# 4.1 데이터를 불러와서 구조를 확인한다.
str(pill)

# 4.2 몸무게의 차이를 나타내는 dif변수를 생성하고, 복용전몸무게pre, 복용후 몸무게post에 대한 히스토그램과 몸무게차이dif의 boxplot을 표현
install.packages("dplyr")
library(dplyr)
pill <- mutate(pill, dif=(pill$pre-pill$post))
hist(pill$post)
boxplot(pill$dif)

# 4.3 정규성 가정에 대한 판단
shapiro.test(pill$pre)
shapiro.test(pill$post) #정규성만족X -> wilcox /w pair

# 4.4 4.3 결과에 따라 모수/비모수적 방법을 선택하여 다이어트약 복용 전과 후에 차이가 있는지 검정을 실시
#t.test(pill$pre, pill$post)
help(wilcox.test)

pill.w <- wilcox.test(pill$pre, pill$post, alternative="less", paired=TRUE)
#pill.w2 <- wilcox.test(pill$dif~1)
#pill.w3 <- wilcox.test(Pair(pill$pre, pill$post)~1)
pill.w
# tie뜨는거 wilcox.exact 함수로 처리됨 (exactRankTest 필요)

# 4.5 결과를 print()함수로 출력
print(pill.w) #효과가 없는듯


# 5. "사교육시간.csv"데이터에서 variable에는 소속 고등학교(A,B,C)가, value에는 학생의 사교육 시간이 표시되어 있다. A,B,C 고등학교 학생들의 사교육 시간은 고등학교 마다 차이가 있는지 검정하라.
private <- read.csv("사교육시간.csv", stringsAsFactors=TRUE, encoding="UTF-8")
head(private)
str(private)
unique(private$variable)

# 5.1 데이터를 불러와서 고등학교별로 boxplot 시각화 하라.
help(boxplot)
boxplot(value~variable, data=private)

# 5.2 분산분석을 위한 가정이 성립하는지 확인(독립성은 만족한다고 가정, 정규성, 등분산성을 확인하라.)
help(var.test)
#var.test(value~variable, data=private) # 2그룹할때
bartlett.test(value~variable, data=private) # 등분산성 만족

# 5.3 5.2의 결과에 따라 모수/비모수적 차이검정을 진행
private.aov <- aov(value~variable, data=private)
summary(private.aov)

# 5.4 필요시 사후검정을 실행한다. -> 그룹별 어떻게 다른가?
install.packages("dunn.test")
library(dunn.test)
dunn.test(private$value, private$variable) 
#A,B간, B,C간에는 유의미한 차이 있다. A,C는 차이 없다

# 5.5 결과를 print()함수로 출력한다.
print(summary(private.aov)) # 차이 존재

str(private)

# 6. mtcars dataset은 32개 차종에 대한 특성과 단위연료당 주행거리를 담고 있다. 변속기종류am, 실린더개수 cyl 등. 데이터를 전처리 하여, am와 cyl에 따라 주행거리mpg평균에 유의미한 차이가 존재하는기 검정하고, cyl과 am변수 사이에 상호작용 효과가 있는지 상호작용 그래프를 이용해 해석하라.

# 6.1 데이터 확인data("mtcars")하여, 필요한 전처리(팩터화)를 하고, 필요변수로만 구성된 car데이터 프레임을 생성 후, 구조 확인.
str(mtcars)
mtcars.pre <-  mtcars[,c("cyl", "am", "mpg")]
mtcars.pre$am <- as.factor(mtcars.pre$am)
mtcars.pre$cyl <- as.factor(mtcars.pre$cyl)
str(mtcars.pre)

# 6.2 분산분석 가정의 검정
#bartlett.test(mpg~am*cyl,data=mtcars.pre)
bartlett.test(mpg~interaction(am,cyl),data=mtcars.pre) #만족

# 6.3 Two-way ANOVA수행
mtcars.aov <- aov(mpg~am*cyl,data=mtcars.pre) #am+cyl+am:cyl
summary(mtcars.aov) # am,cyl 변수 둘다 유의함

# 6.4 주효과와 상호작용에 대한 검정결과를 print()함수로 출력하라.
interaction.plot(response=mtcars.pre$mpg,mtcars.pre$am, mtcars.pre$cyl)
# 

# 7. 런검정(우연성검정)
# 7.1 한 공장의 야간 조립라인에서 미숙련공들이 생산하는 제품들에 문제가 있는지 살펴보기 위해 최근 생산된 27개의 제품을 순서대로 검사하여 합격품은 S/ 불합격품은 F로 표시하였다. x1.csv파일을 불러들여 유의수준 1%로 우연성을 검정하라.
x1 <- read.csv("x1.csv", stringsAsFactors=TRUE, encoding="UTF-8")
head(x1)
str(x1)
x1$x <- as.factor(x1$x)
install.packages("tseries")
library(tseries)
runs.test(x1$x)

# 7.2 250 mL 음료수 캔에 적정량의 음료수를 채우는 기계에 변화가 일어나는가를 알아보기 위하여 최근 연속적으로 생산한 40개 캔의 용량을 측정하였다. x2.csv를 불러들여 유의수준 1%를 사용하여 생산기계에 너무 자주 변화가 일어난다는 대립가설과 우연성의 귀무가설을 검정하여라. 
x2 <- read.csv("x2.csv", stringsAsFactors=TRUE, encoding="UTF-8")
head(x2)
str(x2)


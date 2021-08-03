.libPaths("D:/R/R-4.0.3/library")
library(reshape); library(ggplot2); library(tidyverse); library(lawstat); library(dunn.test)
##### 전처리 주제 #####
# 통계검정: 그룹간 평균검정, 모수/비모수 
##### 전처리 문제 #####
## 구분 ##### 모수 통계 #### 비모수 통계####
########################################################
#####구분### 모수 통계 ###### 비모수 통계 ################
# 단일표본## 단일표본t검정  # 부호검정,윌콕슨부호순위검정 #
# 두표본  ## 독립표본t검정  # 윌콕슨순위합테스트          #  
#         ## 대응표본t검정  # 부호검정,윌콕슨부호순위검정 #
# 분산분석## ANOVA          # 크루스칼-왈리스검정         #
# 무작위성##                # 런검정                      #   
# 상관분석## 피어슨상관계수 # 스피어만순위상관계수        #
###########################################################
#### one sample t test ####
# 1. "Ch0701.OST.csv"는 한 아이스크림 가게에서 판매한 파인트의 g수를 측정한 데이터이다. 표본조사결과 아이스크림무게가 본사에서 공지한 파인트의 무게 320g와 차이가 나는지 검정하라. 
 # 1.1 데이터를 불러와서 구조를 확인하고, 소수점 2자리에서 반올림하고, boxplot, 히스토그램으로 시각화 하라. 
ost <- read.csv("Ch0701.OST.csv", header = TRUE)
ost <- round(ost, 2) #소수점2자리 반올림림
boxplot(ost$weight)
hist(ost$weight, breaks = 10, col = "red",
     xlab = "무게", ylab = "개수",
     ylim = c(0,25), main = "아이스크림 무게에 대한 히스토그램(정규분포?)")
 # 1.2 평균 아이스크림 무게가 320g과 같은지 검정하고 그 결과를 print함수로 출력하라.(양측검정, 유의수준 0.05)
t.test(ost, alternative = "two.sided",
       mu = 320.0, conf.level = 0.95)
print("아이스크림 가게에서 파는 파인트이 무게는 320g과 같지 않다")
#### wilcoxon signed rank test ####
# 2. MASS패키지의 cats data는 고양이의 성별Sex, 몸무게Bwt, 심장무게Hwt를 담고 있다. 고양이들의 평균몸무게가 2.6kg인지 아닌지에 대한 통계적 검정을 수행하라.(양측검정, 유의수준 = 0.05)
 # 2.1 데이터 불러와서 구조 확인 
library(MASS) # data(cats)사용
str(cats)     # 구조확인, 144obs, 3vars
 # 2.2 Bwt(고양이 몸무게) 변수에 대한 정규성 검정 수행
shapiro.test(cats$Bwt) # p-value = 6.731e-05, 귀무기각(정규분포아님)
 # 2.3 평균 고양이 몸무게가 2.6kg과 같은지 검정
wilcox.test(cats$Bwt, mu=2.6, alternative = "two.sided") # p-value = 0.02532, 귀무기각
 # 2.4 결론을 print()함수로 출력하라.
print("고양이 평균 몸무게(Bwt평균)는 2.6kg이 아니다.")

#### independent t test ? Wilcoxon rank sum test?####
# 3. "lifespan.csv"는 개와 고양이의 수명을 표본추출한 결과이다. 개와 고양이의 수명에 차이가 있는지, 주어진 데이터의 평균의 차이를 이용해 검정하라.
  # 3.1 데이터 구조를 확인
lifespan <- read.csv("lifespan.csv", header = TRUE)
str(lifespan)
  # 3.2 두 그룹(dog, cat)의 값을 boxplot 시각화
boxplot(lifespan$dog,lifespan$cat,names=c("dog","cat"),ylim=c(0,20),cex.axis=2)
    # 3.3 기술통계
    # dog_DS <- paste0("dog :",round(mean(dog),1),"±",round(sd(dog),1),"(",min(dog),",",max(dog),")")
    # cat_DS <- paste0("cat :",round(mean(cat),1),"±",round(sd(cat),1),"(",min(cat),",",max(cat),")")
    # dog_DS #"dog :13.4±1.2(12.1,15.7)"
    # cat_DS #"cat :17.3±1.5(15.2,19.5)"
  # 3.4 (표본크기 10~30이므로)정규성 검정
    # Lilliefors test
library(nortest)
lillie.test(dog)$p.value #[1] 0.008957666(귀무기각, 정규분포따르지 않음)
lillie.test(cat)$p.value #[1] 0.1555209
    # shapiro wilk test
shapiro.test(dog)$p.value #[1] 0.03101055(귀무기각, 정규분포따르지 않음)
shapiro.test(cat)$p.value #[1] 0.1959406
  # 3.5 (dog가 정규분포를 따르지 않으므로)윌콕슨 순위합 검정 진행
wilcox.test(dog,cat,alternative = "less") # p-value = 2.762e-06
  # 3.6 결과를 print()로 출력하라.
print("고양이 수명이 개의 수명보다 유의미하게 크다")
    # Wilcoxon rank sum test with continuity correction
    # data:  dog and cat
    # W = 2.5, p-value = 2.762e-06
    # alternative hypothesis: true location shift is less than 0  
#### paired t test ####
# 4. "Ch0802.PST.csv"는 K제약사에서 개발한 다이어트 약의 효능을 검증하기 위해, 실험자들의 복용전 몸무게와 3개월후 몸무게를 측정한 데이터이다. 새로운 약이 다이어트에 효과가 있는지 검정하라.(유의수준 0.05)
  #  4.1 데이터를 불러와서 구조를 확인한다.
pst <- read.csv("Ch0802.PST.csv", header = TRUE)
str(pst)
 # 4.2 몸무게의 차이를 나타내는 dif변수를 생성하고, 복용전몸무게pre, 복용후 몸무게post에 대한 히스토그램과 몸무게차이dif의 boxplot을 표현
dif <- c(pst$post-pst$pre) #몸무게 차이
hist(pst$pre, main="복용 전 몸무게") 
hist(pst$post, main="복용 후 몸무게") 
boxplot(dif, main="몸무게 차이")
 # 4.3 정규성 가정에 대한 검사
shapiro.test(dif) #p-value = 0.8834(정규분포를 따른다.)
 # 4.4 정규성 가정을 만족하는지 여부에 따라 모수/비모수적 방법을 선택하여 다이어트약 복용 전과 후에 차이가 있는지 검정을 실시
t.test(pst$post, pst$pre,
       alternative = c("two.sided"),
       paired = TRUE,
       conf.level = 0.95) # p-value = 0.00176
t.test(dif, mu = 0) # p-value = 0.00176
  # 4.5 결과를 print()함수로 출력
print()
#### 분산분석? Kruskal-Wallis test? ####
# 5. "사교육시간.csv"데이터에서 variable에는 소속 고등학교(A,B,C)가, value에는 학생의 사교육 시간이 표시되어 있다. A,B,C 고등학교 학생들의 사교육 시간은 고등학교 마다 차이가 있는지 검정하라.
 # 5.1 데이터를 불러와서 고등학교별로 boxplot 시각화 하라.
data <- read.csv("사교육시간.csv", header = TRUE)
ggplot(data = data, aes(x = variable, y = value, color = variable)) +
  geom_boxplot() + 
  geom_point(alpha = 0.5)
 # 5.2 분산분석을 위한 가정이 성립하는지 확인한다.
 #독립성 Independence :  연구의 목적과 데이터 추출 방법, 추가적으로 연구를 통해서 만족한다고 가정
 #정규성 Normality : 잔차에 대해 정규성 검정해야함
model_anova <-  aov(value ~ variable, data = data)
residuals <- model_anova$residuals
shapiro.test(residuals) # p-value = 0.006657(귀무기각,정규분포아님) 
hist(residuals)         # 따라서, 모수적 방법 불가.(=비모수)
 #등분산성 Same variance : 그룹간 분산은 동일해야 함 (=등분산으로 나옴)
levene.test(data$value, data$variable)
levene.test(data$value, data$variable, location = "mean")
bartlett.test(value~variable, data=data) # 등분산성 만족
  # 5.3 가정 성립여부에 따라, 모수/비모수적 분석을 결정하여 검정한다.
  # -> 독립성o , 정규성x, 등분산성o
  # ANOVA(x)/ welch's ANOVA(x)/ Kruskal-Wallis test(o,가능)
model_fin <- kruskal.test(value ~ variable, data = data)
model_fin # p-value = 4.473e-08(중앙값은 적어도 하나는 다르다)
  # 5.4 필요시 사후검정을 실행한다.
  # 그룹의 중앙값이 같은지 검정 -> 다르다면, 사후검정
install.packages("dunn.test")
library(dunn.test)
dunn.test(data$value, data$variable) 
  # 5.5 결과를 print()함수로 출력한다.
print("A,B간, B,C간에는 유의미한 차이 있다. A,C는 차이 없다.") 
#### two way ANOVA ####
# 6. mtcars dataset은 32개 차종에 대한 특성과 단위연료당 주행거리를 담고 있다. 변속기종류am, 실린더개수 cyl 등. 데이터를 전처리 하여, am와 cyl에 따라 주행거리mpg평균에 유의미한 차이가 존재하는기 검정하고, cyl과 am변수 사이에 상호작용 효과가 있는지 상호작용 그래프를 이용해 해석하라.
# 1. 가설설정
#1.1 주효과 검정에 대한 가설
#   H0: cyl(팩터)에 따른 mpg 평균 차이는 없다.
#   H1: Not H0
#   H0: am(팩터)에 따른 mpg 평균 차이는 없다.
#   H1: Not H0
#1.2 상호작용효과 검정에 대한 가설
#   H0: am, cyl 간에는 상호작용이 없다.
#   H1: Not H0

 # 6.1 # 2. 데이터 확인 및 전처리(팩터화)
data("mtcars")
str(mtcars) # 32obs. 11vars
# cyl, am 독립변수들의 팩터화, 필요 변수들로만 구성된 df생성
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

car <- mtcars[,c("cyl", "am", "mpg")]
str(car)
  # 6.2 분산분석 가정의 검정
ㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐ
ㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐㅐ
  # 6.3 Two-way ANOVA수행
car_aov <- aov(mpg ~ cyl*am, data = car)
summary(car_aov) #통계량 확인
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# cyl          2  824.8   412.4  44.852 3.73e-09 ***
# am           1   36.8    36.8   3.999   0.0561 .  
# cyl:am       2   25.4    12.7   1.383   0.2686    
# Residuals   26  239.1     9.2     
  # 6.4 주효과와 상호작용에 대한 검정결과를 print()함수로 출력하라
# 주효과검정1(mpg ~ cyl): 귀무기각, "실린더 개수에 따라 주행거리간 유의미한 차이가 존재"
# 주효과검정2(mpg ~ am): 귀무채택, "변속기 종류에 따른 주행거리 평균간 차이는 존재하지 않는다."
# 상호작용효과검정: 0.2686으로 0.05보다 크다(귀무채택, "cyl:am의 상호작용은 없다."
interaction.plot(car$cyl, car$am, car$mpg, col=c("red","blue"))
#그래프가 서로 교차하지 않으므로, 상호작용이 없다.

#### Run test ####
# 7. 런검정(우연성검정)
  # 7.1 범주형 변수의 우연성 검정 
# 한 공장의 야간 조립라인에서 미숙련공들이 생산하는 제품들에 문제가 있는지 살펴보기 위해 최근 생산된 27개의 제품을 순서대로 검사하여 합격품은 S/ 불합격품은 F로 표시하였다. 유의수준 1%로 우연성을 검정하라.
  x1 <- as.factor(c("S","S","S","S","S","F","F","F","F","S","S","S","S","S","S","S","S","S","S","F","F","S","S","F","F","F","F"))
tseries::runs.test(x1)
  # 7.2 연속형 변수의 우연성검정
# 250 mL 음료수 캔에 적정량의 음료수를 채우는 기계에 변화가 일어나는가를 알아보기 위하여 최근 연속적ㅇ르ㅗ 생산한 40개 캔의 용량을 측정하였다. 유의수준 1%를 사용하여 생산기계에 너무 자주 변화가 일어난다는 대립가설과 우연성의 귀무가설을 검정하여라.
  #우연성검정 실시하기
  x2 <- c(261, 258, 249, 251, 256, 250, 247, 255, 243, 252, 250, 253, 247, 251, 243, 258, 251, 245, 250, 248, 252, 254, 250, 247, 253, 251, 246, 249, 252, 247, 250, 253, 247, 249, 253, 246, 251, 249, 253)
snpar::runs.test(x2, TRUE)

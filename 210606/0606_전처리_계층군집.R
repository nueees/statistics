# 1. 계층적 군집분석 - 신입사원의 면접시험 결과를 군집분석한다.
# 1-1. interview.csv 파일을 불러오고 데이터 구조를 파악한다.
interview <- read.csv("interview.csv", fileEncoding = "CP949", encoding = "UTF-8")
names(interview)
head(interview)

# 1-2. "no", "종합점수", "합격여부" 컬럼을 제외하고 면접시험 평가요소인 6개 컬럼만으로 interview_df로 다시 저장하고, 유클리디안 거리를 계산한다. (dist 함수 사용)
interview_df <- interview[c(2:7)]
head(interview_df)
# dplyr 활용한 변수 선택
# interview_df <- dplyr::select(interview, -c("no", "종합점수", "합격여부"))
# names(interview_df)

idist <- dist(interview_df)
idist

# 1-3. 계층적 군집 분석을 수행하고 plot으로 군집 분석 결과를 확인한다.
hc <- hclust(idist)
hc

plot(hc, hang = -1)     # hang = -1 : 음수 값 제외

# 1-4. 군집의 개수를 3개로 하여 subset을 생성하고 각 subset의 합격여부를 파악한다.
rect.hclust(hc, k = 3, border ="red")  # k개의 군집 단위 테두리 생성

g1 <- subset(interview, no == 108 | no == 110 | no == 107 |
               no == 112 | no == 115)
g2 <- subset(interview, no == 102 | no == 101 | no == 104 |
               no == 106 | no == 113)
g3 <- subset(interview, no == 105 | no == 114 | no == 109 |
               no == 103 | no == 111)
g1
g2
g3

# 1-5. 요약통계량(summary)를 통해 각 subset의 종합점수 평균 등의 군집별 특징을 확인한다.
summary(g1)
summary(g2)
summary(g3)


####### cutree: 군집 수 자르기 ###############
# 단계 1: 유클리디안 거리 계산
rm(list = ls())
idist <- dist(iris[1:4])
hc <- hclust(idist)
plot(hc, hang = -1)

# 단계 2: 군집 수 자르기
ghc <- cutree(hc, k = 3)
ghc

# 단계 3: iris 데이터 셋에 ghc 컬럼 추가 
iris$ghc <- ghc
table(iris$ghc)
head(iris)

# 단계 4: 요약 통계량 구하기 
g1 <- subset(iris, ghc == 1)
summary(g1[1:4])
g2 <- subset(iris, ghc == 2)
summary(g2[1:4])
g3 <- subset(iris, ghc == 3)
summary(g3[1:4])

# datasets::USArrests 데이터를 로딩하시오. 
# 각 변수간의 산점도를 그리시오.
# 각 변수간의 다중공선성 확인하시오. 
# 상관행렬을 이용 주성분 분석을 수행/해석하고 주요 2변수를 그래프(biplot)로 도식화하시오.
# 동일한 작업을 표준화(centering, scaling)하고 수행해 보시오. (receipe 이용)

rm(list=ls())
# * PCA
library(dplyr); library(magrittr); library(ggplot2);

# datasets::USArrests 데이터 로딩 
data("USArrests", package = "datasets") 

USArrests %>%  str()
USArrests %>%  colnames()
USArrests %>%  head()

# 각 변수간의 산점도를 그리시오. 
USArrests %>% pairs()
USArrests %>% pairs(., panel = panel.smooth )

library(corrplot)
USArrests %>% cor()
USArrests %>% cor() %>%  corrplot::corrplot()

par(mfrow = c(1,1))

USArrests$index <- 1:NROW(USArrests)

library(car)
lm(index ~ Murder + Assault + UrbanPop + Rape, data =  USArrests) %>%  vif()
# vif 5 이상의 값이 없으므로 다중 공선성 없다. 

# 상관행렬을 이용 주성분 분석을 수행/해석하고 주요 2변수를 그래프(biplot)로 도식화하시오.  
USArrests[, -5] %>%  princomp(., cor=TRUE) -> pca1
pca1 
pca1 %>% summary()
pca1 %>%  biplot()
pca1 %>%  plot()

library(FactoMineR) # 빅분기 패키지 아님. 
USArrests[, -5] %>%  PCA(.) -> pca2
pca2
pca2$var$coord
pca2 %>%  summary()
pca2 %>% 


# 동일한 작업을 표준화(centering, scaling)하고 수행해 보시오. (receipe 이용)
library(recipes)
recipe(index ~ . , data = USArrests) %>%  
  step_center(all_numeric(), -index) %>%  
  step_scale(all_numeric(), -index) %>% 
  # step_pca(all_numeric(), -index) %>%  
  prep(., training = USArrests) %>% 
  bake(., new_data = USArrests) -> USArrests2


USArrests2[, -5] %>%  princomp(., cor=TRUE) -> pca3
pca3 
pca3 %>% summary()
pca3 %>%  biplot()


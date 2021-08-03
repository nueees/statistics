#installed.packages()[,c(3,4)]

library(caret)
library(dplyr)
#library(purrr)
library(recipes)
#library(stringr)
#library(tidyr)

rm(list=ls())

#### 1. 불러오기 ####
x_train <- read.csv("./STUDY_R/DataSet2/X_train.csv") %>% as_tibble()
y_train <- read.csv("./STUDY_R/DataSet2/y_train.csv") %>% as_tibble()
x_test <- read.csv("./STUDY_R/DataSet2/x_test.csv") %>% as_tibble()

#### 2. 데이터 확인 #### 
x_train %>% head()
y_train %>% head()
x_test %>% head()

x_train %>% str()
y_train %>% str()
x_test %>% str()

#### 3. 전처리 및 데이터 확인 ####
# train data x,y left_join
f_trn_tbl <- y_train %>% left_join(x_train, by="cust_id")

## train set과 평가용 test set 합치기(for 전체data로 전처리)
f_tbl <- bind_rows(trn=f_trn_tbl, tst=x_test, .id="dataset")

## y 범주화
f_tbl$gender <- factor(f_tbl$gender, levels=c(0,1), labels=c("여자", "남자"))
f_tbl %>% str()
f_tbl %>% summary()

## 환불금액 NA를 0으로
f_tbl$환불금액 <- ifelse(is.na(f_tbl$환불금액), 0, f_tbl$환불금액)
f_tbl
f_tbl %>% summary()


## 로그정규화, 표준화... 음...
recp <- recipe(gender~., data=f_tbl) %>% 
  step_center(all_numeric(), -cust_id) %>% 
  step_scale(all_numeric(), -cust_id) %>% 
  step_BoxCox(all_numeric(), -cust_id) %>% 
  step_zv(all_numeric(), -cust_id) %>%
  step_other(all_nominal(), -gender, -dataset, threshold=0.1) %>% 
  step_dummy(all_nominal(), -gender, -dataset, one_hot=TRUE) %>%
  prep()

recp
f_tbl_r <- recp %>% juice()
f_tbl_r
f_tbl_r %>% summary()

## 전처리 끝났으니까 다시  train/test dataset 분리
trn_tbl <- f_tbl_r %>% filter(dataset=="trn") %>% select(-dataset)
tst_tbl <- f_tbl_r %>% filter(dataset=="tst") %>% select(-dataset, -gender)
trn_tbl %>% dim()
tst_tbl %>% dim()

#### 5. 모델링 ####
fit_ctl <- trainControl(method="repeatedcv", 3, 3,
                        #sampling="up",
                        classProbs = TRUE,
                        summaryFunction=twoClassSummary)

# error
# glm
# set.seed(2021)
# glm_fit <- train(gender~.,
#                    data=trn_tbl,
#                    method="glm",
#                    #preProc=c("center", "scale"),
#                    trControl=fit_ctl,
#                    metric="ROC")
# 50건 이상의 경고들을 발견되었습니다 (이들 중 처음 50건을 확인하기 위해서는 warnings()를 이용하시길 바랍니다).
# >   warnings()
# 경고 메시지:
#   1: In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  ... :

# glm_fit
# 
# glm_pred <- predict(glm_fit, tst_tbl, type="prob")
# glm_pred %>% head()

# rpart
set.seed(2021)  
rpart_fit <- train(gender~., 
                   data=trn_tbl, 
                   method="rpart", 
                   #preProc=c("center", "scale"), 
                   trControl=fit_ctl, 
                   metric="ROC")

rpart_fit

rpart_pred <- predict(rpart_fit, tst_tbl)
rpart_pred %>% summary()
rpart_pred <- predict(rpart_fit, tst_tbl, type="prob")
rpart_pred %>% head()

# randomforest
set.seed(2021)
rf_fit <- train(gender~.,
                data=trn_tbl,
                method="rf",
                #preProc=c("center", "scale"),
                trControl=fit_ctl,
                metric="ROC")

rf_fit

rf_pred <- predict(rf_fit, tst_tbl)
rf_pred %>% summary()
rf_pred <- predict(rf_fit, tst_tbl, type="prob")
rf_pred %>% head()

# knn
set.seed(2021)
knn_fit <- train(gender~.,
                 data=trn_tbl,
                 method="knn",
                 #preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = fit_ctl,
                 metric="ROC"
)
knn_fit

knn_pred <- predict(knn_fit, tst_tbl)
knn_pred %>% summary()
knn_pred <- predict(knn_fit, tst_tbl, type="prob")  
knn_pred %>% head()

bind_cols(X_test, rf_pred ) %>%  
  select(cust_id, 남성) %>%  
  rename(gender = "남성") %>%  
  write.csv(., "서은별.csv", row.names = FALSE)

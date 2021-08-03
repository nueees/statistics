library(dplyr); library(caret); library(recipes); library(magrittr); 
rm(list=ls())
getwd()
setwd("C:/Users/Administrator/Documents/R/BDA/210613")

library(data.table)


left_join(
  read.csv("X_train.csv") ->  X_train, 
  read.csv("y_train.csv") ->  y_train ) %>%  
  mutate(index = "train") -> train
str(X_train)
str(y_train)
str(train)

read.csv("X_test.csv")  %>%  
  mutate(index = "test") ->  X_test
str(X_test)

bind_rows(train, X_test) -> full 

full$gender   <-  ifelse(full$gender   == "1", "남성", "여성")
full$gender %<>%  as.factor()
full$index  %<>%  as.factor()
full$환불금액 <-  ifelse(full$환불금액  %>% is.na(), 0, full$환불금액 )


full %>%  str()
# full %>%  is.na() %>%  sum() 
# full %>%  summary()

recipe(gender ~ ., data = full) %>%
  # step_rm(index,cust_id ) %>% 
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%  
  step_BoxCox(all_numeric()) %>%  
  step_zv(all_numeric()) %>%
  step_dummy(all_nominal(), -gender, -index,  one_hot = TRUE) %>%
  prep() %>%  
  juice() -> full2

full2 %>%  filter(index == "train") -> train2
full2 %>%  filter(index == "test")  -> test2

full2 %>%  select(-gender) %>%  is.na() %>%  sum()


# #### rf
library(randomForest)

ctrl <- trainControl(
  # method = "none",
  method = "repeatedcv",
  # repeats = 3,    # (10 fold, repeated 3 times)
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  # sampling = "down",
  # p = 0.8
  # sampling = "rose"
  # sampling = "smote"
)

set.seed(123)
rfFit <- train(
  gender ~ .,
  data = train2,
  method = "rf",
  # preProc = c("center", "scale"),
  tuneLength = 3,
  trControl = ctrl,
  metric = "ROC"
)
rfFit

#### rpart
library(rpart)
# Cross Validation & ROC Metric 을 위한 조치 필요 
ctrl <- trainControl(
  method = "repeatedcv", 
  # repeats = 3,    # (10 fold, repeated 3 times) 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary,  
  sampling = "up"
  # sampling = "rose" 
  # sampling = "smote"
)

set.seed(123)
rpartFit <- train(
  gender ~ .,
  data = train2,
  method = "rpart",
  # preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl,
  metric = "ROC"
)
rpartFit 
#                 cp 0.003419453일 때, 0.6377514
# sampling="up"   cp 0.005319149일 때, 0.6376410
# sampling="down" cp 0.005319149일 때, 0.6341834
rpartFit %>%  plot()
#### glm
library(rpart)

set.seed(123)
rpartFit <- train(
  gender ~ .,
  data = train2,
  method = "rpart",
  # preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl,
  metric = "ROC"
)
rpartFit 

# test2 %>%  head()

predict(rpartFit, test2, type = "prob") -> pred_rpart

# rpartFit %>%   predict(., test2, type = "raw") -> pred_rpart_raw
# rfFit %>%   predict(., test2, type = "raw")

test.0<- bind_cols(X_test, pred_rpart ) %>%  
  select(cust_id, 남성) %>%  
  rename(gender = "남성") 
test.0$gender <-ifelse(test.0$gender>0.5, 1, 0)
test.0 %>% write.csv("y_test.csv") #회귀분석용 csv

bind_cols(X_test, pred_rpart ) %>%  
  select(cust_id, 남성) %>%  
  rename(gender = "남성") %>%  
  write.csv(., "2017026.csv", row.names = FALSE)


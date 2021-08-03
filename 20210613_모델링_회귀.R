options(scipen=100) # 근데.이게 안 먹는 거 같은데.. ㅠㅠㅠ
library(dplyr); library(caret); library(recipes); library(magrittr); 
rm(list=ls())
getwd()
setwd("C:/Users/Administrator/Documents/R/BDA/210613")


######################################################################
left_join(
  read.csv("X_train.csv") ->  X_train, 
  read.csv("y_train.csv") ->  y_train ) %>%  
  mutate(index = "train") -> train 
left_join(
  read.csv("X_test.csv") -> X_test,
  read.csv("y_test.csv") -> y_test ) %>%
  mutate(index = "test") ->  test
test <- test[,-11]
#bind_rows(train, X_test) -> full 
bind_rows(train, test) -> full
str(train)
str(test)
str(full)

full$gender   <-  ifelse(full$gender   == "1", "남성", "여성")
full$gender %<>%  as.factor()
full$index  %<>%  as.factor()

full_bak <- full

############################# response 총구매액 #######################


full$환불금액 <-  ifelse(full$환불금액  %>% is.na(), 0, full$환불금액 )
full$환불금액 %>% is.na() %>%  sum()

# 회귀로 바꾸기 위해서.. 총구매액을 예측해 보겠음. 
full %>% summary()
recipe(총구매액 ~ ., data = full) %>%
  step_center( all_numeric_predictors(), -cust_id) %>%
  step_scale( all_numeric_predictors(), -cust_id) %>%  
  step_YeoJohnson( all_numeric_predictors(), -cust_id) %>% 
  #step_dummy(all_nominal(), -gender, -index, one_hot = TRUE) %>% 회귀는 자동으로 해 줌 
  prep() %>%  
  juice() -> full2
full2 %>% summary()
full2 %>%  filter(index == "train") %>%  select(-index) -> train2
full2 %>%  filter(index == "test")  %>%  select(-index) -> test2

full2 %>%  is.na() %>%  sum()

# conrol은 공통으로 cross validation
ctrl <- trainControl( method = "cv" )

#####  lm 회귀
str(train2)
lm.1 <- train (총구매액~., data=train2, method="lm", trControl=ctrl, metric="RMSE")
lm.1 #RMSE 109980015
pred.lm.1 <- predict(lm.1, newdata=test2)

as.numeric(pred.lm.1) %>% head()
as.numeric(test2$총구매액) %>% head()
t1 <- bind_cols(as.numeric(pred.lm.1), test2$총구매액)
t1 %>% head(10)

RMSE(as.numeric(pred.lm.1), test2$총구매액) #119479596





############################# response 환불금액 #######################

full.r <-full_bak
#full.r$환불금액 <-  ifelse(full.r$환불금액  %>% is.na(), 0, full.r$환불금액 )
str(full.r$환불금액) # 총 5982건
full.r$환불금액 %>% is.na() %>%  sum() # NA 3906
plot(full.r$환불금액)
plot(full.r$총구매액)
plot(full.r$최대구매액)

# 회귀로 바꾸기 위해서.. 총구매액을 예측해 보겠음. 
full.r %>% summary()
recipe(환불금액 ~ ., data = full.r) %>%
  #step_modeimpute(gender) %>%  # gender는 최빈값으로 결측 보정 
  step_center( all_numeric_predictors(), -cust_id) %>%
  step_scale( all_numeric_predictors(), -cust_id) %>%  
  step_YeoJohnson( all_numeric_predictors(), -cust_id) %>% 
  # log/root < boxcox < 요한슨..
  # step_zv(all_numeric(), -cust_id) %>%
  #step_dummy(all_nominal(), -gender, -index, one_hot = TRUE) %>% 회귀는 자동으로 해 줌 
  prep() %>%  
  juice() -> full.r2
full.r2 %>% summary()
# full.r2 %>%  filter(index == "train") %>%  select(-index) -> train4
# full.r2 %>%  filter(index == "test")  %>%  select(-index) -> test4
full.r2 %>%  filter(!is.na(환불금액))  %>%  select(-index)-> train3
full.r2 %>%  filter(is.na(환불금액)) %>%  select(-index)->  test3

test3 %>%  is.na() %>%  sum()
sapply(full.r2, function(x) is.na(x)) %>% summary
# lm
ctrl <- trainControl(method="cv")
str(train3)
lm.2 <- train(환불금액~., data=train3, method="lm", trControl=ctrl, metric="RMSE")
lm.2 #RMSE 46223721 
pred.lm.2 <- predict(lm.2, newdata=test3)
summary(train3$환불금액)
summary(pred.lm.2)


str(train4)
lm.3 <- train(환불금액~., data=train4, method="lm", trControl=ctrl, metric="RMSE")
lm.3 #RMSE 25616506 
pred.lm.3 <- predict(lm.3, newdata=test4)
summary(train4$환불금액)
summary(pred.lm.3)
summary(test4$환불금액)
plot(train4$환불금액)
plot(pred.lm.3)
plot(test4$환불금액)
RMSE(pred.lm.3,test4$환불금액) #RMSE 34609323

summary(pred.lm.3)
ifelse(pred.lm.3<0,"-","+") 
df <- as.data.frame(pred.lm.3) 
str(df) #2482
df%>% filter(pred.lm.3<0) %>% count() #627
df%>% filter(pred.lm.3>=0) %>% count() #1855



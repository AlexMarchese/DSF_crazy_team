
library(ranger)
library(rsample)
library(dplyr)
library(tidyverse)
library(randomForest)
library(magrittr)
library(caret)


rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#The test was done for all three data
M = read.csv("../Data/clean_DS.csv")
M = read.csv("../Data/rearranged_DS.csv")
names(M)[5] = "visits"
M = read.csv("../Data/rearranged_extended_DS.csv")
names(M)[10] = "visits"

M = M %>%
  select(-X)



M = read.delim("../Data/dataset.csv",sep = ";")
names(M)[4] = "visits"

M$date = paste0(M$year,"0",M$month,M$day)
M$date = as.Date(M$date, tryFormats = "%Y%m%d")

M = M %>%
  select(-year,-month,-day)

hist(M$visits)

#Trying to exclude outlier
#M = M %>%
  ##  filter(visits>10) %>%
#  filter(visits< 38)


#Using the boxplot to exclude the extreme value (over 38)
ggplot()+geom_boxplot(aes(M$visits))


M_split <- initial_split(M, prop = .7)
M_train <- training(M_split)
M_test  <- testing(M_split)

#my first random forest

m1 <- ranger(
  formula = visits ~ .,
  data    = M_train,
  num.trees = 800,
  mtry = NULL,
  importance = "impurity",
  min.node.size = 5,
  sample.fraction = 0.8
)

"For the rearranged data with formula =count~. and 
For the clean_DS, formula = visits~. 
m1 <- ranger(
  formula = count ~ .,
  data    = M_train,
  num.trees = 800,
  mtry = NULL,
  min.node.size = 5,
  sample.fraction = 0.8
)
"

#Initial data/ RSME: 10.41 MAE: 6.78 Mean: 9.81
#Clean data/ RSME:7.28 MAE:3.64 Mean: 6.9
#For the rearranged data Mean: 51.15, MAE: 41.15 RSME 117.53

# WIth lower bound and upper bound both means are almost the same around 18
mean(M_train$visits)
mean(M_train$visits)

mean(m1$predictions)

m1$r.squared
#0.407 for initial
#0.6 for the rearranged
#0.2 for the clean data


# The folowing is to high is too high

caret::MAE(m1$predictions, M_train$visits)
caret::RMSE(m1$predictions, M_train$visits)

#manually is the same
mean(abs(M_train$visits - m1$predictions))


#Compare to the mean is extremely high in all cases
#Increase mtry increase the result but it id still very high
#But if mtry is too high the risk to have corrolated trees also increase

#With so bad results, optimisation of the forest cannot make miracles
#with a loop through node.size, mtry, sample.fraction
#My loop would go through this:

#with the clean Data I wouldn't take 10 mtry because it is like bagging
Matrixforloop <- expand.grid(
  mtry       = seq(2,10, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0 
)
for(i in 1:nrow(Matrixforloop)) {
  
  m12 <- ranger(
    formula         = visits ~ ., 
    data            = M_train, 
    num.trees       = 800,
    mtry            = Matrixforloop$mtry[i],
    min.node.size   = Matrixforloop$node_size[i],
    sample.fraction = Matrixforloop$sampe_size[i],
    seed            = 123
  )
  
  Matrixforloop$OOB_RMSE[i] <- sqrt(m12$prediction.error)
}

Matrixforloop %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)
#for the initial data
#mtry node_size sampe_size OOB_RMSE
#1    10         7      0.800 8.693120
#2    10         9      0.800 8.694490
#3    10         5      0.800 8.717609

OOB_RMSE <- vector(mode = "numeric", length = 100)

#take a while

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula         = Sale_Price ~ ., 
    data            = ames_train, 
    num.trees       = 800,
    mtry            = 10,
    min.node.size   = 7,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
  
  print(i)
}

hist(OOB_RMSE, breaks = 20)

#RMSE around 8, not much variance





#Testing make not much sense as the model is not suitable anyway
#But can be used for a good model

data_ctrl <- trainControl(method = "cv", number = 5)

model_caret <- train(visits~.,
                     tuneGrid= data.frame(mtry= 10,min.node.size = 7,splitrule = "variance"),
                     data = M_train,                        
                     trControl = data_ctrl,           
                     method = "ranger",                 
                     na.action = na.pass) 


model_caret$results[["MAE"]]
model_caret$results






"Cross validation but only with nummerical value (for example for our clean data)
library(spm)

M_trainx = M_train %>%
  select (-visits)

M_trainy = M_train %>%
  select(visits)
M_train 

rgcv(M_trainx,
    M_trainy,
    cv.fold = 5,
    num.trees       = 800,
    mtry            = 10,
    min.node.size   = 7,
    sample.fraction = .8)


M_trainy = as.vector(M_trainy)
class(M_trainy$visits)
M_trainy$visits = as.numeric(M_trainy$visits)
"

#Variables importance

tibble(x = m1$variable.importance,
       labels = names(m1$variable.importance)) %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(labels, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variables importance")












#Boosting trees
#to confirm the result of the RF

library(xgboost)     
library(vtreat)  

#First convert the data as numeric for initial data only
#Only for initial data

features <- setdiff(names(M_train), "visits")


treatplan <- vtreat::designTreatmentsZ(M_train, features, verbose = FALSE)

?designTreatmentsZ

?vtreat


new_vars <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%        
  dplyr::filter(code %in% c("clean", "lev")) %>% 
  magrittr::use_series(varName)     



features_train <- vtreat::prepare(treatplan, M_train, varRestriction = new_vars) %>% as.matrix()
response_train <- M_train$visits

features_test <- vtreat::prepare(treatplan, M_test, varRestriction = new_vars) %>% as.matrix()
response_test <- M_test$visits

#Implement a boosted tree


#Without the folowing it will give wonderful result (predict results with results...)
#Only with  our clean data
M_train = M_train %>%
  select(-visits)
response_train <- M_train$visits
response_test <- M_test$visits

M3 <- xgb.cv(
  data = as.matrix(M_train),
  label = response_train,
  nrounds = 1500,
  nfold = 5,
  objective = "reg:squarederror",  
  verbose = 0
)


M3clean <- xgboost(
  data = as.matrix(M_train),
  label = response_train,
  nrounds = 30,
  nfold = 4,
  objective = "reg:squarederror",  
  verbose = 0
)

which.min(M3$evaluation_log$train_rmse_mean)

M3clean$evaluation_log
M3$evaluation_log
?xgb.cv


pred <- predict(M3clean, as.matrix(M_test))

caret::RMSE(pred, response_test)
caret::MAE(pred, response_test)



#No tuning as it is clear that the based data is not suitable
#Results are comparable to the RF


#Corrolation


M = read.delim("../Data/dataset.csv",sep = ";")


M$date = paste0(M$year,"0",M$month,M$day)
M$date = as.Date(M$date, tryFormats = "%Y%m%d")


M = M %>%
  select(-year,-month,-day,-date,-page.2..clothing.model.)
"
M = M %>%
  filter(visits>10) %>%
  filter(visits< 38)
"

library(corrplot)
library(RColorBrewer)
C <-cor(M)
corrplot(C, type="upper", visits="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))



hist(M$visits)





#Prophet implementation (not suitable for only 4 month)

library(prophet)

#Converting day month year into date 
M = read.delim("../Data/dataset.csv",sep = ";")

M$date = paste0(M$year,"0",M$month,M$day)
M$date = as.Date(M$date, tryFormats = "%Y%m%d")


M = M %>%
  select(-year,-month,-day)


?prophet


M$ds = M$date
M$y = M$visits

M2= prophet(M)

#In order to have good predictions and take advantage of holidays effect
#The data has to be in more than 4 months


future = make_future_dataframe(M2, periods = 120) #greater periods is better

forecast <- predict(M2, future)

plot()







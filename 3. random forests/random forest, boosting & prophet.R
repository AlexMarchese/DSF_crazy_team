
library(ranger)
library(rsample)
library(dplyr)
library(tidyverse)
library(randomForest)
library(magrittr)
library(caret)

rm(list = ls())


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # you will need it later (helps us ensure that the code runs properly on all machines)


M = read.delim("../Data/dataset.csv",sep = ";")


M$date = paste0(M$year,"0",M$month,M$day)
M$date = as.Date(M$date, tryFormats = "%Y%m%d")


M = M %>%
  select(-year,-month,-day)

hist(M$order)

M = M %>%
  filter(order>10) %>%
  filter(order< 38)

#Less then 10 clicks is not relevant for our analysis
#Using the boxplot to exclude the extreme value (over 38)
ggplot()+geom_boxplot(aes(M$order))

M_split <- initial_split(M, prop = .7)
M_train <- training(M_split)
M_test  <- testing(M_split)

#my basic random forest

m1 <- ranger(
  formula = order ~ .,
  data    = M_train,
  num.trees = 800,
  mtry = 4,
  importance = "impurity",
  min.node.size = 5,
  sample.fraction = 0.8
)


#Both means are almost the same around 18

mean(M_train$order)

mean(m1$predictions)

m1$r.squared
?ranger


# The MSE is too high?

caret::MAE(m1$predictions, M_train$order)
caret::RMSE(m12$predictions, M_train$order)

#manually is the same
mean(abs(M_train$order - m1$predictions))


#Both gives 5.17 which compare to the mean is extremely high
#Increase mtry increase the result but it id still very high


#With so bad results it makes no sense to make an optimisation of the forest
#with a loop through node.size, mtry, sample.fraction
#My loop would go through this:

Matrixforloop <- expand.grid(
  mtry       = seq(2,10, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0 
)
for(i in 1:nrow(Matrixforloop)) {
  
  m12 <- ranger(
    formula         = order ~ ., 
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


OOB_RMSE <- vector(mode = "numeric", length = 100)

#take a while

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula         = order ~ ., 
    data            = M_train, 
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
#Testing part.

"

My questions are: 

It is common to have such result in the beginning of an model implementation?

Is something completely false in my implementation ? Data cleaning, ranger package?

If not, is it worth to tune my random forest and try to have better result?
Or the model is just not suitable... Should I continue anyway?

I am struggling to find a conclusion with my result...

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

library(xgboost)     
library(vtreat)  

#First convert the data as numeric

features <- setdiff(names(M_train), "order")


treatplan <- vtreat::designTreatmentsZ(M_train, features, verbose = FALSE)

?designTreatmentsZ

?vtreat


new_vars <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%        
  dplyr::filter(code %in% c("clean", "lev")) %>% 
  magrittr::use_series(varName)     



features_train <- vtreat::prepare(treatplan, M_train, varRestriction = new_vars) %>% as.matrix()
response_train <- M_train$order

features_test <- vtreat::prepare(treatplan, M_test, varRestriction = new_vars) %>% as.matrix()
response_test <- M_test$order

#Implement a boosted tree

M3 <- xgb.cv(
  data = features_train,
  label = response_train,
  nrounds = 2500,
  nfold = 5,
  objective = "reg:squarederror",  
  verbose = 0               
)

which.min(M3$evaluation_log$train_rmse_mean)

M3$evaluation_log







#Prophet implementation

library(prophet)

#Converting day month year into date 
M = read.delim("../Data/dataset.csv",sep = ";") #would be better to delete this -> you uploaded it already once in the script

M$date = paste0(M$year,"0",M$month,M$day)
M$date = as.Date(M$date, tryFormats = "%Y%m%d")


M = M %>%
  select(-year,-month,-day)


?prophet


M$ds = M$date
M$y = M$order

M2= prophet(M)

#As the time serie is made on daily basis, we have to change our goal to use it
#Meaning grouping the variables per day...

future = make_future_dataframe(M2, periods = 120)

forecast <- predict(M2, future)

plot()












myPaths <- .libPaths()

myPaths <- c(myPaths, "C://Program Files//R//win-library//4.1")

.libPaths(myPaths)

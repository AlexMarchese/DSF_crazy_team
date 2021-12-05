

# packages needed

library(tidyverse)
library(caret)
library(nnet) # for multinomial logistic regression

library(glue) # this is a python f string equivalent -> needed for nice output printing



# clean environment and set current wd -> helps to easier work with imported files
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# three types of dataset (the original and two variations to see what gives best results)

# 1. binary approach


DS_or <- read.delim("../Data/dataset.csv",sep = ";") # original dataset

med_1 <- median(DS_or$order)
DS_or <- DS_or %>% mutate(Label = ifelse(order >= med_1, 1, 0))
DS_or <-DS_or %>% select(-order)


DS_aggr <- read.csv("../Data/clean_DS.csv") # aggregated dataset

DS_aggr <- DS_aggr %>% select(-X) 
DS_aggr_dive_in <- DS_aggr # needed to see insights later
med_2 <- median(DS_aggr$visits)
DS_aggr <- DS_aggr %>% mutate(Label = ifelse(visits >= med_2, 1, 0))
DS_aggr <-DS_aggr %>% select(-visits)


DS_rearr <- read.csv("../Data/rearranged_DS.csv") # rearranged dataset

DS_rearr <- DS_rearr %>% select(-X)
DS_rearr_multiv <- DS_rearr # needed to do a multivariate classification later
med_3 <- median(DS_rearr$count)
DS_rearr <- DS_rearr %>% mutate(Label = ifelse(count >= med_3, 1, 0))
DS_rearr <- DS_rearr %>% select(-count)


# for each dataset: fitting the model, predicting & calculating the misclassification

# function to do it quickly
misclass_calc <- function(type_DS, dataset) {

  
  modelAll = glm(Label ~ ., family = "binomial", data = dataset)
  predict(modelAll, type = "response")
  
  D = dataset %>% 
    mutate(probAll = predict(modelAll, type = "response")) %>%
    mutate(predAll50 = ifelse(probAll >=0.5, 1, 0))
  
  misClas = mean(D$predAll50 != D$Label)
  
  falPos = 
    length(which(D$Label == 0 & D$predAll50 == 1))/
    length(which(D$Label == 0))
  
  falNeg = 
    length(which(D$Label == 1 & D$predAll50 == 0))/
    length(which(D$Label == 1))
  
  return(glue("Dataset: {type_DS} dataset, misclassification: {round(misClas, digits=3)}, FP: {round(falPos, digits=3)}, FN: {round(falNeg, digits=3)}"))
  
}

# running the function

misclass_calc("original", DS_or)
misclass_calc("aggregated ", DS_aggr)
misclass_calc("original", DS_rearr)

# N.B. the functions took quite some time to be run, therefore we are exporting the results at the very end

# The original and the rearranged dataset lead to very similar results (in terms of misclassificat, FP and TP).
# Quite interestingly, the aggregated dataset has a high level of FP and a low one of FN.
# This can be explained by the fact that the median in that dataset is 4 and the values of clicks per individual
# user session can be quite high in some cases. This can be seen below


med_2_dive_in <- median(DS_aggr_dive_in$visits)

hist(DS_aggr_dive_in$visits, xlab = "visits per customer", main = "frequency of visits per customer")

# taking out the outliers can be quite interesting, to see how the three metric change. Let us keep all values <= 15



DS_aggr_dive_in = DS_aggr_dive_in %>%
  filter(visits <= 15) 

DS_aggr_dive_in <- DS_aggr_dive_in %>% mutate(Label = ifelse(visits >= med_2_dive_in, 1, 0)) # n.b. I am not changing the median value
DS_aggr_dive_in <-DS_aggr_dive_in %>% select(-visits)

misclass_calc("aggregated without outliers ", DS_aggr_dive_in)

# As can be seen, this helped getting a better balance between FP and FP. However it made the model worse overall


# After having analyzed the three datasets with a binary logistic regression, it can be concluded that the 
# third dataset, which focuses on the variables price, category and colour, works best four our algorithm.


# Now that this was chosen, we can look into applying the CV for better comparison with other models, as 
# it allows us to estimate the testing error


str(DS_rearr)
DS_rearr$Label <- as.factor(DS_rearr$Label)


CV <- trainControl(method = "cv", number = 10)

model_caret <- train(Label ~ .,   
                     data = DS_rearr,                        
                     trControl = CV,           
                     method = "glm",                  
                     family = "binomial",
                     na.action = na.pass)


D = DS_rearr %>%
  mutate(probCV = predict(model_caret$finalModel, type = "response")) %>%
  mutate(predCV = ifelse(probCV >=0.5, 1, 0))

misClasCV = mean(D$predCV != D$Label)

falPosCV = 
  length(which(D$Label == 0 & D$predCV == 1))/
  length(which(D$Label == 0))


falNegCV = 
  length(which(D$Label == 1 & D$predCV == 0))/
  length(which(D$Label == 1))



# As a final step, we assess whether a multinomial logistic regression can lead to even better results



str(DS_rearr_multiv)

length(unique(DS_rearr_multiv$count))

DS_rearr_multiv$count <- as.factor(DS_rearr_multiv$count)


set.seed(20)

ind <- sample(2, nrow(DS_rearr_multiv), replace = T, prob = c(0.7, 0.4))

training <- DS_rearr_multiv[ind==1,]
testing <- DS_rearr_multiv[ind==2,]

model <- multinom(count ~., data = training)

prediction <- model %>% predict(testing)
head(prediction)

mean(prediction == testing$count)



















lapply(DS_or, class)



# logistic regression

# DS_conv = as_tibble(model.matrix(~ ., data = DS_cl)) %>% 
#   select(-"(Intercept)")

modelAll = glm(Label ~ ., family = "binomial", data = DS_or)
predict(modelAll, type = "response")

D = DS_or %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.5, 1, 0))


misClas_predAll50 = mean(D$predAll50 != D$Label)

falPos_predAll50 = 
  length(which(D$Label == 0 & D$predAll50 == 1))/
  length(which(D$Label == 0))


falNeg_predAll50 = 
  length(which(D$Label == 1 & D$predAll50 == 0))/
  length(which(D$Label == 1))




# dataset 2

DS <- read.csv("../Data/clean_DS.csv")

# orig <- read.delim("../Data/dataset.csv",sep = ";")
# 
# data_log <- orig[,c(5, 7, 9, 12)]




lapply(DS_cl, class)

summary(DS_cl)
str(DS_cl)
dim(DS_cl)

# logistic regression

# DS_conv = as_tibble(model.matrix(~ ., data = DS_cl)) %>% 
#   select(-"(Intercept)")

modelAll = glm(Label ~ ., family = "binomial", data = DS_cl)
predict(modelAll, type = "response")

D = DS_cl %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.5, 1, 0))


misClas_predAll50 = mean(D$predAll50 != D$Label)

falPos_predAll50 = 
  length(which(D$Label == 0 & D$predAll50 == 1))/
  length(which(D$Label == 0))


falNeg_predAll50 = 
  length(which(D$Label == 1 & D$predAll50 == 0))/
  length(which(D$Label == 1))


# model A price, category and country


modelA = glm(Label ~ c(price, country, category), family = "binomial", data = DS_cl)
predict(modelAll, type = "response")

D = DS_cl %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.5, 1, 0))



misClas_predAll50 = mean(D$predAll50 != D$Label)

falPos_predAll50 = 
  length(which(D$Label == 0 & D$predAll50 == 1))/
  length(which(D$Label == 0))


falNeg_predAll50 = 
  length(which(D$Label == 1 & D$predAll50 == 0))/
  length(which(D$Label == 1))




modelAll = glm(Label ~ ., family = "binomial", data = DS_cl)
predict(modelAll, type = "response")

D = DS_cl %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.5, 1, 0))

misClas_predAll50 = mean(D$predAll50 != D$Label)

falPos_predAll50 = 
  length(which(D$Label == 0 & D$predAll50 == 1))/
  length(which(D$Label == 0))


falNeg_predAll50 = 
  length(which(D$Label == 1 & D$predAll50 == 0))/
  length(which(D$Label == 1))







DS_rearranged <- read.csv("../Data/rearranged_DS.csv")



# lapply(DS_cl, class)
# 
# summary(DS_cl)
# str(DS_cl)
# dim(DS_cl)

# logistic regression

# DS_conv = as_tibble(model.matrix(~ ., data = DS_cl)) %>% 
#   select(-"(Intercept)")

modelAll = glm(Label ~ ., family = "binomial", data = DS_rearranged)
predict(modelAll, type = "response")

D = DS_rearranged %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.5, 1, 0))



misClas_predAll50 = mean(D$predAll50 != D$Label)

falPos_predAll50 = 
  length(which(D$Label == 0 & D$predAll50 == 1))/
  length(which(D$Label == 0))


falNeg_predAll50 = 
  length(which(D$Label == 1 & D$predAll50 == 0))/
  length(which(D$Label == 1))




# multinomial logistic regression



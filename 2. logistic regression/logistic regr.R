

# packages needed

library(tidyverse)
library(caret)

library(glue) # this is a python f string equivalent -> needed for nice output printing



# clean environment and set current wd -> helps to easier work with imported files
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# three types of dataset (the original and two variations to see what gives best results)


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


# DS_aggr_ext <- read.csv("../Data/rearranged_extended_DS.csv") # rearranged extended dataset
# 
# DS_aggr_ext <- DS_aggr_ext %>% select(-X) 
# med_4 <- median(DS_aggr_ext$count)
# DS_aggr_ext <- DS_aggr_ext %>% mutate(Label = ifelse(count >= med_4, 1, 0))
# DS_aggr_ext <-DS_aggr_ext %>% select(-count)

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
misclass_calc("rearranged", DS_rearr)
# misclass_calc("rearranged extended", DS_aggr_ext)



# The original and the rearranged dataset lead to very similar results (in terms of misclassificat, FP and TP).
# Quite interestingly, the aggregated dataset has a high level of FP and a low one of FN.
# This can be explained by the fact that the median in that dataset is 4 and the values of clicks per individual
# user session can be quite high in some cases. This can be seen below


med_2_dive_in <- median(DS_aggr_dive_in$visits)

hist(DS_aggr_dive_in$visits, xlab = "visits per customer", main = "frequency of visits per customer")

# taking out the outliers can be quite interesting, to see how the three metrics change. Let us keep all values <= 15



DS_aggr_dive_in = DS_aggr_dive_in %>%
  filter(visits <= 15) 

DS_aggr_dive_in <- DS_aggr_dive_in %>% mutate(Label = ifelse(visits >= med_2_dive_in, 1, 0)) # n.b. I am not changing the median value
DS_aggr_dive_in <-DS_aggr_dive_in %>% select(-visits)

misclass_calc("aggregated without outliers", DS_aggr_dive_in)

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





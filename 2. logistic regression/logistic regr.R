
rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

DS <- read.csv("../Data/clean_DS.csv")

orig <- read.delim("../Data/dataset.csv",sep = ";")

data_log <- orig[,c(5, 7, 9, 12)]


DS_cl <- DS %>% select(-X) 



median(DS_cl$visits)

DS_cl <- DS_cl %>% mutate(Label = ifelse(visits >= 4, 1, 0))
DS_cl <-DS_cl %>% select(-visits)

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

D = DS_cl %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.4, 1, 0))

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

D = DS_cl %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.4, 1, 0))

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

D = DS_cl %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.4, 1, 0))

misClas_predAll50 = mean(D$predAll50 != D$Label)

falPos_predAll50 = 
  length(which(D$Label == 0 & D$predAll50 == 1))/
  length(which(D$Label == 0))


falNeg_predAll50 = 
  length(which(D$Label == 1 & D$predAll50 == 0))/
  length(which(D$Label == 1))


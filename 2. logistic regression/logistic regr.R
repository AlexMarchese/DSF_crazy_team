
rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

DS <- read.csv("../Data/clean DF.csv")

DS_cl <- DS %>% select(-c(X, session.ID, Page_mean, mode_page1)) %>% rename(page_visits = order)


median(DS_cl$page_visits)

DS_cl <- DS_cl %>% mutate(Label = ifelse(page_visits >= 4, 1, 0))
DS_cl <-DS_cl %>% select(-page_visits)

lapply(DS_cl, class)

# logistic regression

# DS_conv = as_tibble(model.matrix(~ ., data = DS_cl)) %>% 
#   select(-"(Intercept)")

modelAll = glm(Label ~ ., family = "binomial", data = DS_cl)
predict(modelAll, type = "response")

D = DS_conv %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.5, 1, 0))

D = DS_conv %>% 
  mutate(probAll = predict(modelAll, type = "response")) %>%
  mutate(predAll50 = ifelse(probAll >=0.4, 1, 0))

misClas_predAll50 = mean(D$predAll50 != D$Label)

falPos_predAll50 = 
  length(which(D$Label == 0 & D$predAll50 == 1))/
  length(which(D$Label == 0))


falNeg_predAll50 = 
  length(which(D$Label == 1 & D$predAll50 == 0))/
  length(which(D$Label == 1))



rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

DS <- read.csv("clean DF.csv")

DS_cl <- DS %>% select(-c(X, session.ID, Page_mean, mode_page1)) %>% rename(`page visits` = order)




# logistic regression

modelAll = glm(`page visits` ~ ., family = "binomial", data = DS_cl)

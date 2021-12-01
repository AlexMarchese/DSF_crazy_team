


rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

DS_old = read.csv("dataset.csv")

names(DS_old)[1] = "year"#<- "year"


DS = DS_old %>% select(-c(year, page.2..clothing.model.))

map(DS, function(x) table(is.na(x))) # -> shows that there are no NAs


max(DS$order)

names(DS)\

# DS_test <- DS %>% group_by(session.ID) %>% which.max(order)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# DS_test$sessions <- DS %>% group_by(session.ID) %>% max(DS$order)

DS_conc <- aggregate(order ~ session.ID, DS, max)
DS_prmean <- aggregate(price ~ session.ID, DS, mean)
DS_remaind <- aggregate(. ~ session.ID, DS, Mode)



# DS_test <- DS %>% group_by(session.ID, page.1..main.category.)
# 
# DS_price <- DS %>% select(-c(page.1..main.category., colour, location, model.photography, price.2, page))
#   
# DS_remainder <- DS %>% select(-price)
# 
# nrow(DS_test)
# nrow(DS)


# logistic regression on all variables. Goal is to predict the sequence of clicks during one session


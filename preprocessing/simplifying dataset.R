
library(tidyverse)
# library(readr)
# data <- read_delim("../Data/dataset.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DS <- read.csv("../Data/dataset.csv")



Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

DS <- DS %>% select(-c(1, 2, 3, 8, 10, 13))

# DS_test$sessions <- DS %>% group_by(session.ID) %>% max(DS$order)

DS_conc <- aggregate(order ~ session.ID, DS, max)
DS_prmean <- aggregate(price ~ session.ID, DS, mean)
DS_remaind <- aggregate(. ~ session.ID, DS, Mode) %>% select(-c(order, price))

middle<-left_join(DS_conc, DS_prmean, by = "session.ID", "session.ID")
final_DS<-left_join(middle, DS_remaind, by = "session.ID", "session.ID")

names(final_DS)[2] <- "visits"
names(final_DS)[5] <- "category"

write.csv(final_DS, "../Data/clean_DS.csv")


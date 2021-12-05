
library(tidyverse)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


DS <- read.delim("../Data/dataset.csv",sep = ";")

DS <- DS %>% select(c(5, 7, 9, 12))
colnames(DS)[2] = "category"

DS_dupl <- DS %>% select(-country)

DS_unique <- unique(DS_dupl)

DS_unique$count <- 0

# class(DS_unique)
# lapply(DS_unique, class)


for (row in 0:nrow(DS_unique)) {

  category<- DS_unique$category[row]
  colour<- DS_unique$colour[row]
  price<- DS_unique$price[row]
  
  DS_unique$count[row] <- as.integer(count(DS[which(DS$category == category & DS$colour == colour & DS$price == price),]))
  
  
}

final_DS <- DS_unique


write.csv(final_DS, "../Data/rearranged_DS.csv")




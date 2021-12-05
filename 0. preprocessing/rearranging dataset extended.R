
library(tidyverse)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


DS <- read.delim("../Data/dataset.csv",sep = ";")

DS <- DS %>% select(c(5, 7, 8, 9, 10, 11, 12, 14))
colnames(DS)[2] = "category"
colnames(DS)[3] = "product_model"
colnames(DS)[6] = "type_of_picture"

# DS_dupl <- DS %>% select(-country)

DS_unique <- unique(DS)

DS_unique$count <- 0

# class(DS_unique)
# lapply(DS_unique, class)


for (row in 0:nrow(DS_unique)) {
  

  country<- DS_unique$country[row]
  product_model<- DS_unique$product_model[row]
  location<- DS_unique$location[row]
  type_of_picture<- DS_unique$type_of_picture[row]
  page<- DS_unique$page[row]
  category<- DS_unique$category[row]
  colour<- DS_unique$colour[row]
  price<- DS_unique$price[row]
  
  DS_unique$count[row] <- as.integer(count(DS[which(DS$category == category & DS$colour == colour & DS$price == price & DS$page == page & DS$type_of_picture == type_of_picture & DS$location == location & DS$product_model == product_model & DS$country == country),]))
  
  
}

final_DS <- DS_unique


write.csv(final_DS, "../Data/rearranged_extended_DS.csv")




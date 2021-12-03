
library(readr)
data <- read_delim("e-shop clothing 2008.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(tidyr)
library(tidyverse)
library(magrittr)
library(plot3D)
library(plot3Drgl)
library(car)
library(caret) 

summary(data)

mode <- function(codes){
  which.max(tabulate(codes))
}
  


table(data$colour)
table(data$country)
mode(data$colour)
mode(data$country)
mode(data$price)



#install.packages("rockchalk")
#library(rockchalk)

#m1 = lm(mpg ~ poly(wt,2) + disp, data=mtcars)

#old.par = par(mfrow=c(1,2), mar=c(1,1,1,1))

#plotPlane(m1, "wt", "disp", pch=16, col=rgb(0,0,1,0.1), drawArrows=TRUE, alength=0, 
#          acol="red", alty=1,alwd=1, theta=25, phi=0)
#plotPlane(m1, "wt", "disp", pch=16, col=rgb(0,0,1,0.1), drawArrows=TRUE, alength=0, 
#          acol="red", alty=1,alwd=1, theta=35, phi=20)



#Ragruppare clienti per colore, in caso in cui il cliente (i) ha tutti colori diversi, assegnavo un valore uguale a 0
#prezzo = media dei prezzi visti
#teniamo la nazione
#model photography  & price 2  & page 1 & page 2 teniamo il valore pi? frequente e nel caso la frequenza et zero il valore sara 0
#page tenre il valore piu alto e il piu frequente
#order valore max

data<-data %>%
  group_by(`session ID`)%>%
  select(-c(1,2,3))

data_1<-data[,c(1,2,3,4,6,8,9,11)]
data_order<-data_1[,c(1,3)]
data_country<-data_1[,c(2,3)]
data_page1<-data_1[,c(4,3)]
data_col<-data_1[,c(5,3)]
data_model<-data_1[,c(6,3)]
data_price<-data_1[,c(7,3)]
data_page<-data_1[,c(8,3)]

data_price<-data_price %>%
  summarise(across(price, mean, na.rm = TRUE))

data_country<-data_country%>%
  count(country)
data_country %<>% select(-c(3))

data_model<-aggregate(data_model,
                by = list(data_model$`session ID`),
                FUN = max)%>%
  data_model %<>% select(-c(1))

data_col <- data_col %>%
  group_by(`session ID`) %>%
  summarise(colour = mode(colour))

data_page_max<-aggregate(data_page,
                      by = list(data_page$`session ID`),
                      FUN = max)

data_page_max %<>% select(-c(1))

data_page_mean<-aggregate(data_page,
                         by = list(data_page$`session ID`),
                         FUN = mean)

data_page_mean %<>% select(-c(1))

rm(data_page)

data_order<-aggregate(data_order,
                         by = list(data_order$`session ID`),
                         FUN = max)
data_order %<>% select(-c(1))

data_page1 <- data_page1 %>%
  group_by(`session ID`) %>%
  summarise(mode_page1 = mode(`page 1 (main category)`))

middle<-left_join(data_col, data_country)
middle<-left_join(middle,data_model)
middle<-left_join(middle,data_order)
middle<-left_join(middle,data_page_max)
middle<-left_join(middle,data_page_mean, by = "session ID", "session ID")
colnames(middle)[6] <- "Page_max"
colnames(middle)[7] <- "Page_mean"
middle<-left_join(middle,data_page1)
middle<-left_join(middle,data_price)

data<-middle

colnames(data)[5] <- "Number_of_clicks"

ggplot(data, aes(x= Page_max, y = Number_of_clicks))+geom_point()


rm(list=ls())

library(readr)

data <- read_delim("1. Polynomial Regression/e-shop clothing 2008.csv", delim = ';')


library(tidyr)
library(tidyverse)
library(magrittr)
library(plot3D)
library(plot3Drgl)
library(car)
library(caret)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(tidyverse)
library(caret)
library(ggplot2)

summary(data)

#Data cleaning and handling

#Create a data set that have country, price, type of prod., and color

data_log <- data[,c(5,7,9,12)]

colnames(data_log)[2]<-'type'

#Remove the not useful columns

data<-data %>%
  group_by(`session ID`)%>%
  select(-c(1,2,3))

#Main process of cleaning and handling

data_1<-data[,c(1,2,3,4,6,8,9,11)]
data_order<-data_1[,c(1,3)]
data_country<-data_1[,c(2,3)]
data_page1<-data_1[,c(4,3)]
data_col<-data_1[,c(5,3)]
data_model<-data_1[,c(6,3)]
data_price<-data_1[,c(7,3)]
data_page<-data_1[,c(8,3)]

data_price<-data_price %>%
  summarise(across(price, mean))

data_country<-data_country%>%
  count(country)
data_country %<>% select(-c(3))

data_model<-aggregate(data_model,
                      by = list(data_model$`session ID`),
                      FUN = mean)
data_model %<>% select(-c(1))

data_model %<>% mutate(data_model,ifelse(data_model$`model photography` < 1.5, 1, 2))
colnames(data_model)[3]<-'model_photo_mean'

mode <- function(codes){
  which.max(tabulate(codes))
}

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
middle<-left_join(middle,data_model, by = 'session ID', 'session ID')
middle<-left_join(middle,data_order)
middle<-left_join(middle,data_page_max)
middle<-left_join(middle,data_page_mean, by = "session ID", "session ID")
colnames(middle)[7] <- "Page_max"
colnames(middle)[8] <- "Page_mean"
middle<-left_join(middle,data_page1)
middle<-left_join(middle,data_price)

data<-middle

rm(middle)

colnames(data)[6] <- "Number_of_clicks"
colnames(data)[4]<-'model_position'

rm(data_price,data_page1,data_col,data_country,data_model, data_order, data_page_max, data_page_mean)

colnames(data_1)[4]<-'type'
colnames(data_1)[6]<-'model'

#Some data visualization to have a better idea of how the features correlate

ggplot(data, aes(y= price, x= colour, col= colour))+geom_point() + 
  scale_color_gradient(low="blue", high="yellow") + 
  scale_x_continuous(breaks = seq(1,14,by=1)) + scale_y_continuous(breaks = 
                                                                     seq(
                                                                       min(data$price),
                                                                       max(data$price),
                                                                       by=5
                                                                     )) +
  xlab('Colours') + ylab('Price') + stat_smooth(method = lm, formula = y ~ x, col='green')



ggplot(data, aes(x=model_position, y= price, col= price)) + geom_point() + 
  scale_color_gradient(low="green", high="red") + 
  xlab('Model_Position') + ylab('Prices') + stat_smooth(method = lm, formula = y ~ x, col='blue')

###############

#The next graphs show us how the mean and the mode differ from each other.
#But the core insight is to see how the price of 40.5-45.5 is the price 
#in which the biggest 'cloud' of products stays. 


data_mid <- data

data_mid<-data_mid%>%
  group_by(country) %>%
  summarise(price_mode = mode(price))



data_mid_2 <- data
data_mid_2<-aggregate(data_mid_2,
                      by = list(data_mid_2$country),
                      FUN = mean)

data_mid_2%<>% select(-c(1))

data_mid['price_mean']<-data_mid_2[,c(10)]

ggplot(data_mid) + geom_point(aes(x= price_mean, y= country), col = 'blue') + scale_y_continuous(
  breaks = seq(1,47,2)) + geom_point(aes(x=price_mode, y= country), col= 'red') + scale_x_continuous(
    breaks = seq(min(data_mid$price_mean),max(data_mid$price_mean), 5)) + 
  xlab('Prices: Mean-Blue VS. Mode-Red') + ylab('Countries')

##############

##Correlation plot to be able to see the correlations and decide which features can be used to have an enough good model.


data_clean_cor<-data[,-c(1,5)]

data_clean_cor<-cor(data_clean_cor, method = c("pearson"))


corrplot(data_clean_cor, method= c('number'), type="full", order="hclust",
         col=brewer.pal(n=4, name="RdYlBu") , tl.srt = 90, tl.col = 'black', tl.pos = 'l')



data_cor_full<-data_1[,-c(1,3)]
data_cor_full<-cor(data_cor_full, method = c("pearson"))

corrplot(data_cor_full, method= c('number'), type="full", order="hclust",
         col=brewer.pal(n=4, name="RdYlBu") , tl.srt = 30, tl.col = 'black')



data_log_cor<-cor(data_log, method = c("pearson"))
corrplot(data_log_cor, method= c('pie'), type="full", order="hclust",
         col = brewer.pal(n=4, name="RdYlBu"), tl.srt = 90, tl.col = 'black', tl.pos = 'd')





#This takes too long and it's more complicated
#install.packages("PerformanceAnalytics")
#library("PerformanceAnalytics")

#chart.Correlation(data_[whatever you want], histogram=TRUE, pch=19)

#################
##Polynomial regression:

# See graphically the (data_log$type, data_log$price), with a linear regression.

ggplot(data_log, aes(x=type, y= price, col= price)) + geom_point() + 
  scale_color_gradient(low="cyan", high="blue") + scale_y_continuous(breaks = seq(
    min(data_log$price), max(data_log$price),10)) + 
  xlab('Type of Product') + ylab('Prices') + stat_smooth(method = lm , formula = y ~ x, col='red', size=1.5)

#Split the data

set.seed(123)
training.samples <- data_log$price %>%
  createDataPartition(p = 0.9, list= FALSE)
train.data  <- data_log[training.samples, ]
test.data <- data_log[-training.samples, ]


#See graphically the train.data with a linear regression.

ggplot(train.data, aes(x=type, y= price, col= price)) + geom_point() + 
  scale_color_gradient(low="cyan", high="blue") + scale_y_continuous(breaks = seq(
    min(data_log$price), max(data_log$price),10)) + 
  xlab('Type of Product ~ Train Dataset') + ylab('Prices') + stat_smooth(method = 'auto', formula = y ~ x, col='red', size=1.5)



#Check the R2 and the RMSE of the previous linear regression of the train.data
model_linPT <- lm(price ~ type, data = train.data)

predictions <- model_linPT %>% predict(test.data)

data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)

#R2 = 13.05%
#RMSE = 11.71



#Some others regressions with an higer degree

#Polynomial regression with a degree of 2nd order

lm_2<-lm(price ~ poly(type, 2, raw = TRUE), data = train.data)
summary(lm_2)

predictions <- lm_2 %>% predict(test.data)

data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)

#R2 = 16.27%
#RMSE = 11.49

#Visualize it

ggplot(train.data, aes(x=type, y= price, col= price)) + geom_point() + 
  scale_color_gradient(low="cyan", high="blue") + scale_y_continuous(breaks = seq(
    min(data_log$price), max(data_log$price),10)) + 
  xlab('Type of Product ~ Train Dataset ~ Degree = 2') + ylab('Prices') + 
  stat_smooth(method = 'auto', formula = y ~ poly(x, 2, raw = TRUE), size= 1.5, col='red')


#Polynomial regression of degree = 3

lm_model_final<-lm(price ~ poly(type, 3, raw = TRUE), data = train.data)
summary(lm_model_final)


predictions <- lm_model_final %>%
  predict(test.data)


data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)

#R2 = 20.40%
#RMSE = 11.21

#Let us visualize it

ggplot(train.data, aes(x=type, y= price, col= price)) + geom_point() + 
  scale_color_gradient(low="cyan", high="blue") + scale_y_continuous(breaks = seq(
    min(data_log$price), max(data_log$price),10)) + 
  xlab('Type of Product ~ Train Dataset ~ Degree = 3') + ylab('Prices') + 
  stat_smooth(method = 'auto', formula = y ~ poly(x, 3, raw = TRUE), size= 1.5, col='red')


#Since it is the best model, we now try to see the prediction intervals and then visualize them

pred_model <- predict(lm_model_final, newdata = test.data, interval="prediction") 
head(pred_model)

data_end<-cbind(test.data, pred_model)

ggplot(data_end, aes(x=type, y= price, col= price)) + geom_point() + 
  scale_color_gradient(low="cyan", high="blue") + scale_y_continuous(breaks = seq(
    min(data_log$price), max(data_log$price),10)) + 
  xlab('Type of Product ~ Train Dataset ~ Degree = 3 ~ Prediction Intervals') + ylab('Prices') + 
  stat_smooth(method = 'auto', formula = y ~ poly(x, 3, raw = TRUE), size= 1.5, col='red') + 
  geom_line(aes(y = lwr), color = "purple") +
  geom_line(aes(y = upr), color = "purple")

#Basically the only insight could be that the type of product number 2, i.e. skirts, is the product with the highest price.


#At the end a log transformation

lmlog<-lm(price ~ log(type), data = train.data)
summary(lmlog)


predictions <- lmlog %>%
  predict(test.data)


data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)

#R2 = 9.86%%
#RMSE = 11.93

#Let us visualize it

ggplot(train.data, aes(x=type, y= price, col= price)) + geom_point() + 
  scale_color_gradient(low="cyan", high="blue") + scale_y_continuous(breaks = seq(
    min(data_log$price), max(data_log$price),10)) + 
  xlab('Type of Product ~ Train Dataset ~ Log') + ylab('Prices') + 
  stat_smooth(method = 'auto', formula = y ~ log(x), size= 1.5, col='red')
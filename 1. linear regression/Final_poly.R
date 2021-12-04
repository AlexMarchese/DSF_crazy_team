rm(list=ls())

library(readr)
data <- read_delim("e-shop clothing 2008.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

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

summary(data)

##Data cleaning

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



#Some data visualization

library(ggplot2)

ggplot(data, aes(y= price, x= colour, col= colour))+geom_point() + 
  scale_color_gradient(low="blue", high="yellow") + 
  scale_x_continuous(breaks = seq(1,14,by=1)) + 
  xlab('Prices') + ylab('Colour')



ggplot(data, aes(x=model_position, y= price, col= price))+geom_point() + 
  scale_color_gradient(low="green", high="red") + 
  xlab('Model_Position') + ylab('Prices')

###############

#The next graphs show us how the mean and the mode differ from each other.
#But the core point is to see how the price of 40.5-45.5 is the price in which the greatest number of firms
#see themself. 

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
  breaks = seq(1,47, by= 1)) + geom_point(aes(x=price_mode, y= country), col= 'red') + scale_x_continuous(
    breaks = seq(min(data_mid$price_mean),max(data_mid$price_mean), by=5)) + 
  xlab('Prices: Mean-Blue VS. Mode-Red') + ylab('Countries')

##############

##Correlation plot


data_cor<-cor(data)

corrplot(data_cor, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

chart.Correlation(data_cor, histogram=TRUE, pch=19)

#################
##polynomial regression

# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth()

# Build the model
model <- lm(medv ~ lstat, data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)

lm(medv ~ lstat + I(lstat^2), data = train.data)

lm(medv ~ poly(lstat, 2, raw = TRUE), data = train.data)

lm(medv ~ poly(lstat, 6, raw = TRUE), data = train.data) %>%
  summary()

# Build the model
model <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))

##############
##############

#Log transformation

  # Build the model
  model <- lm(medv ~ log(lstat), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ log(x))

############


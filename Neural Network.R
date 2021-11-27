
rm(list = ls())

library(tidyverse)
library(caret)  # for cross validation

# Preparing the data:

Data = as_tibble(read_delim("~/Downloads/e-shop data and description/e-shop clothing 2008.csv", col_names = FALSE, skip = 1, delim = ";"))

colnames(Data) = c("year", "month",	"day", "order",	"country", "session_ID",	"page_1_main_category",	"page_2_clothing_model",	"colour",	"location",	"model_photography",	"price",	"price_2",	"page")

View(tail(D))

# Neural network with all features (except the year) and cross validation:

data_ctrl = trainControl(method = "cv", number = 5)

model_caret <- train(order ~ month + day + country + session_ID + page_1_main_category + page_2_clothing_model + colour + location + model_photography + price + price_2 + page,
                     data = Data,                      
                     trControl = data_ctrl,           # folds
                     method = "nnet",                   # specifying regression model
                     na.action = na.pass)

model_caret
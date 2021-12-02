
rm(list = ls())

library(tidyverse)
library(caret)  


# Preparatory steps ---------------

Data = as_tibble(read_delim("~/Downloads/e-shop Data and description/e-shop clothing 2008.csv", 
                            col_names = FALSE, skip = 1, delim = ";"))

colnames(Data) = c("year", "month",	"day", "orders",	"country", "session_ID",	
                   "page_1_main_category",	"page_2_clothing_model",	"colour",	
                   "location",	"model_photography",	"price",	"price_2",	"page")

Data = Data %>%
  dplyr::select(c("day", "month", "price",	"page", "orders")) %>%
  filter(orders < 38)



# Prediction using all numerical features with Neural Network---------------

## Creating index variable 

# Random sampling
samplesize = 0.60 * nrow(Data)
set.seed(80)
index = sample(seq_len(nrow(Data)), size = samplesize)

# Create training and test set
Datatrain = Data[index,]
Datatest = Data[-index,]

## Scale Data for neural network
max = apply(Data, 2, max)
min = apply(Data, 2, min)
scaled = as.Data.frame(scale(Data, center = min, scale = max - min))

## Fit neural network 

# load library (after installing library: install.packages("neuralnet"))
library(neuralnet)

# creating training and test set
trainNN = scaled[index,]
testNN = scaled[-index,]

# fit neural network
set.seed(2)
NN = neuralnet(orders ~ day + month + price + page, trainNN, hidden = 3, linear.output = T)

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(Data$orders) - min(Data$orders))) + min(Data$orders)

plot(Datatest$orders, predict_testNN, col='blue', pch=16, ylab = "predicted orders NN", xlab = "actual number of orders")

abline(0,1)

# Calculate Mean Absolute Error (MAE)
MAE = sum(abs(Datatest$orders - predict_testNN)) / nrow(Datatest)
MAE



# Cross validation of neural network model ---------------

Data_ctrl = trainControl(method = "cv", number = 5)

model_caret <- train(orders ~ day + month
                     + price + page,
                     Data = Data,                      
                     trControl = Data_ctrl,     # folds
                     method = "nnet",           # specifying regression model
                     na.action = na.pass)

model_caret


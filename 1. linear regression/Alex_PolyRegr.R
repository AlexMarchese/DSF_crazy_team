rm(list=ls())

library(readr)
data <- read_delim("../Data/dataset.csv")

library(tidyr)
library(tidyverse)
library(magrittr)
library(plot3D)
library(plot3Drgl)
library(car)
library(caret)
library(dplyr)

summary(data)





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


library(ggplot2)

ggplot(data, aes(y= price, x= colour, col= colour))+geom_point()


ggplot(data, aes(x=model_position, y= price))+geom_point()

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
  breaks = seq(1,47, by= 5)) + geom_point(aes(x=price_mode, y= country), col= 'red') + scale_x_continuous(
    breaks = seq(min(data_mid$price_mean),max(data_mid$price_mean), by=10)) + xlab('Price') + ylab('Countries')

##############


#case of 25<price<70
#skip to take in account all the prices

data<-data%>%
  filter(data$price>=25 & data$price <=75)

#SPLITTING THE DATA AND TRAIN IT
set.seed(123)
training.samples <- data$price %>%
  createDataPartition(p = 0.9, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]


#Build the model
#lm([target variable] ~ [predictor variables], data = [data source])

model <- lm(country ~ poly(price, 10, raw = TRUE),
            data = train.data)

# Make predictions

predictions <- model %>% predict(test.data)

# Model performance

modelPerfomance = data.frame(
  RMSE = RMSE(predictions, test.data$country),
  R2 = R2(predictions, test.data$country)
)

print(lm(country ~ price + I(country^3), data = train.data))
print(modelPerfomance)


#TRAINING MODEL
# ********

# We now re-estimate the above models, but ONLY ON THE TRAINING SET!
# We then want to check how well they perform when we test their 
# predictions on the testing/validation set


m1_v = lm(price ~ colour, data = train.data)
summary(m1_v)
train.data$pred1 = predict(m1_v)
Dtrain<-train.data
mse_train1 = mean((Dtrain$pred1 - Dtrain$price)^2)
mae_train1 = mean(abs(Dtrain$pred1 - Dtrain$price))


m2_v = lm(price ~ poly(colour, degree = 5), data = Dtrain)
summary(m2_v)
Dtrain$pred2 = predict(m2_v)
mse_train2 = mean((Dtrain$pred2 - Dtrain$price)^2)
mae_train2 = mean(abs(Dtrain$pred2 - Dtrain$price))


n = 10
m_n_v = lm(price ~ poly(colour, degree = n), data = Dtrain)
summary(m_n_v)
Dtrain$pred_n = predict(m_n_v)
mse_train_n = mean((Dtrain$pred_n - Dtrain$price)^2)
mae_train_n = mean(abs(Dtrain$pred_n - Dtrain$price))


# Add MAEs to our list

maeList = c(mae_train1, mae_train2, mae_train_n)
maeList = round(maeList)

# And plot the three curves

ggplot(Dtrain) + 
  geom_line(aes(x = colour, y = pred_n), col = "purple", size = 1.2) +
  geom_line(aes(x = colour, y = pred2), col = "green1", size = 1.2) +
  geom_point(aes(x=colour, y= price), col = 'red', size = 0.8) +
  scale_x_continuous(breaks = seq(1, 14, by = 1)) +
  scale_y_continuous(breaks = seq(20, 80,by= 10))



# Validation/testing
# ******************

# We now use the validation models ("_v") to make 
# predictions for the validation set

Dtest<-test.data

Dtest$pred1 = predict(m1_v, newdata = Dtest)
Dtest$pred2 = predict(m2_v, newdata = Dtest)
Dtest$pred_n = predict(m_n_v, newdata = Dtest)


mse_test1 = mean((Dtest$pred1 - Dtest$price)^3)
mse_test2 = mean((Dtest$pred2 - Dtest$price)^3)
mse_test_n = mean((Dtest$pred_n - Dtest$price)^3)


mae_test1 = mean(abs(Dtest$pred1 - Dtest$price))
mae_test2 = mean(abs(Dtest$pred2 - Dtest$price))
mae_test_n = mean(abs(Dtest$pred_n - Dtest$price))


# Add MAEs to our list

maeList = rbind(maeList, c(mae_test1, mae_test2, mae_test_n))
maeList


# Visualize

ggplot(Dtest) + geom_point(aes(x = colour, y = price), col = "turquoise", size = 0.8) + 
  geom_line(aes(x = colour, y = pred1), col = "orange", size = 1.2) + 
  geom_line(aes(x = colour, y = pred2), col = "red4", size = 1.2) +
  geom_line(aes(x = colour, y = pred_n), col = "tomato", size = 1.2) +
  ggtitle("Different models for testing data")




Dtest %>% filter(pred_n >= min(data$price) & pred_n <= max(data$price))  %>%
  ggplot() + 
  geom_point(aes(x = colour, y = price), col = "red", size = 0.8) + 
  geom_line(aes(x = colour, y = pred1), col = "cyan1", size = 1.2) + 
  geom_line(aes(x = colour, y = pred2), col = "purple", size = 1.2) +
  geom_line(aes(x = colour, y = pred_n), col = "orange", size = 1.2) + 
  scale_x_continuous(breaks = seq(1,14,by=1)) +
  ggtitle("Different models for testing data")


#CROSS VALIDATION

data_ctrl <- trainControl(method = "adaptive_cv", number = 10, repeats = 5)


# Let's see how this works for a polynomial of degree 5

model_caret <- train(price ~ poly(colour, degree = 5),   # model to fit
                     data = data,                        
                     trControl = data_ctrl,           # folds
                     method = "lm",                   # specifying regression model
                     na.action = na.pass) 


# Here is some output (there is a lot more)
model_caret

names(model_caret)

model_caret$finalModel

model_caret$results

# get MAE as
model_caret$results[["MAE"]]




# Run loop over multiple models with different
# polynomial degrees, to find the best model



polyDegree = 1:25
maeListCV = matrix(polyDegree * NA, ncol = 1)
rownames(maeListCV) = paste0("poly", polyDegree)
colnames(maeListCV) = "MAE_CV"

for (i in polyDegree){
  
  # For the "letter" i to be read
  # correctly, we need this:
  f <- bquote(price ~ poly(colour, .(i)))
  
  # This would not work, as the 'i' would not get interpreted properly
  # f = formula(yield~poly(temp, i))
  
  
  model_caret <- train(as.formula(f),   
                       data = data,                        
                       trControl = data_ctrl,              
                       method = "lm",                      
                       na.action = na.pass)  
  maeListCV[i] = model_caret$results[["MAE"]]
  
}


(maeListCV = round(maeListCV))



stop = 20
plot(polyDegree[1:stop], maeListCV[1:stop], type = "l")




# Add predictions for the winning model
# *************************************

# If there are multiple winning models, we choose the simplest one
# Here, we may be curious about comparing poly2, 3, and 4...

# How can we add predictions to the data?
# We could have saved all results, but that would be inefficient.
# Rather, let's now loop over only the winning models.


for (i in 2:4){
  
  f <- bquote(price ~ poly(colour, .(i)))
  
  model_caret <- train(as.formula(f),   
                       data = data,                        
                       trControl = data_ctrl,              
                       method = "lm",                      
                       na.action = na.pass)  
  
  varNameForD = paste0("predCV", i)
  data[[varNameForD]] = predict(model_caret$finalModel)
  
}


# And visualize the result

ggplot(data) + geom_point(aes(x = colour, y = price), col = "black") + 
  geom_line(aes(x = colour, y = predCV2), col = "purple", size = 1.5) + 
  geom_line(aes(x = colour, y = predCV3), col = "orange") + 
  geom_line(aes(x = colour, y = predCV4), col = "blue") +
  scale_x_continuous(breaks = seq(1,14,by=1))
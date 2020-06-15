# Linear regression is the relation between on dependent and one or more independent variable.
# Load the required libraries
library(tidyverse)
library(caTools)
# Load the input data
data = read.csv("Computer_Data.csv")
# We need develop a model to predict price
# 1.First we need to test linear relationship beween dependent and independent
# 2.No Multicollinearity

cor.test(data$price,data$speed)
# 1. simple lineariaty between two variables
ggplot(data = data,aes(x = speed,y= price)) +
  geom_point()

# 2. chect multicollineariaty among continuous independent variables

cor(data[c(3:6,10)])
# There is more than 70% correllation between hd and ram variables
# so we will develop 2 models for each one at a time hd and ram

cont_data <- data[c(2:6,10)]

data_hd <- cont_data[-c(4)]
data_ram <- cont_data[-c(3)]

# now we will go with data_hd

sample = sample.split(data_hd$price,SplitRatio = 0.70)

trainingset = subset(data_hd,sample == TRUE)
testset = subset(data_hd,sample == FALSE)

model_hd <- lm(price ~ .,data = trainingset)
summary(model_hd)
# R2 = 0.2839

trainingset$predict_price <- predict(model_hd,testset)

# Develop the model for ram

sample1 = sample.split(data_ram,SplitRatio = 0.70)
trainingset1 = subset(data_ram,sample == TRUE)
testset1 = subset(data_ram,sample == FALSE)

model_ram <- lm(price ~ .,data = trainingset1)
summary(model_ram)
# R2 = 0.4686

# Always we need to select highest R2 model to implement

# so by above two models we need go for model_ram

testset1$predicted_price <- predict(model_ram,testset1)

cor.test(testset1$price,testset1$predicted_price)

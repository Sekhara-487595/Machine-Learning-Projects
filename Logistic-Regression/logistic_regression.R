# Find the servival prabability on titanic data
# Load the required libraries

library(tidyverse)
library(caTools)

titanicdf <- read.csv("titanic.csv",na.strings = "")

# Know about your data
str(titanicdf)

# Remove unwanted variables

df <- titanicdf[,-c(1,4,9)]

sapply(df, function(x) sum(is.na(x)))

# By observing missing values we can remove cabin because missing values are more than 70%
str(df)
df <- df[,-8]
# There are only two missing values in embarked so we can remove that two obsevation, still we have enough observations to predit

df <- df %>% filter(is.na(Embarked) == FALSE)

# Replacing missing values of Age with mean
df$Age[is.na(df$Age)] = mean(df$Age,na.rm = TRUE)

df$Embarked[is.na(df$Embarked)] <- unique(df$Embarked)[which.max(tabulate(match(df$Embarked,unique(df$Embarked))))]

# Now my data is ready to test collinearity
# chect multicollineariaty among continuous independent variables

cor(df[c(2,4:7)])

# now multiple collinearity we can see in the df data
sample = sample.split(df$Survived,SplitRatio = 0.70)

trainingset = subset(df,sample == TRUE)
testset = subset(df,sample == FALSE)

model <- glm(Survived ~ .,data = trainingset)
summary(model)

testset$redicted_probability <- predict(model,testset)

testset$binary_prediction <- ifelse(testset$redicted_probability > 0.50,1,0)
table(testset$Survived,testset$binary_prediction)





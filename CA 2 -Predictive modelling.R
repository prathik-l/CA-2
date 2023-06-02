#Load the libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(ElemStatLearn)
library(MASS)
library(QuantPsyc)

# Read the dataset
worldhappiness <- read.csv("Worldhappiness.csv", header =T )

# Examine initial linearity between variabless
plot(worldhappiness$Life.Ladder ~ worldhappiness$Log.GDP.per.capita, main="Scatterplot of Log GDP per capita and Life Ladder")

# Examine a linear correlation in detail 
correlation <- cor('Log.GDP.per.capita', 'Life.Ladder', data=worldhappiness)

#Response variable (Y) is unexplained by the predictor (X)
model <- lm(Life.Ladder~Log.GDP.per.capita,data=worldhappiness)
summary(model)

# Examining correlation between all variables
cor_relations <- cor(worldhappiness$Life_Ladder, worldhappiness[,c(2:9)], use =  "pairwise.complete.obs")
corrplot.mixed(cor_relations,tl.cex=0.6)

# Find outliers
boxplot(worldhappiness$Log.GDP.per.capita ~ worldhappiness$Life.Ladder)

# Split data into training and testing
set.seed(61989)
#sample index 
index <-sample(1:nrow(worldhappiness),0.8 * nrow(worldhappiness))
train<-worldhappiness[index,]
test<-worldhappiness[-index,]

# Run the regression
lr_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                 Social.Support + Healthy.Life.Expectancy.At.Birth + Freedom.To.Make.Life.Choices + Generosity + 
                 Perceptions.Of.Corruption + Positive.Affect + Negative.Affect, data = train)

# Estimate coefficients
coefficients <- summary(lr_model)$coefficient

# Make predictions
prediction_train <- predict(lr_model, train)

# Calculate error
error <- mean(prediction_train - train$Life_Ladder)

# Plot the results
ggplot() + 
  geom_point(data=train, aes(x=Life_Ladder,y=prediction_train)) + 
  geom_abline(intercept=coefficients[1,1], slope=coefficients[2,1], color="red")

# Compare to Test data
prediction_test <- predict(lr_model, test)
meanabsErrorTest <- meanabsError(prediction_test, test$Life_Ladder)

# Plot the results
ggplot() + 
  geom_point(data=test, aes(x=Life_Ladder,y=prediction_test)) + 
  geom_abline(intercept=coefficients[1,1], slope=coefficients[2,1], color="blue")

# Final Prediction
final_prediction <- predict(lr_model, worldhappiness)

# Plot the results
ggplot() + 
  geom_point(data=worldhappiness, aes(x=Life_Ladder,y=final_prediction)) + 
  geom_abline(intercept=coefficients[1,1], slope=coefficients[2,1], color="black")


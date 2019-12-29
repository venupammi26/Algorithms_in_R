#Loading required R packages
#"tidyverse" for easy data manipulation and visualization
#"caret" for easy machine learning workflow
theme_set(theme_classic())

#install.packages("tidyverse")
library(tidyverse)
#install.packages("caret")
library(caret)
#install.packages("magrittr") # only needed the first time you use it
library(magrittr) # need to run every time you start R and want to use %>%
#install.packages("dplyr")    # alternative installation of the %>%
library(dplyr)    # alternative, this also loads %>%

# # Preparing the data
# We'll use the iris data set, introduced in Chapter @ref(classification-in-r), for predicting iris species based on the predictor variables Sepal.Length, Sepal.Width, Petal.Length, Petal.Width.
# Split the data into training and test set:
# Load the data
data("iris")
View(iris)
# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples <- createDataPartition(iris$Species,p = 0.8, list = FALSE)
train.data <- iris[training.samples, ]
test.data <- iris[-training.samples, ]
nrow(train.data)
nrow(test.data)
nrow(iris)
# Data Preprocessing step:
# Normalize the data. Categorical variables are automatically ignored.
# Estimate preprocessing parameters
preproc.param <- preProcess(train.data,method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <-  predict(preproc.param,train.data)
test.transformed <- predict(preproc.param,test.data)

# Now let's start the ML Classification Models
# LDA
#Quick start R code:

library(MASS)
# Fit the model
model <- lda(Species~., data = train.transformed)
# Make predictions
predictions <- predict(model,test.transformed)
# Model accuracy
mean(predictions$class==test.transformed$Species)
#Compute LDA:
  
library(MASS)
model <- lda(Species~., data = train.transformed)
model
par(mar=c(1,1,1,1))
plot(model)
#Make predictions:

predictions <- predict(model,test.transformed)
names(predictions)
#Inspect the results:

# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 

#Note that, you can create the LDA plot using ggplot2 as follow:

lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species))

#Model accuracy:

#You can compute the model accuracy as follow:
  
mean(predictions$class==test.transformed$Species)
#It can be seen that, our model correctly classified 100% of observations, which is excellent.

#####################################################
#### QDA
#####################################################
library(MASS)
# Fit the model
model <- qda(Species~., data = train.transformed)
model
# Make predictions
predictions <- predict(model,test.transformed)
# Model accuracy
mean(predictions$class == test.transformed$Species)
####################################################
#Mixture discriminant analysis - MDA
####################################################
library(mda)
# Fit the model
model <- mda(Species~., data = train.transformed)
model
# Make predictions
predicted.classes <- predict(model,test.transformed)
# Model accuracy
mean(predicted.classes == test.transformed$Species)

#####################################################
#Flexible discriminant analysis - FDA
####################################################
library(mda)
# Fit the model
model <- fda(Species~., data = train.transformed)
# Make predictions
predicted.classes <- predict(model,test.transformed)
# Model accuracy
mean(predicted.classes == test.transformed$Species)

######################################################
#Regularized discriminant analysis
######################################################
library(klaR)
# Fit the model
model <- rda(Species~., data = train.transformed)
# Make predictions
predictions <- predict(model,test.transformed)
# Model accuracy
mean(predictions$class == test.transformed$Species)
########### END ##########################################





#####################################################################
###### STUDENT PROJECT ########################################
###### DISCRIMINANT ANALYSIS: CLASSIFICATION ALGORITHMS##############
#####################################################################
#---
#Project title: "Car Evaluation Analysis"
#author: "ABC"
#date: "2019"
#---
#install.packages("tidyverse", dependencies=TRUE)
#install.packages("rlang")
#install.packages("glue")
#install.packages("tidyselect")
library(tidyverse)
library(caret)
theme_set(theme_classic())
rm(list = ls())
options(java.parameters = "-Xmx8000m") 
par(mar = rep(2, 4))

#1)Load the data

car_eval <- read.csv("D:\\datasets\\car_evaluation.csv", header=FALSE)
colnames(car_eval)<-c("buying","maint","doors","persons","lug_boot","safety","class")
head(car_eval)
#fix(car_eval)

###2)Exploratory Data Analysis

summary(car_eval)
str(car_eval)

# SECTION A: LINEAR CLASSIFICATION MODELS
#1)Linear Discriminant Analysis [LDA]
#install.packages("MASS", dependencies = TRUE)
library(MASS)

#Build the model
model1<-lda(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval)

#Summarize the model
summary(model1)

#Predict using the model
#Predict using the model
View(car_eval)
x<-car_eval[,1:6]
y<-car_eval[,7]


car_eval$pred_lda<-predict(model1,x)$class

#Accuracy of the model
mtab<-table(car_eval$pred_lda,car_eval$class)
confusionMatrix(mtab)


#SECTION B:NON-LINEAR CLASSIFICATION MODELS
#2)Mixture Discriminant Analysis
library(mda)

#Build the model
model2<-mda(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval)

#Summarize the model
summary(model2)

#Predict using the model
car_eval$pred_mda<-predict(model2,x)

#Accuracy of the model
mtab<-table(car_eval$pred_mda,car_eval$class)
confusionMatrix(mtab)


#3)Quadratic Discriminant Analysis

library(MASS)

#Build the model
model3<-qda(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval)

#Summarize the model
summary(model3)

#Predict using the model
car_eval$pred_qda<-predict(model3,x)

#Accuracy of the model
mtab<-table(car_eval$pred_qda,car_eval$class)
confusionMatrix(mtab)


#4)Regularized Discriminant Analysis

library(klaR)

#Build the model
model4<-rda(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval,gamma = 0.05,lambda = 0.01)

#Summarize the model
summary(model4)

#Predict using the model
car_eval$pred_rda<-predict(model4,x)$class

#Accuracy of the model
mtab<-table(car_eval$pred_rda,car_eval$class)
confusionMatrix(mtab)

#5)Flexible Discriminant Analysis

library(mda)

#Build the model
model5<-fda(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval)

#Summarize the model
summary(model5)

#Predict using the model
car_eval$pred_fda<-predict(model5,x,type="class")

#Accuracy of the model
mtab<-table(car_eval$pred_fda,car_eval$class)
confusionMatrix(mtab)

#Multiple Linear Regresson on mtcars

data=data.frame(mtcars)
dim(data)
names(data)
View(data)

# Multiple Linear Regression Example
fit <- lm(data$mpg ~ data$cyl + data$disp + data$hp +data$drat 
          + data$wt + data$qsec + data$vs + data$am + data$gear
          + data$carb)
summary(fit) # show results

#coefficints

coefficients(fit)

#calculate predict value

df=data.frame(4,5,10,1,100,1,4,1,5,1)
predict(fit,newdata=df, interval = "confidence" )

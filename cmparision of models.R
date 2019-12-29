#reading file
data=read.csv("D:\\DataScience\\Algorithms\\Multiple Linear Regression Algorithms\\mtcars.csv")


fit10 <- lm(data$mpg ~ data$cyl + data$disp + data$hp +data$drat 
          + data$wt + data$qsec + data$vs + data$am + data$gear
          + data$carb, data=mydata)

fit9 <- lm(data$mpg ~ data$cyl + data$disp + data$hp +data$drat 
          + data$wt + data$qsec + data$vs + data$am + data$gear
          , data=mydata)

fit8 <- lm(data$mpg ~ data$cyl + data$disp + data$hp +data$drat 
           + data$wt + data$qsec + data$vs + data$am, data=mydata)

fit7 <- lm(data$mpg ~ data$cyl + data$disp + data$hp +data$drat 
           + data$wt + data$qsec + data$vs , data=mydata)

fit6 <- lm(data$mpg ~ data$cyl + data$disp + data$hp +data$drat 
           + data$wt + data$qsec , data=mydata)

fit5<- lm(data$mpg ~ data$cyl + data$disp + data$hp +data$drat 
           + data$wt , data=mydata)

fit4 <- lm(data$mpg ~ data$cyl + data$disp + data$hp +data$drat 
           , data=mydata)

fit3 <- lm(data$mpg ~ data$cyl + data$disp + data$hp , data=mydata)

fit2 <- lm(data$mpg ~ data$cyl + data$disp, data=mydata)

fit1 <- lm(data$mpg ~ data$cyl , data=mydata)

anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10)

predict(fit,data.frame(10))










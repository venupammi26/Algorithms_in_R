
data=read.csv("C:\\Users\\reshw\\Downloads\\position.csv")

names(data)

#Linear Rgeression

model=lm(data$Salary~data$Level)
summary(model)

# Regression line + confidence intervals


plot(data$Level,data$Salary,pch=16,ylab="Salary",xlab="Level",col='red',cexlab=1.3)
abline(model,col="blue")
dev.off()
#Polynomial Regression
#In R,  create a predictor x^2 you should use the function I(), as follow: I(x^2). This raise x to the power 2.
attach(data)
pol=I(Level^4)+I(Level^3)+I(Level^2)+Level
model2=lm(Salary~pol,data)


plot(data$Level,data$Salary,pch=16,ylab="Salary",xlab="Level",col='red',cexlab=1.3)
abline(model2,col="blue")
dev.off()


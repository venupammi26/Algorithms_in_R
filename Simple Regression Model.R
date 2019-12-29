#Prediction model

data=read.csv("C:\\Salary_Data.csv",header = TRUE)

#Building Model
fit<-lm(data$Salary~data$YearsExperience)
summary(fit)

# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics



#correlation
cor(data)


#Diagnostic plots


layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

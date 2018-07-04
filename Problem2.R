#Problem - 2 Leslie Salt Data

library(ggplot2)
library(readxl)
library(corrplot)
library(car)
library(lmtest)
library(gridExtra)
library(dplyr)
library(MASS)

getwd()
setwd("C:/Users/Bhavya Gupta/Desktop/R Folder/Great Lakes/GL Data")

ga2 <- read_excel("Leslie_Salt.xlsx", col_names = T)
str(ga2)
summary(ga2)


#Linearity & Outlier
a <- ggplot(ga2, aes(x = Size, y = Price)) + geom_point()
b <- ggplot(ga2, aes(x = Elevation, y = Price)) + geom_point() 
c <- ggplot(ga2, aes(x = Sewer, y = Price)) + geom_point()
d <- ggplot(ga2, aes(x = Date, y = Price)) + geom_point()
e <- ggplot(ga2, aes(x = Distance, y = Price)) + geom_point() 

grid.arrange(a,b,c,d,e, nrow = 2, ncol = 3)

#Remove Outlier

ga2 <- filter(ga2, Price != 37.2)

#Histograms

a <- ggplot(ga2, aes(x = Size)) + geom_histogram(fill = "Blue", binwidth = 1.5)
b <- ggplot(ga2, aes(x = Elevation)) + geom_histogram(fill = "Blue")
c <- ggplot(ga2, aes(x = Sewer)) + geom_histogram(fill = "Blue")
d <- ggplot(ga2, aes(x = Date)) + geom_histogram(fill = "Blue", binwidth = 30)
e <- ggplot(ga2, aes(x = Distance)) + geom_histogram(fill = "Blue")
f <- ggplot(ga2, aes(x = Price)) + geom_histogram(fill = "Blue", binwidth = 1.2)
grid.arrange(a,b,c,d,e,f, nrow = 2, ncol = 3)

#transforming Price and Size to make it normal. Sewer and Distance are not normal.
#but any transformation applied to them will make the variables less informative as 0 values would remain 0

ga2$Price <- log(ga2$Price)
ga2$Size <- log(ga2$Size)

#ga2$Sewer.T <- (ga2$Sewer)^(1/3)
#ga2$Elevation.T <- (ga2$Elevation)^(1/3)
#ga2$Distance.T <- (ga2$Distance)^(1/2)

dev.off()
#corrleation matrix

round(cor(ga2),2) 
corrplot(cor(ga2)) #there is slight multi collinearity among independent variables

#From the corrplot we can take the Elevation, Sewer, Date and Flood for the model
#these variables have maximum correlation with the Price

model1 <- lm(Price ~ Elevation + Sewer + Date + Flood , data = ga2)
summary(model1) #Strangely, Sewer is not significant in predicting the Price. This might be due to negative correlation between Sewer & Elevation 

vif(model1) #Multi collinearit is absent
dwtest(model1) #auto correlation is absent
gqtest(model1) #heteroscadsticity is absent
ggplot(model1, aes(x = residuals(model1))) + geom_histogram(binwidth = 0.3) #Residulas are somewhat normal
par(mfrow = c(2,2))
plot(model1) #Residuals vs fitted 

#Forward Selection

model2 <- lm(Price ~ Date + Elevation + Flood + Distance, data = ga2) #All the chosen variables are significant and R2 adjusted is better than model1
summary(model2)
vif(model2) #Multi collinearit is absent
dwtest(model2)#auto correlation is absent
gqtest(model2)#heteroscadsticity is absent
ggplot(model2, aes(x = residuals(model2))) + geom_histogram(binwidth = 0.5)
par(mfrow = c(2,2))
plot(model2)

AIC(model1)
AIC(model2) #low AIC value of model 2 signify that model 2 is more optimum

#Final Verdict

#Q1) what is the nature of each variable
#A1) Price, 1size, Elevation, Sewer, Date & distance are continous variables
#    County and Flood are two categorical varibles transformed into continous using dummy binary digits

#Q2) Check whether variables require transformation indivisually
#A2) There are multiple variables which require indivisual transformation
#    Price - Price is Right skewed and hence log transformation applied to better the reuslt of linear model
#    Size - Size because again its heavily skewed and hence log trnasformation again applied to make the distribution normal
#    Other vairables also show skewed distribution but due to large no. of 0 values, trnasformation cannot be applied as it will make the variables value diificult to interpret

#Q3) Setup a regression equation, run the model and discuss the results
#A3) Please copy the commen ts from above and state the final regression equation here
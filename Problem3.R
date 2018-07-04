library(ggplot2)
library(readxl)
library(corrplot)
library(car)
library(lmtest)
library(gridExtra)

getwd()
wd <- setwd("C:/Users/Bhavya Gupta/Desktop/R Folder/Great Lakes/GL Data")
ga3 <- read_excel("All Greens Franchise.xls", sheet = "Mlr05", col_names = T)

#colnames(g3) <- c("Net Sales", "Sq. Ft", "Inventory", "Adv. Expense", "District Size", "Competing Stores")
str(ga3)
summary(ga3)


#check for linear relationship for each predictor with response variable
a <- ggplot(ga3, aes(x = X2, y = X1)) + geom_point() #Linear Relationship
b <- ggplot(ga3, aes(x = X3, y = X1)) + geom_point() #Linear Relationship
c <- ggplot(ga3, aes(x = X4, y = X1)) + geom_point() #Linear Relationship
d <- ggplot(ga3, aes(x = X5, y = X1)) + geom_point() #Linear Relationship
e <- ggplot(ga3, aes(x = X6, y = X1)) + geom_point() #Linear Relationship

grid.arrange(a,b,c,d,e, nrow = 2, ncol = 3)

#Multivariate normality

par(mfrow = c(2,3))
qqnorm(ga3$X2)
qqline(ga3$X2, col = 'Red')

qqnorm((ga3$X3))
qqline(ga3$X3, col = 'Red')

qqnorm(ga3$X4)
qqline(ga3$X4, col = 'Red')

qqnorm(ga3$X5)
qqline(ga3$X5, col = 'Red')

qqnorm((ga3$X6))
qqline(ga3$X6, col = 'Red')

#Run shapiro Test

shapiro.test(ga3$X2) #Normal
shapiro.test(ga3$X3) #Normal
shapiro.test(ga3$X4) #Normal
shapiro.test(ga3$X5) #Not Normal
shapiro.test(ga3$X6) #Normal

#transofrming X5 to bring make it less skewed
ga3$X5 <- scale(ga3$X5, center = T, scale = T)

summary(ga3)
#Histogram

a <- ggplot(data = ga3, aes(x=X2)) + geom_histogram(binwidth = 2)
b <- ggplot(data = ga3, aes(x=X3)) + geom_histogram(binwidth = 250)
c <- ggplot(data = ga3, aes(x=X4)) + geom_histogram(binwidth = 5)
d <- ggplot(data = ga3, aes(x=X5)) + geom_histogram(binwidth = 2)
e <- ggplot(data = ga3, aes(x=X6)) + geom_histogram(binwidth = 5)

grid.arrange(a,b,c,d,e, nrow = 2, ncol = 3)

#renaming the column names for the dataset

#names(ga3) <- c("NetSales", "SqFt.", "Inventory", "AdvSpend", "DistrictSize","CompetingStores")

#Collinearity

dev.off()

cor.data <- cor(ga3)
cor.data
corrplot(cor.data, method = 'color')

model1 <- lm(X1~., data = ga3)
summary(model1)


vif(model1) #to check for multi collinearlity and drop variables to remove the same

#removing X3 with maximum VIF value

#cor.data <- cor(ga3[,c(-1,-3)])
#cor.data
#corrplot(cor.data, method = 'color')


model2 <- lm(X1~X2 + X4 + X5 + X6, data = ga3)
summary(model2)

vif(model2) #Multi collinearity check

dwtest(model2) #Autocorrelation is absent

ggplot(model2, aes(x = residuals(model2))) + geom_histogram(binwidth = 30)

par(mfrow = c(2,2))
    
plot(model2)

gqtest(model2) #Homoscedasticity check


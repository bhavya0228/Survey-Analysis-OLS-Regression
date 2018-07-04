#Advanced Stats GA - Group 9 

#Question - 1
library(corrplot)
library(corrgram)
library(psych)
library(GPArotation)
library(dplyr)

getwd()
setwd("C:/Users/Bhavya Gupta/Desktop/R Folder/Great Lakes/GL Data/")

q1.data <- read.csv("cereal.csv")
q1.data <- as.data.frame(q1.data)
View(q1.data)
str(q1.data)
summary(q1.data)


# Replacing the highest value pf 6 in some variables with 5 with an assumption that the scale for every question ranges from 1:5

q1.data[q1.data == 6] <- 5
summary(q1.data)

round(cor(q1.data[,-1]),1)
corrplot(cor(q1.data[,-1]))
q1.cereal <- as.matrix(q1.data[-1])

#applying PCA
PCA1 <- princomp(~q1.cereal, scores = T, cor = T)
summary(PCA1)

loadings(PCA1)

plot(PCA1, type="line")
#Based on Scree Plot it shows that 4 factors are enough to descirbe the dataset

#Running Factor Analysis with Varimax rotation
FACT1 <- factanal(~q1.cereal, 4, rotation = "varimax", scores = "regression")
summary(FACT1)

load1 <- FACT1$loadings
load1

FACT2 <- factanal(~q1.cereal, 4, rotation = "promax", scroes = "regression")
summary(FACT2)

load2 <- FACT2$loadings
load2

FACT3 <- factanal(~q1.cereal, 4, rotation = "oblimin", scores = "regression")
summary(FACT3)

load3 <- FACT3$loadings
load3

#The Varimax and oblimin roation gives same results and optimum results

#Adding the cereal name column to the factor scores
f1 <- c(1,2,3,7,8,13,18,22,25)
f2 <- c(4,6,15,19,21)
f3 <- c(9,11,16,17,20,23,24)
f4 <- c(5,10,12,14)

q1.data.fa <- q1.data
q1.data.fa$Nutrition_Value <- apply(q1.cereal[,f1],1,mean)
q1.data.fa$Taste <- apply(q1.cereal[,f2],1,mean)
q1.data.fa$Perception <- apply(q1.cereal[,f3],1,mean)
q1.data.fa$Demographics <- apply(q1.cereal[,f4],1,mean)

q1.data.fa <- q1.data.fa[,c(1,27:30)]
str(q1.data.fa)

df2 <- q1.data.fa %>% group_by(Cereals) %>% summarise_each(funs(mean), Nutrition_Value, Taste, Perception, Demographics)

df2 <- cbind(df2[,1],round(df2[,2:5],2))
df2
#Q1 How do you characterise the consideration behaviour of the 12 brands? Analyse and interpret.

#A The 12 brands namely have the following number of observations :
#         AllBran   15
#         CMuesli   13
#      CornFlakes   27
#       JustRight   16
#        Komplete   14
#      NutriGrain   24
#         PMuesli   18
#     RiceBubbles   21
#        SpecialK   23
#         Sustain   12
#        Vitabrit   25
#        Weetabix   27
# the table above shows that CornFlakes and Weetabix accounts for the most number of observations i.e 27
# while CMuseli accounts for the least i.e 13.

#If were to give equal weightage to all the variables we can also conclude that they have an average rating, on the basis of likert scale, as follows:
#       AllBran  2.94        
#       CMuesli  3.29        
#    CornFlakes  3.06        
#     JustRight  3.08        
#      Komplete  3.14        
#    NutriGrain  3.20        
#       PMuesli  3.35        
#   RiceBubbles  2.87        
#      SpecialK  2.98        
#       Sustain  3.26        
#      Vitabrit  3.09        
#      Weetabix  3.03
#This shows that CMuesli has a best rating of 3.29 out of 5 in the given 12 cereals while RiceBubbles has the lowest rating of 2.87.

#However there are 25 variables in this dataset which makes the task of gathering the data tidious when taking feedback from customers.
#So by applying factor analysis we have reduced the number of variables from 25 to 4 namely:
#    Nutrition_Value
#    Taste
#    Perception
#    Dempgraphics

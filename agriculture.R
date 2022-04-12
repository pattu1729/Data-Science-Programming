#Load the Data
library(dplyr)
library(ggplot2)
library(reshape2)
library(corrplot)

grains <- read.csv("C:/Users/veniniyan/Downloads/Table_8.3-All_India_1.csv")
rain <- read.csv("C:/Users/veniniyan/Downloads/All_India_Area_Weighted_Monthly_Seasonal_And_Annual_Rainfall.csv")

#Exploratory Data Analysis (EDA) - Grain Production
str(grains)

cormat <- round(cor(as.matrix(grains[,2:36])),2)

##Function 
get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

lowertri <- get_lower_tri(cormat)
melted.cormat <- melt(lowertri, na.rm = TRUE)

##Sorting the corelation matrix to pick up variables with maximum positive or negative corelation
sorted.cormat <- melted.cormat[order(melted.cormat$value),]

neg.cormat <- head(sorted.cormat, 6)

ggplot(data = neg.cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile(color = "white") 

pos.cormat <- sorted.cormat[c(140:170),]

ggplot(data = pos.cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile(color = "white") 

#EDA - Rainfall
head(rain)
#Combined EDA
rain.new <- rain %>% filter(YEAR >=2001)
grains.new <- grains[c(1:14),]

## Combining the two datasets
grainsrain <- cbind(grains.new, rain.new)
grains1 <- melt(grainsrain, id = "ANN", measure = c("rice", "maize"))
ggplot(data = grains1, aes(x = ANN, y = value, color = variable)) + geom_point() + xlab("Annual Rainfall in mm") + ylab("Grain Production (1000s tonnes)")
ggplot(grainsrain, aes(x = ANN, y = mesta)) + geom_point()+ xlab("Annual Rainfall in mm") + ylab("Crop Production (1000s bales)")
ggplot(grainsrain, aes(x = ANN, y = maize)) + geom_point()+ xlab("Annual Rainfall in mm") + ylab("Grain Production (1000s tonnes)")




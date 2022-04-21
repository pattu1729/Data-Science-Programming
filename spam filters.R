#Loading library
library(caTools)
library(kernlab)

#view data
data(spam)
str(spam[,1:5])

#spam types
table(spam$type)

#Training and Testing Modle
set.seed(32)
split=sample.split(spam$type,SplitRatio=0.7)
trainSpam=subset(spam,split==T)
testSpam=subset(spam,split==F)
names(spam)
head(spam)[,1:7]

#Comparing the values of data classified as spam vs nonspam
plot(log10(trainSpam$capitalAve+1)~trainSpam$type,ylab="Frequency of occurence",xlab="Type")

#Analysing the relationships between various predictors
plot(log10(trainSpam[,1:4]+1))

#Performing hierarchical clustering
mdist=dist(t(log(trainSpam[,1:55]+1)))
hclustering=hclust(mdist)
plot(as.dendrogram(hclustering))

#Converting the label from character string to numeric type
trainSpam$numType=as.numeric(trainSpam$type)-1

#Function to calculate cost function
costFunc=function(x,y){
  sum(x!=(y>0.5))
}

#Initialising a numeric vector to store the error
cvError=rep(NULL,55)
#Fitting a linear model
library(boot)
suppressWarnings(
    for(i in 1:55){
       lmFormula=reformulate(names(trainSpam)[i],response = "numType")
       glmFit=glm(lmFormula,family = "binomial",data=trainSpam)
       cvError[i]=cv.glm(trainSpam,glmFit,costFunc,2)$delta[2]
       }
     )

#Getting a measure of uncertainity
predModel=suppressWarnings(
    glm(
       numType ~ charDollar+charExclamation+remove+money+free,family = "binomial",data = trainSpam))
  
#Getting predictions on the test set
pred_y=as.character(
    ifelse(
       as.numeric(predict(predModel,testSpam))>0.5,"spam","nonspam"))

#Comparing actual vs predicted   
crossTab=table(pred_y,testSpam$type)
crossTab

#Accuracy
accuracy=sum(crossTab[-c(2:3)])/sum(crossTab[1:4])
accuracy
      
      

d <- c(1,2,3,4)
e <- c("red", "white", "red", "sadf")
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
mydata
myframe[3:5] # columns 3,4,5 of dataframe
myframe[c("ID","Age")] # columns ID and Age from dataframe
mydata$residuals # variable x1 in the dataframe 



attach(mydata)
mydata$sum <- x1 + x2
mydata$mean <- (x1 + x2)/2
detach(mydata)
mydata <- transform( mydata,sum = x1 + x2,mean = (x1 + x2)/2 )

z <- matrix(1:10, nrow=5, ncol=2)
tz <- mytrans(z)
tz

a <- c(1,2,3,4)
b <- c("red", "white", "red", "asg")
c <- c(TRUE,TRUE,TRUE,FALSE)
mydata1 <- data.frame(a,b,c)
total <-rbind(mydata,mydata1) 
total

a=c(1,2,3)
b=c(2,4,6)
d1=data.frame(a,b)
a=c(1,2,3)
b=c(2,4,6)
d2=data.frame(a,b)
total <-merge(d1,d2) 
total


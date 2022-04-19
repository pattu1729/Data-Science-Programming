sales=read.csv("C:\\Users\\veniniyan\\Downloads\\Datasets\\house_data.csv")
sales
str(sales)

which(is.na(data))
sum(is.na(data))

summary(sales)

set.seed(100)
library(caTools)
split=sample.split(sales,SplitRatio=0.7)
train=subset(sales,split="TRUE")
test=subset(sales,split="FALSE")
train
test

model=lm(data=train,price~bedrooms+bathrooms+sqft_living+view+grade+long+lat+zipcode+condition+sqft_above+sqft_living15)
summary(model)
res=residuals(model)
res
plot(model)

#prediction
pred=predict(model,test)
pred



predtally_table=data.frame(actual=train$price, predicted=pred)
mape=mean(abs(tally_table$actual-tally_table$predicted)/tally_table$actual)
accuracy=1-mape
cat("The accuracy of the model is : ",accuracy)

pred_test=predict(newdata=test,model)
tally_table1=data.frame(actual=test$price, predicted=pred_test)
mape_test=mean(abs(tally_table1$actual-tally_table1$predicted)/tally_table1$actual)
accuracy_test=1-mape_test
cat("The accuracy of the testing model is : ",accuracy_test)


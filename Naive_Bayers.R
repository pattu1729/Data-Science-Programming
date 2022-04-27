# Naïve Bayes Classifier
library(e1071)
ir=iris
train=ir[1:100,]
test=ir[101:50,]
model=naiveBayes(Species~.,data=train)
pred=predict(model,test)
pred
table(pred)
table(test$Species)
table(pred,test$Species)


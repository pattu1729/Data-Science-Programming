#installing pacakages
install.packages("tidyverse")
install.packages("plotly")
install.packages("ggsci")
install.packages("caret")
install.packages("e1071")

#Reading data
data=read.csv("C:/Users/veniniyan/Downloads/Datasets/diabetes.csv")
data
names(data)
str(data)
summary(data)
data$Outcome=factor(data$Outcome)
class(data$Outcome)

#Scatter Plot
p3=ggplot(data,aes(x=Age,y=Pregnancies,size=Glucose,fill=BloodPressure))+geom_point(alpha=0.2)+facet_grid(.~Outcome)+geom_jitter(width = 0.4)+scale_x_continuous(limits = c(18, 80))+scale_fill_material("red")

ggplotly(p3)

#Training and Testing Data
trainRowNumbers <- createDataPartition(data$Outcome, p=0.8, list=FALSE)
trainData=data[trainRowNumbers,]
testData=data[-trainRowNumbers,]
x=trainData[,1:8]
y=trainData$Outcome
xt= testData[, 1:8]
yt=testData$Outcome

  
  preProcess_range_modeltr <- preProcess(trainData, method='range')
  preProcess_range_modelts <- preProcess(testData, method='range')
  
    trainData <- predict(preProcess_range_modeltr, newdata = trainData)
    testData <- predict(preProcess_range_modelts, newdata = testData)
    
      trainData$Outcome=y
      testData$Outcome=yt
      
        
      fitControl <- trainControl(
           method = 'cv',                   
             number = 5,                    
             savePredictions = 'final',       
             classProbs = T,                  
             summaryFunction=twoClassSummary  
         ) 
      
        levels(trainData$Outcome) <- c("Class0", "Class1")
        levels(testData$Outcome) <- c("Class0", "Class1")
        
        #Creating Model
        model1 = train(Outcome ~ ., data=trainData, method='lda', tuneLength = 5, metric='ROC', trControl = fitControl)
        summary(model1)
        predicted=predict(model1, testData[,1:8])
        
        #Confusion Matrix
        confusionMatrix(reference = testData$Outcome, data = predicted, mode='everything')
       
        confusionMatrix(reference = testData$Outcome, data = predicted, mode='everything')
        
        
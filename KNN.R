#KNN
data<-iris
row_labels=data[,5]
data$Species=as.numeric(data$Species)
data[,1:4]=scale(data[,1:4])
set.seed(123)
size=floor(0.8*nrow(data))
train_ind=sample(seq_len(nrow(data)),size=size)
train_label=data[train_ind,5]
test_label<-row_labels[-train_ind]
data_train=data[train_ind,1:4]
data_test=data[-train_ind,1:4]

library(class)
pred_model=knn(train=data_train,test=data_test,cl=train_label,k=round(sqrt(nrow(data_train))))
plot_predictions<-data.frame(data_test$Sepal.Length,data_test$Sepal.Width,
                             data_test$Petal.Length,data_test$Petal.Width,predicted=pred_model)

colnames(plot_predictions)<-c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Class")

library(ggplot2)
library(gridExtra)
pl<-ggplot(plot_predictions,aes(Sepal.Length,Sepal.Width,color=pred_model,fill=pred_model))+
  geom_point(size=5)+geom_text(aes(label=test_label),hjust=1,vjust=2)+
  ggtitle("Predicted relationship between Sepal Length and Sepal Width")+
  theme(plot.title=element_text(hjust=0.5))+theme(legend.position="none")
pl


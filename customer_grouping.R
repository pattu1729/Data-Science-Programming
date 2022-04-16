#Install library
library(readr)
library(ggplot2)
library(GGally)
library(purrr)
library(NbClust)
library(factoextra)

#Load dataset
Mall_Customers <- read_csv("C:/Users/veniniyan/Downloads/Customers.csv")

#View dataset
View(Mall_Customers)

#summary of dataset
summary(Mall_Customers)

#To find missing values
any(is.na(Mall_Customers))

#Histogram of Age filling Gender
ggplot(Mall_Customers,aes(x= Age, fill=Gender))+geom_histogram(bins = 50) 
#Histogram of Annual_Income filling Gender
ggplot(Mall_Customers,aes(x= `Annual_Income`,fill=Gender)) +geom_histogram(bins = 50)
#Histogram of Spending_Score filling Gender
ggplot(Mall_Customers,aes(x= `Spending_Score`,fill=Gender)) +geom_histogram(bins=50)
#Bar plot
ggplot(Mall_Customers,aes(x= Gender))+geom_bar ()
#Frequency Polygon
ggplot(Mall_Customers,aes(x= `Spending_Score`, col=Gender)) + geom_freqpoly(bins=50)
ggplot(Mall_Customers,aes(x= `Annual_Income`, col=Gender)) + geom_freqpoly(bins=50)

#Correlation and Multi-collinearity
ggcorr(Mall_Customers)
ggcorr(Mall_Customers, label = TRUE, label_alpha = TRUE)
par(mfrow=c(2,2))
plot(Mall_Customers)
X<-Mall_Customers[,2:5]
ggpairs(X)

#kmeans clustering
#Elbow plot
set.seed(123)
#function to calculate total intra-cluster sum of square
iss<-function(k)
{kmeans(Mall_Customers[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss}
k.value<-1:10
iss_value<- map_dbl(k.value, iss)
plot(k.value, iss_value,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total intra-clusters sum of squares")

#Average Silhouette method
fviz_nbclust(Mall_Customers[,3:5], kmeans, method = "silhouette")

#Gap statistic method
stat_gap <- clusGap(Mall_Customers[,3:5], FUN = kmeans, nstart = 25,K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

#Plottinf 6 Culsters
pcclust= prcomp(Mall_Customers[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

k6<-kmeans(Mall_Customers[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
# Annual_Income vs Spending_Score clusters 
ggplot(Mall_Customers, aes(x =`Annual_Income`, y = `Spending_Score`)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",breaks=c("1", "2", "3", "4", "5","6"), labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

#Spending_Score vs age clusters
ggplot(Mall_Customers, aes(x =`Spending_Score`, y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",breaks=c("1", "2", "3", "4", "5","6"),labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

#Annual_Income vs age clusters
ggplot(Mall_Customers, aes(x =`Annual_Income`, y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",breaks=c("1", "2", "3", "4", "5","6"),labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

#Using only customer ID for easy tracking
o=order(k6$cluster)
data.frame(Mall_Customers$CustomerID[o],k6$cluster[o])

#Mapping using all the other columns
x = data.frame(Mall_Customers$CustomerID[o],Mall_Customers$Gender[o], Mall_Customers$Age[o], Mall_Customers$`Annual_Income`[o], Mall_Customers$`Spending_Score`[o],k6$cluster[o])

#Dataset with clusters
View(x) 















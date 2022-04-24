#KMEANS CLUSTERING
library(e1071)
Iris.Features=iris
Iris.Features$Species<-NULL
View(Iris.Features)
results=kmeans(Iris.Features,3)
results
results$size
results$cluster
table(iris$Species,results$cluster)
plot(iris[c("Petal.Length","Petal.Width")],col=results$cluster)
plot(iris[c("Petal.Length","Petal.Width")],col=iris$Species)
plot(iris[c("Sepal.Length","Sepal.Width")],col=results$cluster)

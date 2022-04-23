#HAC CLUSTER
install.packages("factoextra")
library(factoextra)
iris_data=iris[1:4]
iris_data_std=scale(iris_data)
iris_dist=dist(iris_data_std)
iris_dist
myclust=hclust(iris_dist,method="complete")
plot(myclust)
rect.hclust(myclust,k=5,border=2.5)
iris.cluster=cutree(myclust,k=3)
rownames(iris_data_std)<-paste(iris$Species,1:dim(iris)[1],sep="-")
fviz_cluster(list(data=iris_data_std,cluster=iris.cluster))

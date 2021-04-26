setwd("~/Desktop/Machine Learning/Assignment_05")
#Importing dataset in R
cereals.df<-read.csv("Cereals.csv")
#Eliminating unwanted variables from the dataset
cereals.df <-select(cereals.df,-c('mfr','type'))
#Giving the row number as name and eliminating the "name" from the dataset to avoid data duplication
rownames(cereals.df)<-cereals.df$name
cereals.df$name=NULL
#Data Normalization
cereals.df<-as.data.frame(scale(cereals.df))
#Data Preprocessing
sum(is.na(cereals.df))
cereals.df<-na.omit(cereals.df)
#Dissimilarity matrix
d <-dist(cereals.df,method="euclidean")
#Hierarchical clustering and Plotting the dendrogram
hc_single <- hclust(d,method="single")
plot(hc_single,cex=0.6,hang=-1)

hc_complete <- hclust(d,method="complete")
plot(hc_complete,cex=0.6,hang=-1)

hc_average <- hclust(d,method="average")
plot(hc_average,cex=0.6,hang=-1)

hc_ward <- hclust(d,method="ward.D")
plot(hc_ward,cex=0.6,hang=-1)
#Compute with Agnes and different linkage methods 
#Compare Agglomerative coefficients
hc_Single <- agnes(d,method="single")
print(hc_Single$ac)

hc_Complete <- agnes(d,method="complete")
print(hc_Complete$ac)

hc_Average <- agnes(d,method="average")
print(hc_Average$ac)

hc_Ward <- agnes(d,method="ward")
print(hc_Ward$ac)
#The Best Linkage method is Ward with Agglomerative coefficient of 0.9049881.
#Plot Dendrogram
pltree(hc_Ward,cex=0.6,hang=-1,main="Dendrogram of Ward")


#2Q Choosing the number of clusters
d <-dist(cereals.df,method="euclidean")
hc_ward <- hclust(d,method="ward.D")
plot(hc_ward,cex=0.6)
rect.hclust(hc_ward,k=3,border=1:3)
clust<-cutree(hc_ward,k=3)

#3Q Stability of cluster
set.seed(123)
kmeans_clust <- kmeans(cereals.df,3,nstart=10)
km_data <- kmeans_clust$cluster
cereal_cluster <- as.data.frame(cbind(kmeans_clust$cluster,cereals.df))
colnames(cereal_cluster)[1]<-"cluster"
print_clusters <-function(labels,k)
{
  for(i in 1:3)
  {
    print(paste("cluster", i))
    print(cereal_cluster[labels==i,c("cluster","calories" ,"protein" , "fat" ,     "sodium"  , "fiber"  , "carbo"  ,  "sugars"  , "potass"  , "vitamins" ,"shelf" ,   "weight"  , "cups"  ,   "rating"  )])
    
  }
}
groups <- cutree(hc_ward, k=3)
print_clusters(groups,3)
kbest.p<- 3       
cboot.hclust <- clusterboot(cereals.df,clustermethod=hclustCBI,method="ward.D", k=kbest.p)
groups<-cboot.hclust$result$partition 
cboot.hclust$bootmean
which.max(cboot.hclust$bootmean)
#Cluster 3 has highest stability(cluster stability) amongst all.

#4Q Cluster with healthy Cereal and High nurition value
mydata <- na.omit(cereals.df)
rating <- cbind(mydata,clust)
rating[rating$clust==1,]
rating[rating$clust==2,]
rating[rating$clust==3,]


#Cluster 1 has has high protein and fibre content .Hence,it can be said that cluster 1 possess high nutritional value and has healthy cereals after normalization.
#Also cluster 1 has highest rating amongst all .
#Yes,the data should be normalized.
    
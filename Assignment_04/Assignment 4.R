library(readr)
library(dplyr)
library(caret)
library(factoextra)
library(tidyverse)

#set the working directory
setwd("~/Desktop/Machine Learning/Assignment /Assignment_04")

#Import the dataset
PHARMACEUTICALS <- read.csv("Pharmaceuticals.csv")

#Cleaning the data and checking for any null values in each of the column
colSums(is.na(PHARMACEUTICALS)) ## returns the number of null values in each column

#a. Use only the numerical variables (1 to 9) to cluster the 21 firms. Justify the various choices made in
#conducting the cluster analysis, such as weights for different variables, the specific clustering algorithm(s)
#used, the number of clusters formed, and so on.
PHARMACEUTICALS_NUMERIC_VALUES <- PHARMACEUTICALS[,c(3:11)]

# Scaling quantitative variables in the data frame (z-score)
DATA<- as.data.frame(scale(PHARMACEUTICALS_NUMERIC_VALUES))
distance <- get_dist(DATA)
fviz_dist(distance) # visualizing a distance matrix 

# Estimating the  number of clusters
# Elbow Method on scaled data
fviz_nbclust(DATA,FUNcluster = kmeans,method = "wss")+labs(subtitle="Elbow Method")

# Silhouette Method on scaled data
fviz_nbclust(DATA,FUNcluster = kmeans,method = "silhouette")+labs(subtitle="Silhouette Method")

set.seed(1)
K5 <- kmeans(DATA,centers=5,nstart=25) #k=5
K5$centers   #Centroids
K5$size # size of each clusters
fviz_cluster(K5,data=DATA) #visualize the clusters

#K-Means Cluster Analysis - Fit the data with 5 clusters
fit <- kmeans(DATA, 5)
aggregate(DATA,by=list(fit$cluster),FUN=mean)
DATA1 <- as.data.frame(DATA, fit$cluster)
DATA1

library(cluster)
clusplot(DATA ,fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


# (b)Interpret the clusters with respect to the numerical variables used in forming the clusters
# Cluster 1 - Row 8,9,12,14
# 
# Cluster 2 - Row 1,4,7,10,16,19,21
# 
# Cluster 3 - Row 2,6,18
# 
# Cluster 4 - Row 3,5,20
# 
# Cluster 5 - Row 11,13,15,17
# 
# By the output of > aggregate(ps,by=list(fit$cluster),FUN=mean), we can observe the following:
# 
# Cluster 1 has lowest Market_Cap,highest Beta,lowest PE_Ratio,highest Leverage,highest Rev_Growth.
# 
# Cluster 2 has lowest Rev_Growth,highest Net_Profit_Margin
# 
# Cluster 3 has highest PE_Ratio,lowest_ROE,lowest ROA,lowest Net_Profit_Margin
# 
# Cluster 4 has lowest Beta,lowest Asset_Turnover
#
# Cluster 5 has highest Market_Cap,highest ROE, highest ROA,highest Asset_Turnover,lowest Leverage.
# 
# (c)Is there a pattern in the clusters with respect to the numerical variables (10 to 12)? (those not used in
# forming the clusters)

# Cluster 1 with highest Beta ,highest Leverage,highest Rev_Growth has moderate buy Recommendation
# Cluster 2 with highest Net_Profit_Margin has in most of the cases hold Recommendation
# Cluster 3 with highest PE_Ratio has hold Recommendation.
# Cluster 4 with lowest Asset_Turnover has strong Buy Recommendation
# Cluster 5 with highest Market_Cap,highest ROE, highest ROA,highest Asset_Turnover has equal hold Moderate Buy
# With respect to media Recommendation Variable ,the clusters follow a particular pattern 
# Cluster 1 and Cluster 4 has moderate buy Recommendation.
# Cluster 2 and Cluster 3 has Hold Recommendation.

# (d) Provide an appropriate name for each cluster using any or all of the variables in the dataset.

# Cluster 1 - lowest Market_Cap,highest Beta,lowest PE_Ratio,highest Leverage,highest Rev_Growth cluster

# Cluster 2 - lowest Rev_Growth,highest Net_Profit_Margin cluster
 
# Cluster 3 - highest PE_Ratio,lowest_ROE,lowest ROA,lowest Net_Profit_Margin cluster

# Cluster 4 - lowest Beta,lowest Asset_Turnover cluster
 
# Cluster 5 - highest Market_Cap,highest ROE, highest ROA,highest Asset_Turnover,lowest Leverage cluster
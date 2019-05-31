library(dplyr)
library(tidyverse)
library(factoextra)


scaled_df <- as.data.frame(scale(df))
colMeans(scaled_df)
summary(scaled_df)


#Silhoutte Plot for Elbow method
fviz_nbclust(scaled_df, kmeans, method = "silhouette", k.max = 8)



#------#

#https://www.guru99.com/r-k-means-clustering.html 
set.seed(1234)
#Within group SS plot to see how the variations occur with different K values. 
kmclust_withinss <- function(k){
  kmclust <- kmeans(scaled_df,k, 10)
  return(kmclust$tot.withinss)
}

# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmclust_withinss)

# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)
  
# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))


#------#
#https://bookdown.org/rdpeng/exdata/k-means-clustering.html 

set.seed(1234)
kcenters <- c(2,7,11)


image(t(scaled_df)[, nrow(scaled_df):1], yaxt = "n", main = paste("Original Data"))
image(t(scaled_df_mat)[, nrow(scaled_df_mat):1], yaxt = "n", main = paste("Original Data_matrix"))


for(k in kcenters){
scaled_df_mat <- scale(df)

kmclust <- kmeans(scaled_df,k, 10)
kmclust_mat <- kmeans(scaled_df_mat,k, 10)

image(t(scaled_df)[, order(kmclust$cluster)], yaxt = "n", main = paste("Clustered Data k=",k))
image(t(scaled_df_mat)[, order(kmclust_mat$cluster)], yaxt = "n", main = paste("Clustered Data_matrix k=",k))
}

#------#

#HEATMAP to highlight difference between feature values wrt each cluster

library(tidyr)

#centers
center <- kmclust$centers

# create dataset with the cluster number

cluster <- c(1: 11)
center_df <- data.frame(cluster, center)

# Reshape the data

center_reshape <- gather(center_df, features, values, .jour_nr: tlq_tlq02_4)
head(center_reshape)

#conda install -c r r-rcolorbrewer : in cmd
library(RColorBrewer)

# Create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

#Plot the heat map
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 11, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()


#------#



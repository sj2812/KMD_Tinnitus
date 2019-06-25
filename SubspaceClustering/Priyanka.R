library(subspace)
library(ggplot2)
library(Rtsne)
library(r2d3)
#library(ChemmineR)
library(scatterplot3d)

ProClus.clusters.k2 <- ProClus(df_scaled,k=4)


#install.packages("BiocManager")
#BiocManager::install("ChemmineR")

#identify the row ID for samples in each cluster (e.g. ProClus2[[1]]$objects identifies cluster 1
#rowIDs)
cluster1<-ProClus.clusters.k2[[1]]$objects
cluster2<-ProClus.clusters.k2[[2]]$objects
cluster3<-ProClus.clusters.k2[[3]]$objects
cluster4<-ProClus.clusters.k2[[4]]$objects


labels<-c()

for(i in cluster1){
  
  labels[i] <- 1
  
}
for(i in cluster2){
  
  labels[i]<- 2
  
}
for(i in cluster3){
  
  labels[i]<- 3
  
}
for(i in cluster4){
  
  labels[i]<- 4
  
}

df_labeled <- df%>%
  mutate(label = labels)
df_labeled%>%head


subspace::


#plot(ProClus.clusters.k2,final_data)

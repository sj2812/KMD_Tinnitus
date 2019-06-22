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


cluster_features<-data.frame(df_labeled%>%
                               select(-.jour_nr)%>%
                               group_by(label)%>%
                               summarise_each(mean))
feature_summary<-data.frame(df%>%
                              select(-.jour_nr)%>%
                              summarise_each(mean))


#Plotting Clusters in Radial Bar Chart

cluster_features <- cluster_features[1:4,]

options(scipen = 999)

f<-function(cluster)
{ cluster_plot_data<-rbind(feature_summary,cluster[-1])
cluster_name<-paste("cluster_",cluster[1])
rownames(cluster_plot_data)<-c("general_mean",cluster_name)
cluster_plot_data<-t(cluster_plot_data)%>%data.frame()
cluster_plot_data<-mutate(cluster_plot_data,mean_difference = cluster_plot_data[,1]-cluster_plot_data[,2],feature=rownames(cluster_plot_data))
r2d3(data = cluster_plot_data, script = "cluster_chart.js",viewer ="internal")
}
apply(cluster_features,1,f)


#plot(ProClus.clusters.k2,final_data)

library(subspace)
library(ggplot2)
library(Rtsne)
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




#plot(ProClus.clusters.k2,final_data)

# plot(final_data, col = rows.2.1, pch = 19, frame = FALSE,
 #     main = "Proclus")
#points(km.res$centers, col = 1:2, pch = 8, cex = 3)


# scatterPlot <- ggplot(final_data,aes(x, y, color=group)) + 
#   geom_point() + 
#   scale_color_manual(values = c('#999999','#E69F00')) + 
#   theme(legend.position=c(0,1), legend.justification=c(0,1))
# scatterPlot
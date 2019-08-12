library(diceR)
library("clValid")



evaluate<-function(data){
  View(data)
  result_set<-vector("list", 3)
  result_set[1]<-dunn(clusters = data$label, Data=df_allF_scaled)
  result_set[2]<-connectivity(distance = NULL, data$label, Data = df_allF_scaled, neighbSize = 10, method = "euclidean")
  result_set[3]<-compactness(df_allF_scaled, data$label)
  print(result_set)
  return(result_set)
}


#kmeans_result_2<-evaluate(df_scaled_labeled_kmeans)
hkmeans_result_2<-evaluate(df_allF_scaled_hk2_labeled)
#hc_result_2<-evaluate(df_scaled__labeled_hc)

#kmeans_result_4<-evaluate(df_scaled_labeled_kmeans)
hkmeans_result_4<-evaluate(df_allF_scaled_hk4_labeled)
#hc_result_4<-evaluate(df_scaled__labeled_hc)
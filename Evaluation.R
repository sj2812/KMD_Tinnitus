library(diceR)
library("clValid")

kmeans_result_2<-evaluate(df_scaled_labeled_kmeans)
hkmeans_result_2<-evaluate(df_scaled__labeled_hkmeans)
hc_result_2<-evaluate(df_scaled__labeled_hc)

kmeans_result_4<-evaluate(df_scaled_labeled_kmeans)
hkmeans_result_4<-evaluate(df_scaled__labeled_hkmeans)
hc_result_4<-evaluate(df_scaled__labeled_hc)

evaluate<-function(data){
  result_set<-vector("list", 3)
  result_set[1]<-dunn(clusters = data[79], Data=df_scaled)
  result_set[2]<-connectivity(distance = NULL, data[79], Data = df_scaled, neighbSize = 10, method = "euclidean")
  result_set[3]<-compactness(df_scaled, data[79])
  return(result_set)
}



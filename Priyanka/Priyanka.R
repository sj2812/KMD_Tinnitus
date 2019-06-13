library(tidyverse)
library(caret)
library(mlbench)

set.seed(7)

test <- df

#correlation matrix

 #corr_matrix <- cor(test)
# print(corr_matrix)
# 
# highly_corr <- findCorrelation(corr_matrix,cutoff = 0.75,verbose = TRUE)
# print(highly_corr)

 
 #library(corrplot)
 
 # for (i in 1:nrow(corr_matrix)){
 #   correlations <-  which((corr_matrix[i,] > 0.85) & (corr_matrix[i,] != 1))
 #   
 #   if(length(correlations)> 0){
 #     print(colnames(test)[i])
 #     print(correlations)
 #   }
 # }
 
 
 corr_check <- function(Dataset, threshold){
   matriz_cor <- cor(Dataset)
   matriz_cor
   
   for (i in 1:nrow(matriz_cor)){
     correlations <-  which((abs(matriz_cor[i,i:ncol(matriz_cor)]) > threshold) & (matriz_cor[i,i:ncol(matriz_cor)] != 1))
     
     if(length(correlations)> 0){
       lapply(correlations,FUN =  function(x) (cat(paste(colnames(Dataset)[i], "with",colnames(Dataset)[x]), "\n")))
       
     }
   }
 }
 
 print("correlation with 95%")
 corr_check(test,0.95)
 print("correlation with 90%")
 corr_check(test,0.9)
 print("correlation with 85%")
 corr_check(test, 0.85)
 

#feature selection


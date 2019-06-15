library(tidyverse)

set.seed(7)

test <- df
test2 <- data.frame(F1 = character(),F2 = character(),coef = numeric())


cat("\ncorrelation with 90%:\n")
matriz_cor <- cor(test,method = "spearman")

for (i in 1:nrow(matriz_cor)){
   correlations <-  which((abs(matriz_cor[i,]) > 0.9) & (matriz_cor[i,] != 1))
   matriz_cor[correlations,i] <- NA
   
   if(length(correlations)> 0){
      lapply(correlations,FUN =  function(x) (cat("\t",paste(colnames(test)[i], "with",colnames(test)[x]), "\n")))
      test2 <-  rbind(test2,data.frame(F1=colnames(test)[i],F2=colnames(test)[correlations],coef=matriz_cor[i,correlations]))
      rownames(test2) <- NULL
   }
   
}


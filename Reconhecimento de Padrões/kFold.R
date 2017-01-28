
# Função que determina os folds 
kFold <- function(data, k) {   
  proClass <- table(data[1])   
  qntdClass <- length(proClass)   
  classes <- names(proClass)   
  matrixResult <- matrix(data =0, nrow=k, ncol = (qntdClass+1),                           
  dimnames = list(1:k, c(classes,"soma")))   
  for(i in 1:qntdClass){  
    propi <- as.integer(proClass[i] / k)     
    resto <- proClass[i] - (propi*k)  
    matrixResult <- matrixResult[order(matrixResult[,qntdClass+1], decreasing = FALSE),]     
      for(j in 1:k){  
        matrixResult[j, i] <- (matrixResult[j, i] + propi)       
        if(resto > 0){  
          matrixResult[j, i] <- (matrixResult[j, i] +1)         
          resto <- resto - 1  
        }  
        matrixResult[j, qntdClass+1] = (matrixResult[j, qntdClass+1] + matrixResult[j, i])  
      }  
  }  
  return(matrixResult)  
}
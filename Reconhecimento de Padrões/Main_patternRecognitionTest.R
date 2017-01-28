library(e1071) 
library(FSelector)  
library(nnet)
source("kFold.R")
source("naiveBayesCross.R")
source("nnetCross.R")
source("svmCross.R")
spectF <- read.table("file.txt", sep = '\t', header=TRUE) spectF$Classe = factor(spectF$Classe) spectF <- as.data.frame(spectF)  


### Naive Bayes ####
#Execução com todas as características print(" -- Todas as características -- ")  
naiveBCrossValid(spectF, 10);  

#Execução com Relief  
weights1 <- relief(Classe ~., spectF, neighbours.count = 79, sample.size = 80) 
subsetRelief <- cbind(spectF[1], spectF[cutoff.k(weights1, 22)]) 
print(" -- Com Relief -- ") 
naiveBCrossValid(subsetRelief, 10);   

#Execução com PCA 
pca <- princomp(spectF[-1], cor=FALSE)  
newpca <- cbind(spectF[1],predict(pca, newdata = spectF))[0:8] 
print(" -- Com PCA -- ") 
naiveBCrossValid(newpca, 10);  



### Neural Network ###
#Execução com todas as características
nnetCrossValid(spectF, 10, 1, 0.1);
print(" -- 2 -- ")
nnetCrossValid(spectF, 10, 5, 0.05);
print(" -- 3 -- ")
nnetCrossValid(spectF, 10, 10, 0.01);
print(" -- 4 -- ")
nnetCrossValid(spectF, 10, 10, 0.1)
print(" -- 5 -- ")
nnetCrossValid(spectF, 10, 1, 0.01)

#Execução com Relief
weights1 <- relief(Classe ~., spectF, neighbours.count = 79, sample.size = 80)
subsetRelief <- cbind(spectF[1], spectF[cutoff.k(weights1, 22)])
print(" -- Com Relief -- ")
print(" -- 1 -- ")
nnetCrossValid(subsetRelief, 10, 1, 0.1);
print(" -- 2 -- ")
nnetCrossValid(subsetRelief, 10, 5, 0.05);
print(" -- 3 -- ")
nnetCrossValid(subsetRelief, 10, 10, 0.01);
print(" -- 4 -- ")
nnetCrossValid(spectF, 10, 10, 0.1)
print(" -- 5 -- ")
nnetCrossValid(spectF, 10, 1, 0.01)


#Execução com PCA
pca <- princomp(spectF[-1], cor=FALSE)
newpca <- cbind(spectF[1],predict(pca, newdata = spectF))[0:8]
print(" -- Com PCA -- ")
print(" -- 1 -- ")
nnetCrossValid(newpca, 10, 1, 0.1);
print(" -- 2 -- ")
nnetCrossValid(newpca, 10, 5, 0.05);
print(" -- 3 -- ")
nnetCrossValid(newpca, 10, 10, 0.01);
print(" -- 4 -- ")
nnetCrossValid(spectF, 10, 10, 0.1)
print(" -- 5 -- ")
nnetCrossValid(spectF, 10, 1, 0.01)





### SVM ###

custos <- c(0.1, 0.5, 1, 10, 100)
degree <- c(0.3, 0.9, 1, 3 ,10)
coef <- c(0.1, 0.5, 0, 1, 10)
gama <- c(0.1, 0.5, 1, 10, 100)

#Execução com todas as características
print(" -- Todas as características -- ")
for(c in 1:length(custos)){
  print("Linear. Parâmetros: ")
  print(custos[c])
  svmCrossValid(spectF, 10, 'linear', custos[c], 0, 0, 0);
  for(g in 1:length(gama)){
    print("radial Parâmetros: ")
    print(gama[g])
    svmCrossValid(spectF, 10, 'radial', custos[c], gama[g], 0, 0);
    for(cf in 1:length(coef)){
      print("sigmoid Parâmetros: ")
      print(coef[cf])
      svmCrossValid(spectF, 10, 'sigmoid', custos[c], gama[g], coef[cf], 0);
      for(d in 1:length(degree)){
        print("polynomial Parâmetros: ")
        print(degree[d])
        svmCrossValid(spectF, 10, 'polynomial', custos[c], gama[g], coef[cf], degree[d]);  
      }
    }
  }
}


print(" -- Com Relief -- ")
for(c in 1:length(custos)){
  print("Linear. Parâmetros: ")
  print(custos[c])
  svmCrossValid(subsetRelief, 10, 'linear', custos[c], 0, 0, 0);
  for(g in 1:length(gama)){
    print("radial Parâmetros: ")
    print(gama[g])
    svmCrossValid(subsetRelief, 10, 'radial', custos[c], gama[g], 0, 0);
    for(cf in 1:length(coef)){
      print("sigmoid Parâmetros: ")
      print(coef[cf])
      svmCrossValid(subsetRelief, 10, 'sigmoid', custos[c], gama[g], coef[cf], 0);
      for(d in 1:length(degree)){
        print("polynomial Parâmetros: ")
        print(degree[d])
        svmCrossValid(subsetRelief, 10, 'polynomial', custos[c], gama[g], coef[cf], degree[d]);  
      }
    }
  }
}
print(" -- Com PCA -- ")
for(c in 1:length(custos)){
  print("Linear. Parâmetros: ")
  print(custos[c])
  svmCrossValid(newpca, 10, 'linear', custos[c], 0, 0, 0);
  for(g in 1:length(gama)){
    print("radial Parâmetros: ")
    print(gama[g])
    svmCrossValid(newpca, 10, 'radial', custos[c], gama[g], 0, 0);
    for(cf in 1:length(coef)){
      print("sigmoid Parâmetros: ")
      print(coef[cf])
      svmCrossValid(newpca, 10, 'sigmoid', custos[c], gama[g], coef[cf], 0);
      for(d in 1:length(degree)){
        print("polynomial Parâmetros: ")
        print(degree[d])
        svmCrossValid(newpca, 10, 'polynomial', custos[c], gama[g], coef[cf], degree[d]);  
      }
    }
  }
}
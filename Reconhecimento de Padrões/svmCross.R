svmCrossValid <- function(data, k, kernel, c, gama, coef, degree){
  folds <- kFold(data, k)
  classe1 <-subset(data, Classe == 1)
  classe0 <-subset(data, Classe == 0)
  c1 <-0
  c0 <-0
  recall <-0
  precisao <-0
  erro <-0
  for(i in 1:k){
    end1 <- c1+folds[i,"1"]
    end0 <- c0+folds[i,"0"]
    subset<- rbind(classe1[-((c1+1):end1),],classe0[-((c0+1):end0),])
    subsetTest <- rbind(classe1[((c1+1):end1),],classe0[((c0+1):end0),])
    classify <- NULL
    if(kernel=='linear'){
      classify<- svm(Classe ~ .,subset, kernel = kernel, cost = c)  
    }else if(kernel == 'radial'){
      classify<- svm(Classe ~ .,subset, kernel = kernel, cost = c, gama = gama)
    }else if(kernel == 'sigmoid'){
      classify<- svm(Classe ~ .,subset, kernel = kernel, cost = c, gama = gama, coef0 = coef)
    }else{
      classify<- svm(Classe ~ .,subset, kernel = kernel, cost = c, gama = gama, coef0 = coef, degree = degree)
    }
    result<- table(predict(classify, subsetTest[,-1]), subsetTest$Classe)
    recall <- recall + (result["1","1"]/(result["1","1"]+result["1","0"]))
    precisao <- precisao +(result["1","1"]/(result["1","1"]+result["0","1"]))
    erro <- erro + (result["1","0"]+result["0","1"])
    c1=end1;
    c0=end0;
  }
  recall = recall/k;
  precisao = precisao/k;
  erro = erro/k;
  list(recall, precisao, erro)
}

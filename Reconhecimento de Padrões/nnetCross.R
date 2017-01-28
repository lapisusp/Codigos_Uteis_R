#Função que roda o redes neurais com Cross Validation 
nnetCrossValid <- function(data, k, numCamadas, taxaAprendizado){
  folds <- kFold(data, k)
  classe1 <-subset(data, Classe == 1)
  classe0 <-subset(data, Classe == 0)
  c1 <-0
  c0 <-0
  recall <-0
  precisao <-0
  erro <-0
  for(i in 1:k){
    TP<-0 
    FP<-0
    TN<-0 
    FN <-0
    end1 <- c1+folds[i,"1"]
    end0 <- c0+folds[i,"0"]
    subset<- rbind(classe1[-((c1+1):end1),],classe0[-((c0+1):end0),])
    subsetTest <- rbind(classe1[((c1+1):end1),],classe0[((c0+1):end0),])
    classify<- nnet(Classe ~ .,subset, size=numCamadas, decay=taxaAprendizado)
    result <-table(subsetTest$Classe, predict(classify, subsetTest[,-1], type = "class"))
    if("1" %in% rownames(result)){
      if("1" %in% colnames(result)){
        TP <- result["1","1"];
      }
      if("0" %in% colnames(result)){
        FN <- result["1","0"]
      }
    }
    if("0" %in% rownames(result)){
      if("0" %in% colnames(result)){
        TN <- result["0","0"]
      }
      if("1" %in% colnames(result)){
        FP <- result["0","1"]
      }
    }
    
    
    recall <- recall + (TP/(end1-c1))
    precisao <- precisao +(TP/(TP+FP))
    erro <- erro + (FN+FP)
  }
  
  recall = recall/k;
  precisao = precisao/k;
  erro = erro/k;
  list(recall, precisao, erro)
}
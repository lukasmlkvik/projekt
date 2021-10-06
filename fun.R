mse <-function(a,b){
  return(sum((a-b)*(a-b))/length(a))
}
sse <-function(a,b){
  return(sum((a-b)*(a-b)))
}


createTree <- function(formula, data, fun = sse, err = 0.5, maxK = 100, minGroupe = 1){
  #
  data2 = model.frame(formula,data)
  countX= ncol(data2) -1;
  countO= nrow(data2);
  node=data.frame(1)
  node$value = mean(data2[,1])
  #kontrola, ci je splnene nejake omedzenie
  if(err >= fun(data2[,1], (1:countO / 1:countO)*mean(data2[,1])) || countO <= minGroupe*2 || maxK == 0){
    return(node)
  }
  
  #hodnoty chyby zo zadanej funkcie, pri rozdeleny dat na pozicii i
  values = matrix(nrow = countX, ncol = countO)
  #zoradenie podla parametra
  pomIndexis = matrix(nrow = countX, ncol = countO)
  
  
  #nstavenie hodnot pre kazde rozdelenie
  for (i in 1:countX) {
    pomIndexis[i,] = order(data2[,i+1])
    for (j in minGroupe:(countO-minGroupe)) {
        values[i,j] = fun(data2[pomIndexis[i,1:j],1], (1:j / 1:j)*mean(data2[pomIndexis[i,1:j],1])) +
        fun(data2[pomIndexis[i,(j+1):countO],1], (((j+1):countO )/ ((j+1):countO))*mean(data2[pomIndexis[i,(j+1):countO],1]))
    }
  }
  min = Inf
  index =0
  separate = 0
  #najdenie najlepsieho rozdelenia dat
  for (i in 1:countX) {
    pomIndexis[i,] = order(data2[,i+1])
    for (j in (countO-minGroupe):minGroupe) {
      if(min > values[i,j]){
        min = values[i,j];
        index = i
        separate = j
      }
    }
  }
  
  #nastavenie vetvenia
  node$param = colnames(data2)[index+1]
  node$compareValue = (data2[pomIndexis[index, separate],node$param]+data2[pomIndexis[index, separate+1],node$param])/2
  
  node$less = createTree(formula, data2[1:pomIndexis[index,separate],], fun, err, maxK -1,minGroupe);
  node$egualesMore = createTree(formula, data2[pomIndexis[index,(separate+1)]:countO,], fun, err,maxK -1,minGroupe);
  
  return(node)
}

#predikcia pomocou modelu pre jeden zaznam
predictionRec <-function(model, data){
  if(is.null(model$param)){
    return(model$value);
  }
  
  if(data[model$param] < model$compareValue){
    return(predictionRec(model$less,data))
  }
  return(predictionRec(model$egualesMore,data))
}

#predikcia pomocou modelu pre cely datatrame
prediction <-function(model, data){
  return(apply(data,1, FUN = function(x)predictionRec(model,x)))
}

test= data.frame(1:20)
test$X1 = 1:20
test$Y = c(0,0,0,0,5,20,100,100,100,100,70,65,60,55,50,45,10,0,0,0)



model = createTree(Y ~ X1, test,fun=sse,maxK=50,minGroupe = 4)

prediction(model, test)

mse(test$Y,prediction(model, test))


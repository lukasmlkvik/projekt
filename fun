mse <-function(a,b){
  return(sum((a-b)*(a-b))/length(a))
}
sse <-function(a,b){
  return(sum((a-b)*(a-b)))
}


createTree <- function(formula, data, fun = sse, err = 0.5, maxK = 100, minGroupe = 1){
  data2 = model.frame(formula,data)
  countX= ncol(data2) -1;
  countO= nrow(data2);
  node=data.frame(1)
  node$value = mean(data2[,1])
  if(err >= fun(data2[,1], (1:countO / 1:countO)*mean(data2[,1])) || countO <= minGroupe*2 || maxK == 0){
    return(node)
  }
  
  values = matrix(nrow = countX, ncol = countO)
  pomIndexis = matrix(nrow = countX, ncol = countO)
  
  
  
  for (i in 1:countX) {
    pomIndexis[i,] = order(data2[,i+1])
    for (j in 1:countO) {
      if(j == countO){
        values[i,j] = fun(data2[pomIndexis[i,1:j],1], (1:j / 1:j)*mean(data2[pomIndexis[i,1:j],1]))
      }else{
        values[i,j] = fun(data2[pomIndexis[i,1:j],1], (1:j / 1:j)*mean(data2[pomIndexis[i,1:j],1])) +
        fun(data2[pomIndexis[i,(j+1):countO],1], (((j+1):countO )/ ((j+1):countO))*mean(data2[pomIndexis[i,(j+1):countO],1]))
      }
        
    }
  }
  min = 1000000
  #wm = which.min(pomIndexis)  
  index =1# wm%%countX
  separate = 0#floor(wm/countO)+1;
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
  #if(min >=  node$value){
  #  return(node)
  #}

  #if(separate == 0){
   # return(node)
  #}
   #print(values)
  #print(pomIndexis)
  #print(separate)
  
  node$param = colnames(data2)[index+1]
  node$compareValue = (data2[pomIndexis[index, separate],node$param]+data2[pomIndexis[index, separate+1],node$param])/2
  node$less = createTree(formula, data2[1:pomIndexis[index,separate],], fun, err, maxK -1,minGroupe);
  node$egualesMore = createTree(formula, data2[pomIndexis[index,(separate+1)]:countO,], fun, err,maxK -1,minGroupe);
  return(node)
}

predictionRec <-function(model, data){
  if(is.null(model$param)){
    return(model$value);
  }
  
  if(data[model$param] < model$compareValue){
    return(predictionRec(model$less,data))
  }
  return(predictionRec(model$egualesMore,data))
}

prediction <-function(model, data){
  return(apply(data,1, FUN = function(x)predictionRec(model,x)))
}

test= data.frame(1:20)
test$X1 = 1:20
test$Y = c(0,0,0,0,5,20,100,100,100,100,70,65,60,55,50,45,10,0,0,0)



model = createTree(Y ~ X1, test,fun=sse,maxK=50,minGroupe = 4)

prediction(model, test)

mse(test$Y,prediction(model, test))


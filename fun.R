mse <-function(a,b){
  return(sum((a-b)*(a-b))/length(a))
}
sse <-function(a,b){
  return(sum((a-b)*(a-b)))
}


#pomocna funkcia aby sa zakazdym nemuseli kopirovat data a indexMatica, ale posle sa len referencia(ak to R tak robi) na data a interval
createTreeRec<-function(data, fun = sse, err = 0.5, maxK = 100, minGroupe = 1, start, end, indexMatrix){
  pocetPozorovani = (end-start+1)
  pocetPremenych = ncol(data) -1
  node=data.frame(1)
  node$value = mean(data[start:end,1])
  
  #podmienky pre ukoncenie vytvarania dalsieho vetvenia
  if(err >= fun(data[start:end,1], node$value) || pocetPozorovani <= minGroupe*2 || maxK == 0){
    return(node)
  }
  
  #hodnoty chyby zo zadanej funkcie, pri rozdeleni dat na pozicii i(index 1 rozdeli data na minGroupe a zvysok)
  values = matrix(nrow = pocetPremenych, ncol = pocetPozorovani-2*minGroupe+1)
  
  #nastavenie hodnot pre kazde rozdelenie
  for (i in 1:pocetPremenych) {
    for (j in (minGroupe+start-1):(end-minGroupe)) {
      values[i,j-start+2-minGroupe] = fun(data[indexMatrix[i,start:j],1],mean(data[indexMatrix[i,start:j],1])) +
        fun(data[indexMatrix[i,(j+1):end],1], mean(data[indexMatrix[i,(j+1):end],1]))
    }
  }
  min = Inf
  index =0
  separate = 0
  #najdenie najlepsieho rozdelenia dat
  for (i in 1:pocetPremenych) {
    for (j in (1):(pocetPozorovani-2*minGroupe+1)) {
      if(min > values[i,j]){
        min = values[i,j]
        index = i
        separate = j+(start-2+minGroupe)
      }
    }
  }
  
  #nastavenie vetvenia
  node$param = colnames(data)[index+1]
  node$compareValue = (data[indexMatrix[index, separate],node$param]+data[indexMatrix[index, separate+1],node$param])/2
  
  node$less = createTreeRec(data, fun, err, maxK -1,minGroupe,start,separate,indexMatrix)
  node$egualesMore = createTreeRec(data, fun, err,maxK -1,minGroupe,separate+1,end,indexMatrix)
  
  return(node)
}


#'      Funkcia, ktora vytvory regresny model 
#' @param formula - zapisuje na v tvare "Y ~ X1 + X2 + ...",kde Y je zavisla premenna, a X1 ,X2 ,... su nezavysle premenne
#' @param data - trenovacie data
#' @param fun - funkcia, ktora pocita chybu, ktorej sa model dopusta pri rozdeleni dat
#' @param err - akceptovana chyby, ak je hodnota z fun nemcia, dane data sa dalej nerozdeluju
#' @param maxK - maximalna hlbka stromu
#' @param minGroupe - minimalna velkost mnoziny rozdelenych dat (aby nedoslo k pretrenovaniu)
#' 
createTree <- function(formula, data, fun = sse, err = 0.5, maxK = 100, minGroupe = 1){
  
  #filtrovanie len potrebnych dat
  data2 = model.frame(formula,data)
  pocetPremenych= ncol(data2) -1
  pocetPozorovani= nrow(data2)

  
  #matica indexov, zoradenie podla parametra
  pomIndexis = matrix(nrow = pocetPremenych, ncol = pocetPozorovani)
  for (i in 1:pocetPremenych) {
    pomIndexis[i,] = order(data2[,i+1])
  }

  return(createTreeRec(data2, fun, err,maxK ,minGroupe,1,pocetPozorovani,pomIndexis))
}

#predikcia pomocou modelu pre jeden zaznam
predictionRec <-function(model, data){
  if(is.null(model$param)){
    return(model$value)
  }
  
  if(data[model$param] < model$compareValue){
    return(predictionRec(model$less,data))
  }
  return(predictionRec(model$egualesMore,data))
}

#predikcia pomocou modelu pre cely dataframe
prediction <-function(model, data){
  return(apply(data,1, FUN = function(x)predictionRec(model,x)))
}


#zatial som nenasiel ine data s ktorymi by som to porovnal
test= data.frame(1:20)
test$X1 = 1:20
test$Y = c(0,0,0,0,5,20,100,100,100,100,70,65,60,55,50,45,10,0,0,0)

model = createTree(Y ~ X1, test,fun=sse,maxK=50,minGroupe = 4)

prediction(model, test)

mse(test$Y,prediction(model, test))


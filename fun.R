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

# Podbobna ako predchodza funkcia, akorat sa data vzdy kopituju, ale dokaze rozdelit data podla skupin
createTreeRec2<-function(data, fun = sse, err = 0.5, maxK = 100, minGroupe = 1){
  
  pocetPremenych= ncol(data) -1
  pocetPozorovani= nrow(data)
  
  node=data.frame(1)
  node$value = mean(data[,1])
  
  #podmienky pre ukoncenie vytvarania dalsieho vetvenia
  if(err >= fun(data[,1], node$value) || pocetPozorovani <= minGroupe*2 || maxK == 0){
    return(node)
  }
  
  #hodnoty chyby zo zadanej funkcie, pri rozdeleni dat na pozicii i(index 1 rozdeli data na minGroupe a zvysok)
  values = matrix(nrow = pocetPremenych, ncol = pocetPozorovani-2*minGroupe+1)
  indexMatrix = matrix(nrow = pocetPremenych, ncol = pocetPozorovani)
  
  #nastavenie hodnot pre kazde rozdelenie
  for (i in 1:pocetPremenych) {
    indexMatrix[i,] = order(data[,i+1])
    if(is.character(data[1,i+1])){
      for (j in (minGroupe):(pocetPozorovani-minGroupe)) {
        filter = data[indexMatrix[i,1:pocetPozorovani],i+1]
        j_offset = j+1-minGroupe
        prvaPolovica = data[indexMatrix[i,1:pocetPozorovani][filter==filter[j_offset]],1]
        druhaPolovica = data[indexMatrix[i,1:pocetPozorovani][filter!=filter[j_offset]],1]
        values[i,j_offset] = fun(prvaPolovica,mean(prvaPolovica)) + fun(druhaPolovica, mean(druhaPolovica))
      }
    }else{
      for (j in (minGroupe):(pocetPozorovani-minGroupe)) {
        prvaPolovica = data[indexMatrix[i,1:j],1]
        druhaPolovica = data[indexMatrix[i,(j+1):pocetPozorovani],1]
        values[i,j+1-minGroupe] = fun(prvaPolovica,mean(prvaPolovica)) + fun(druhaPolovica, mean(druhaPolovica))
      }
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
        separate = j+(-1+minGroupe)
      }
    } 
  }
  
  #ukoncenie vetvenia(toto vetvenie by nevytvorilo lepsi vysledok)
  if(min >= fun(data[,1], node$value)){
    return(node)
  }
  
  #nastavenie vetvenia
  node$param = colnames(data)[index+1]
  
  if(is.character(data[1,node$param])){
    node$compareValue = data[separate,node$param]
    
    node$less = createTreeRec2(data[data[,node$param]==node$compareValue,], fun, err, maxK -1,minGroupe)
    node$egualesMore = createTreeRec2(data[data[,node$param]!=node$compareValue,], fun, err,maxK -1,minGroupe)
  }else{
    node$compareValue = (data[indexMatrix[index, separate],node$param]+data[indexMatrix[index, separate+1],node$param])/2
    
    node$less = createTreeRec2(data[i:separate,], fun, err, maxK -1,minGroupe)
    node$egualesMore = createTreeRec2(data[(separate+1):pocetPozorovani,], fun, err,maxK -1,minGroupe)
  }
  
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

  #return(createTreeRec(data2, fun, err,maxK ,minGroupe,1,pocetPozorovani,pomIndexis))
  return(createTreeRec2(data2, fun, err,maxK ,minGroupe))
}

#predikcia pomocou modelu pre jeden zaznam
predictionRec <-function(model, data){
  if(is.null(model$param)){
    return(model$value)
  }
  
  if(is.character(model$compareValue)){
    if(data[model$param] == model$compareValue){
      return(predictionRec(model$less,data))
    }
  }else{
    if(data[model$param] < model$compareValue){
     return(predictionRec(model$less,data))
    }
  }
  return(predictionRec(model$egualesMore,data))
}

#predikcia pomocou modelu pre cely dataframe
prediction <-function(model, data){
  return(apply(data,1, FUN = function(x)predictionRec(model,x)))
}

path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
titanic[1,]

titanicRand = sample(titanic)
train = titanicRand[1:1000,]
test = titanicRand[1001:1200,]

model = createTree(survived ~ sex  + age + pclass , train,fun=sse,maxK=10,minGroupe = 5)
model

vysledok  = prediction(model, test)
pravdepodobnost = 0.5
vysledok[vysledok<pravdepodobnost] = 0
vysledok[vysledok>=pravdepodobnost] = 1

mse(test$survived,vysledok)

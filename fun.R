mse <-function(a,b){
  return(sum((a-b)*(a-b))/length(a))
}
sse <-function(a,b){
  return(sum((a-b)*(a-b)))
}

#rekurzivne vytvaranie rozdeleni
createTreeRec<-function(trainData, fun = sse, err = 0.5, maxK = 100, minGroupe = 1){
  pocetPremenych= ncol(trainData) -1
  pocetPozorovani= nrow(trainData)
  

  node=data.frame(1)
  node$value = mean(trainData[,1])
  
  #podmienky pre ukoncenie vytvarania dalsieho vetvenia
  if(err >= fun(trainData[,1], node$value) || pocetPozorovani <= minGroupe*2 || maxK == 0){
    return(node)
  }
  
  #hodnoty chyby zo zadanej funkcie, pri rozdeleni dat na pozicii i(index 1 rozdeli trainData na minGroupe a zvysok)
  values = matrix(nrow = pocetPremenych, ncol = pocetPozorovani-2*minGroupe+1)
  #matica indexov, zoradenie podla parametra
  indexMatrix = matrix(nrow = pocetPremenych, ncol = pocetPozorovani)
  
  #nastavenie hodnot pre kazde rozdelenie
  for (i in 1:pocetPremenych) {
    if(is.character(trainData[1,i+1])){
      for (j in (minGroupe):(pocetPozorovani-minGroupe)) {
        j_offset = j+1-minGroupe
        prvaPolovica = trainData[trainData[,i+1]==trainData[j_offset,i+1],1]
        druhaPolovica = trainData[trainData[,i+1]!=trainData[j_offset,i+1],1]
        values[i,j_offset] = fun(prvaPolovica,mean(prvaPolovica)) + fun(druhaPolovica, mean(druhaPolovica))
      }
    }else{
      indexMatrix[i,] = order(trainData[,i+1])
      for (j in (minGroupe):(pocetPozorovani-minGroupe)) {
        prvaPolovica = trainData[indexMatrix[i,1:j],1]
        druhaPolovica = trainData[indexMatrix[i,(j+1):pocetPozorovani],1]
        values[i,j+1-minGroupe] = fun(prvaPolovica,mean(prvaPolovica)) + fun(druhaPolovica, mean(druhaPolovica))
      }
    }
  }
  min = Inf
  index =0
  separate = 0
  #najdenie najlepsieho rozdelenia dat
  for (i in 1:pocetPremenych) {
    for (j in (pocetPozorovani-2*minGroupe+1):(1)) {
      if(min > values[i,j] ){
        if(is.character(trainData[1,i+1]) || trainData[indexMatrix[i,j+(-1+minGroupe)],i+1]!=trainData[indexMatrix[i,j+(+minGroupe)],i+1]){
          min = values[i,j]
          index = i
          separate = j+(-1+minGroupe)
        }
      }
    } 
  }
  
  #ukoncenie vetvenia(toto vetvenie by nevytvorilo lepsi vysledok)
  if(min >= fun(trainData[,1], node$value)){
    return(node)
  }
  
  #nastavenie vetvenia
  node$param = colnames(trainData)[index+1]
  
  if(is.character(trainData[1,node$param])){
    node$compareValue = trainData[separate,node$param]
    
    node$less = createTreeRec(trainData[trainData[,node$param]==node$compareValue,], fun, err, maxK -1,minGroupe)
    node$egualesMore = createTreeRec(trainData[trainData[,node$param]!=node$compareValue,], fun, err,maxK -1,minGroupe)
  }else{
    node$compareValue = (trainData[indexMatrix[index, separate],node$param]+trainData[indexMatrix[index, separate+1],node$param])/2
    
    node$less = createTreeRec(trainData[indexMatrix[index, 1:separate],], fun, err, maxK -1,minGroupe)
    node$egualesMore = createTreeRec(trainData[indexMatrix[index, (separate+1):pocetPozorovani],], fun, err,maxK -1,minGroupe)
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
#' zdroj : https://www.youtube.com/watch?v=g9c66TUylZ4&t=1111s
#' 
createTree <- function(formula, data, fun = sse, err = 0.5, maxK = 100, minGroupe = 1){
  
  #filtrovanie len potrebnych dat
  data2 = model.frame(formula,data)
  
  for (i in 1:ncol(data2)) {
    c = data2[1,i]
    print(c)
   if(!is.numeric(c)&&!is.character(c)&&!is.factor(c)){
     stop("Bad columns!!!")
   } 
  }

  return(createTreeRec(data2, fun, err,maxK ,minGroupe))
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
  pocetPozorovani = nrow(data)
  vysledok = 1:pocetPozorovani
  for (i in 1:pocetPozorovani) {
    vysledok[i] = predictionRec(model,data[i,]) 
  }
  return(vysledok)
}

#vypisanie modelu
printTree <- function(model, m=""){
  if(is.null(model$param)){
    print(paste(m,model$value))
  }else{
    print(paste(m,model$param, model$compareValue))
    printTree(model$less,paste(m," |"));
    printTree(model$egualesMore,paste(m,"  "));
  }
}

#nacitanie a uprava modelu
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
titanic[1,]
titanic$age=strtoi(titanic$age)
titanic = titanic[!is.na(titanic$age),]
titanic$fare=strtoi(titanic$fare)
titanic[is.na(titanic$fare),"fare"] = 0
titanic$pclass = paste(titanic$pclass)

#rozdelenie na trenovacie a testovacie data
titanicRand = sample(titanic)
train = titanicRand[1:700,]
test = titanicRand[701:900,]

#vyhtorenie modelu a testovanie rychlosti
zaciatok = Sys.time()
model = createTree(survived ~ sex + age + pclass + fare, train,fun=sse,maxK=10,minGroupe = 20)
Sys.time() - zaciatok

printTree(model)

#vytvorenie predikcii na testovacich datach
vysledok  = prediction(model, test)
#kedze zavizla premenna, ktoru chceme zistit je bud 0 alebo 1, tak vysledok predikcie sa zaokruhli
pravdepodobnost = 0.5
vysledok[vysledok<pravdepodobnost] = 0
vysledok[vysledok>=pravdepodobnost] = 1

#kontrola presnosti modelu na testovacich datach
table(vysledok,test$survived)


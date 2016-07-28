#Ricordatevi di settare la wd all'inizio, (la vostra, dah) else puzzate e siete stupidi.
setwd("~/Documents/Kaggle/Taxi")

#caricate train e test con stringAsFactors=FALSE, else non vi funzia un cazzo
train <- read.csv("~/Documents/Kaggle/Taxi/train.csv",stringsAsFactors=FALSE)
test <- read.csv("~/Documents/Kaggle/Taxi/test.csv",stringsAsFactors=FALSE)

#queste due libraries sono necessarie per poter splittare le polylines. Importatele
library(readr)
library(rjson)
#funzione magica: inserisci una riga, ti restituisce una tabella x,y di posizioni, in ordine
#di tutti i punti della polyline di QUELLA riga
positions <- function(row) as.data.frame(do.call(rbind, fromJSON(row$POLYLINE)))

#example usage:
positions(train[1,]) #posizioni della prima polyline del train


#Interpolazione "proporzionale":
#carichiamo un data frame solo x avere già la struttura della submission.
#probabilmente si potrebbe fare anche sub <- *crea un vettore da 1 a 320*
sub <- read.csv("~/Documents/Kaggle/Taxi/sampleSubmission.csv",stringsAsFactors=FALSE)
for(i in 1:320){
  #la tabella con tutte le posizioni di UNA riga
  actualPos<-positions(test[i,])
  #la lunghezza della polyline
  n<-nrow(actualPos)
  #se non abbiamo abbastanza punti, "fregacazzi", mettiamo l'ultima posizione disponibile
  if(n<4){
    sub[i,"LATITUDE"]<-actualPos[n,2]
    sub[i,"LONGITUDE"]<-actualPos[n,1]
  }
  else{
    pos1<-actualPos[n,]
    pos2<-actualPos[n-1,]
    pos3<-actualPos[n-2,]
    pos4<-actualPos[n-3,]
    #pos5<-actualPos[n-4,]
    #pos6<-actualPos[n-5,]
    diff1l<-pos1[2]-pos2[2]
    diff2l<-pos2[2]-pos3[2]
    diff3l<-pos3[2]-pos4[2]
    #diff4l<-pos4[2]-pos5[2]
    #diff5l<-pos5[2]-pos6[2]
    
    diff1r<-pos1[1]-pos2[1]
    diff2r<-pos2[1]-pos3[1]
    diff3r<-pos3[1]-pos4[1]
    #diff4r<-pos4[1]-pos5[1]
    #diff5r<-pos5[1]-pos6[1]
    
    #un fattore direttamente proporzionale alla lunghezza della polyline
    mulfactor <- n/0.9 - n
    sub[i,"LATITUDE"]<-pos1[2]+mulfactor*((diff1l+diff2l+diff3l)/3)
    sub[i,"LONGITUDE"]<-pos1[1]+mulfactor*((diff1r+diff2r+diff3r)/3)
  }
  
}
#così si scrive il risultato, nella wd
write.csv(sub,file = "interPOL2.csv",row.names = FALSE)

#POLTer magico: scrivi il numero della riga (del train soltanto!!! se volete, overloadate
#la specifica per averlo parametrico al dataset) e ti disegna una magica traiettoria numerata!
POLT_trip <- function(row){
  trace <- positions(train[row,])
  plot(trace)
  lines(trace)
  text(trace[,1], trace[,2], c(1:nrow(trace)))
}

#vi sto davvero facendo vedere come si usa? Dafuq?
POLT_trip(45)
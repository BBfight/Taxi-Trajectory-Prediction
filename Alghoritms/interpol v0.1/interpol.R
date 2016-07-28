### SETUP ###

# Working directory
setwd("C:/Users/Benedetto/Desktop/Data Mining/Taxi Trajectory Prediction/Data")

# libraries
source("../interpol v0.1/utils.r")
source("../interpol v0.1/evaluation_script.r")

# Import data set
# train <- read.csv(paste(getwd(),"/original data/train.csv",sep=""),stringsAsFactors=FALSE)
test <- read.csv(paste(getwd(),"/test.csv",sep=""),stringsAsFactors=FALSE)
answer <- TODO
sub <- read.csv(paste(getwd(),"/sampleSubmission.csv",sep=""),stringsAsFactors=FALSE)

### PREPROCESSING ###


### INTERPOL ALGORITHM ###

N <- nrow(myTest)
sub <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
for(i in 1:nrow(myTest)){
  #la tabella con tutte le posizioni di UNA riga
  actualPos<-positions(myTest[i,])
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

### POSTPROCESSING ###

#write.csv(sub,file = "interPOL.csv",row.names = FALSE)

### EVALUATION ###

eval <- evaluate(sub,myAnswers)
print(eval)

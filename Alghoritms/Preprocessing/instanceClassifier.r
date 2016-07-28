### SETUP ###

# Working directory
setwd("C:/Users/Benedetto/Desktop/Data Mining/Taxi Trajectory Prediction/Data")

# libraries
library(rpart)
library(class)
source("../stepClassification v0.22/utils.r")
source("../stepClassification v0.22/evaluation_script.r")
source("../stepClassification v0.22/stepClassification_utils.r")

# Import data set
#train_full <- read.csv(paste(getwd(),"/original data/train.csv",sep=""),stringsAsFactors=FALSE)
test <- read.csv(paste(getwd(),"/test.csv",sep=""),stringsAsFactors=FALSE)
# answer <- TODO
sub <- read.csv(paste(getwd(),"/sampleSubmission.csv",sep=""),stringsAsFactors=FALSE)


### PREPROCESSING ###
step <- 5
gap <- 3

trainS <- train
for(i in 1:nrow(train)){
  if(nrow(positions((train[i,]))) >= step){
  trainS[i,"POLYLINE"] <- posToJSON(pol_smooth(train[i,]))
  }
}

write.csv(trainS,file = "trainS_10k.csv")

train_ready <- disassembleData(trainS, step, gap) #i parametri da cambiare sono dentro alla funzione, ma possono essere facilmente portati qui

### TRAINING ###

# check the documentation for more fun!

### CLASSIFICATION ###

N <- nrow(test_kaggle)
prediction <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
for(i in 1:N){
  trip <- positions(test_kaggle[i,])
  
  if(nrow(trip)<=step){
    dest <- rev(tail(trip,1))
  } else {
    test_trip <- disassemble(trip, step, 1, start=0, end=1)
    #instance based classification goes here!
    sample <- knn(train_ready[,1:(step*2)],test_trip,train_ready[,(step*2+1)],k = 1)
    prediction_dest <- as.vector(sample)
    dest <- rev(extractPrediction(prediction_dest))
  }
  prediction[i,] <- c(test_kaggle[i,"TRIP_ID"], dest)
}


### EVALUATION ###

write.csv(prediction, file = "myprediction.csv", row.names = FALSE)

# TODO
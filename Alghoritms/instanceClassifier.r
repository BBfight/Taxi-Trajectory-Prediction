### SETUP ###

# Working directory
setwd("~/Polimi/Magistrale/Anno I/Semestre II/Data Mining and Text Mining/Project Taxi/stepClassification v0.2")

# libraries
library(rpart)
source("utils.r")
source("evaluation_script.r")
source("stepClassification_utils.r")

# Import data set
#train_full <- read.csv(paste(getwd(),"/original data/train.csv",sep=""),stringsAsFactors=FALSE)
train_mini <- read.csv(paste(getwd(),"/../original data/minitrain.csv",sep=""),stringsAsFactors=FALSE)
test <- read.csv(paste(getwd(),"/../original data/test.csv",sep=""),stringsAsFactors=FALSE)
train <- train_mini[1:500,]
# answer <- TODO
sub <- read.csv(paste(getwd(),"/../original data/sampleSubmission.csv",sep=""),stringsAsFactors=FALSE)

### PREPROCESSING ###
step <- 5
gap <- 3
train_ready <- disassembleData(train, step, gap) #i parametri da cambiare sono dentro alla funzione, ma possono essere facilmente portati qui

### TRAINING ###

# check the documentation for more fun!

### CLASSIFICATION ###

N <- nrow(test)
prediction <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
for(i in 1:N){
  trip <- positions(test[i,])
  
  if(nrow(trip)<=step){
    dest <- rev(tail(trip,1))
  } else {
    test_trip <- disassemble(trip, step)
    #instance based classification goes here!
    sample <- knn(train_ready[,1:(step*2)],test_trip,train_ready[,(step*2+1)],k = 1)
    prediction_dest <- as.vector(sample)
    dest <- rev(extractPrediction(prediction_dest))
  }
  prediction[i,] <- c(test[i,"TRIP_ID"], dest)
}


### EVALUATION ###

write.csv(prediction, file = "myprediction.csv", row.names = FALSE)

# TODO
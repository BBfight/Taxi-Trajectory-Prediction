### SETUP ###

# Working directory
setwd("~/Polimi/Magistrale/Anno I/Semestre II/Data Mining and Text Mining/Project Taxi/stepKNN - Snake Edition")

# libraries
library(class)
source("utils.r")
source("stepKNN_utils.r")

# Import data set
#train_full <- read.csv(paste(getwd(),"/../original data/train.csv",sep=""),stringsAsFactors=FALSE)
#train_mini <- read.csv(paste(getwd(),"/../mydata/minitrain.csv",sep=""),stringsAsFactors=FALSE)
#train_medium <- read.csv(paste(getwd(),"/../mydata/train_200k.csv",sep=""),stringsAsFactors=FALSE)

#test <- read.csv(paste(getwd(),"/../original data/test.csv",sep=""),stringsAsFactors=FALSE)
#test <- read.csv(paste(getwd(),"/../mydata/test_1k.csv",sep=""),stringsAsFactors=FALSE)
#test <- create_test(tail(train_medium,1000))


#answer <- read.csv(paste(getwd(),"/../mydata/answer_1k.csv",sep=""),stringsAsFactors=FALSE)
#answer <- create_answer(tail(train_medium,1000))

### PREPROCESSING ###
step <- 5
gap <- 3
train_ready <- disassembleData(train, step, gap, start=0.3, end=0.8)
#write.csv(train_ready, file = "train_ready_from30k_approx2.csv", row.names = FALSE)

### TRAINING ###

### CLASSIFICATION ###

N <- nrow(test)
prediction <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
for(i in 1:N){
  original_trip <- positions(test[i,])
  
  if(nrow(original_trip)<2){
    dest <- rev(tail(trip,1))
    prediction[i,] <- c(test[i,"TRIP_ID"], dest)
    next
  }
  
  trip <- square_transform(clean_error(original_trip,0.8))
  
  if(nrow(trip)<=step){
    dest <- rev(tail(trip,1))
  } else {
    test_trip <- disassemble(trip, step, gap, start=0.3, end=1)

#     KNN NON FUNZIONA CON I NOMINALI
#     dayPhase <- timeToPhase(test[i,]$TIME)
#     test_trip <- cbind(dayPhase,test_trip)
    
    prediction_dest <- knn(train_ready[,2:(step*2+1)],test_trip,train_ready[,(step*2+2)],k = 1)
    dest <- rev(extractPrediction(as.vector(prediction_dest)))
  }
  prediction[i,] <- c(test[i,"TRIP_ID"], dest)
}


### EVALUATION ###

evaluation <- evaluate(prediction,answer)
print(evaluation)

write.csv(prediction, file = "myprediction.csv", row.names = FALSE)

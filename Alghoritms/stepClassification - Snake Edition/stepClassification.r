### SETUP ###

# Working directory
setwd("~/Polimi/Magistrale/Anno I/Semestre II/Data Mining and Text Mining/Project Taxi/stepClassification - Snake Edition")

# libraries
library(rpart)
source("utils.r")
source("stepClassification_utils.r")

# Import data set
#train_full <- read.csv(paste(getwd(),"/../original data/train.csv",sep=""),stringsAsFactors=FALSE)
#train_mini <- read.csv(paste(getwd(),"/../original data/minitrain.csv",sep=""),stringsAsFactors=FALSE)
#train_medium <- read.csv(paste(getwd(),"/../mydata/train_200k.csv",sep=""),stringsAsFactors=FALSE)

#test <- read.csv(paste(getwd(),"/../original data/test.csv",sep=""),stringsAsFactors=FALSE)
#test <- read.csv(paste(getwd(),"/../mydata/test_tail2k.csv",sep=""),stringsAsFactors=FALSE)
#test <- create_test(tail(train_medium,1000))


#answer <- read.csv(paste(getwd(),"/../mydata/answer_tail2k.csv",sep=""),stringsAsFactors=FALSE)
#answer <- create_answer(tail(train_medium,1000))

### PREPROCESSING ###
step <- 5
gap <- 3
train_ready <- disassembleData(train, step, gap, start=0.3, end=0.8) #i parametri da cambiare sono dentro alla funzione, ma possono essere facilmente portati qui

### TRAINING ###

# check the documentation for more fun!
# THE NAMES OF THE COLUMNS MUST BE CHANGED BY HAND WHEN CHANGING THE STEP
fit <- rpart(destination ~ callType + dayWeek + dayPhase + LONG1 + LAT1 + LONG2 + LAT2 + LONG3 + LAT3 + LONG4 + LAT4 + LONG5 + LAT5
             , data=train_ready, method="class", minsplit=15, cp=0.0005)

### CLASSIFICATION ###

N <- nrow(test)
prediction <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
for(i in 1:N){
  original_trip <- positions(test[i,])
  
  if(nrow(original_trip)<2){
    dest <- rev(tail(original_trip,1))
    prediction[i,] <- c(test[i,"TRIP_ID"], dest)
    next
  }
  
  trip <- square_transform(clean_error(original_trip,0.8))
  
  if(nrow(trip)<=step){
    dest <- rev(tail(original_trip,1))
  } else {
    test_trip <- disassemble(trip, step, gap, start=0.3, end=1)
    
    dayPhase <- timeToPhase(test[i,]$TIME)
    test_trip <- cbind(dayPhase,test_trip)
    
    prediction_dest <- as.vector(predict(fit, test_trip, type="class"))
    dest <- rev(extractPrediction(prediction_dest))
  }
  prediction[i,] <- c(test[i,"TRIP_ID"], dest)
}


### EVALUATION ###

evaluation <- evaluate(prediction,answer)
print(evaluation)

write.csv(prediction, file = "myprediction.csv", row.names = FALSE)

# TODO
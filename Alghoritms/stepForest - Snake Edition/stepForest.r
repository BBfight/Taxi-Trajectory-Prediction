### SETUP ###

# Working directory
setwd("~/Polimi/Magistrale/Anno I/Semestre II/Data Mining and Text Mining/Project Taxi/stepForest - Snake Edition")

# libraries
library(randomForest)
# library(party)
source("utils.r")
source("stepForest_utils.r")

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

set.seed(117)

fit <- randomForest(destination ~ dayPhase + LONG1 + LAT1 + LONG2 + LAT2 + LONG3 + LAT3 + LONG4 + LAT4 + LONG5 + LAT5
             , data=train_ready, ntree=100)

# fit <- cForest(destination ~ dayPhase + LONG1 + LAT1 + LONG2 + LAT2 + LONG3 + LAT3 + LONG4 + LAT4 + LONG5 + LAT5
#              , data=train_ready, controls=cforest_unbiased(ntree=1000))


### CLASSIFICATION ###

doPrediction <- function(fit,test_trip){
  dayPhase <- timeToPhase(test[i,]$TIME)
  #     dayWeek <- timeToDay(test[i,]$TIME)
  #     callType <- test[i,]$CALL_TYPE
  
  test_trip <- cbind(dayPhase,test_trip)
  
  levels(test_trip$dayPhase) <- levels(train_ready$dayPhase)
  #     levels(test_trip$dayWeek) <- levels(train_ready$dayWeek)
  #     levels(test_trip$callType) <- levels(train_ready$callType)
  
  prediction_dest <- predict(fit,test_trip)
  dest <- rev(extractPrediction(as.vector(prediction_dest)))
  return(dest)
}

x<-0 # sia originale che snakizzato <= step
y <- 0 # originale > step ma snakizzato <= step
N <- nrow(test)
prediction <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
for(i in 1:N){
  original_trip <- positions(test[i,])
  
  if(nrow(original_trip)<2){
    dest <- rev(tail(original_trip,1))
    prediction[i,] <- c(test[i,"TRIP_ID"], dest)
    next
  }

  trip <- square_transform(clean_error(original_trip,0.8),edge_lat=0.00225, edge_long=0.003)
  
  if(nrow(trip)<=step){
    
    if(nrow(original_trip)>step){
      y <- y+1
      test_trip <- disassemble(original_trip, step, gap, start=0.3, end=1)
      dest <- doPrediction(fit,test_trip)
    } else {
      x <- x+1
      dest <- rev(tail(original_trip,1))
    }
    
  } else {
    test_trip <- disassemble(trip, step, gap, start=0.3, end=1)
    dest <- doPrediction(fit,test_trip)
  }
  prediction[i,] <- c(test[i,"TRIP_ID"], dest)
}


### EVALUATION ###

evaluation <- evaluate(prediction,answer)
print(evaluation)

write.csv(prediction, file = "myprediction.csv", row.names = FALSE)

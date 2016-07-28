### SETUP ###

# Working directory
setwd("C:/Users/Benedetto/Desktop/Data Mining/Taxi Trajectory Prediction/Data")

# libraries
library(readr)
library(stats)
library(randomForest)
source("../baggingForest/utils.r")
source("../baggingForest/baggingForest_utils.r")

# Import data set
train_full <- read.csv(paste(getwd(),"/train.csv",sep=""),stringsAsFactors=FALSE)

test <- read.csv(paste(getwd(),"/test.csv",sep=""),stringsAsFactors=FALSE)

### BAGGING ###
size <- 1000
chunks <- 50

train_list <- list()
for(i in 1:chunks){
  train_list[[i]] <- train_full[sample(nrow(train_full),size),]
}

### PREPROCESSING ###
step <- 5
gap <- 2

train_ready_list <- list()
for(i in 1:chunks){
  train_ready_list[[i]] <- disassembleData(train_list[[i]], step, gap,
                                           start=0.3, end=0.8)
  cat("disassembled train ",i,"/",chunks,"\n",sep ="")
}

### TRAINING ###
set.seed(7)

fit_forest_list <- list()
for(i in 1:chunks){
  fit_forest_list[[i]] <- randomForest(destination ~ LAT1 + LONG1 + LAT2 + LONG2 + 
                              LAT3 + LONG3 + LAT4 + LONG4 + LAT5 + LONG5 +
                              dayPhase + callType + dayWeek,
                              data = train_ready_list[[i]],ntree=100)
  cat("created forest ",i,"/",chunks,"\n",sep ="")
}

### CLASSIFICATION ###

N <- nrow(test)

prediction <- read.csv(paste(getwd(),"/wtf_why_does_it_work.csv",sep=""),
                      stringsAsFactors=FALSE)

### PREDICTION ###
for(i in 1:N){
  cat("predicted up to ",i,"/",N,"\n",sep="")

  original_trip <- positions(test[i,])
  
  if(nrow(original_trip)<2){
    next
  }
  
  trip <- square_transform(clean_error(original_trip,0.8))
  
  if(nrow(trip)<=step){
    next
  } else {
    dest_forest <- data.frame(LATITUDE = 1:chunks, LONGITUDE = 1:chunks)
    
    test_trip <- disassemble(trip, step, gap, start=0.3, end=1)

    dayPhase <- timeToPhase(test[i,]$TIME)
    dayWeek <- timeToDay(test[i,]$TIME)
    callType <- test[i,]$CALL_TYPE
    test_trip <- cbind(callType,dayWeek,dayPhase,test_trip)
    
    
    for(i in 1:chunks){
      
      levels(test_trip$callType) <- levels(train_ready_list[[i]]$callType)
      
      pred_forest <- predict(fit_forest_list[[i]], test_trip)
      dest_forest[i,] <- rev(extractPrediction(as.vector(pred_forest))) 
    }
    
    dest <- handlePredictions(dest_forest)
    prediction[i,] <- c(test[i,"TRIP_ID"],dest[1],dest[2])
    
    }
}

### WRITE PREDICTION ###
write.csv(prediction, file = "smashing.csv", row.names = FALSE)

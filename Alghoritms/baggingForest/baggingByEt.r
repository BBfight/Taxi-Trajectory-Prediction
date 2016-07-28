### SETUP ###

# Working directory
setwd("C:/Users/Benedetto/Desktop/Data Mining/Taxi Trajectory Prediction/Data")

# libraries
library(readr)
library(stats)
library(randomForest)
source("../baggingForest/utils.r")
source("../baggingForest/baggingForest_utils.r")

#USEFUL FUNCTIONS
extractOverallPrediction <- function(chunks, row){
  N <- length(chunks)
  dest_forest <- data.frame(LONGITUDE = 1:N,LATITUDE = 1:N,SSE = 1:N)
  for(i in 1:N){
    dest_forest[i,1:2] <- chunks[[i]][row,2:3] # DA VEDERE SE FUNZIA
  }
  for(i in 1:N){
    single_SSE <- 0
    for(j in 1:N){ #yeah I count also the distance from myself, that is 0.
      single_SSE <- single_SSE + HaversineDistance(as.numeric(dest_forest[i,1]),as.numeric(dest_forest[i,2]),as.numeric(dest_forest[j,1]),as.numeric(dest_forest[j,2]))
    }
    #print(single_SSE)
    dest_forest[i,3] <- single_SSE
  }
  newDest <- dest_forest[order(dest_forest[,3]),]
  return(newDest[1,1:2])
}



# Import data set
train_full <- read.csv(paste(getwd(),"/train.csv",sep=""),stringsAsFactors=FALSE)

test <- read.csv(paste(getwd(),"/test.csv",sep=""),stringsAsFactors=FALSE)

### BAGGING ###
size <- 15000
chunks <- 30
trees <- 125

### PREPROCESSING ###
step <- 5
gap <- 3

#train_list <- list()
chunk_predictions <- list()

for(i in 1:chunks){
  fit_forest <- NULL
  train_ready <- NULL
  
  set.seed(i*137)
  
  train_chunk <- train_full[sample(nrow(train_full),size),]
  
  cat("selected train chunk ",i,"/",chunks,"\n",sep ="")
  
  train_ready <- disassembleData(train_chunk, step, gap,
                                 start=0.2, end=0.9)
  cat("disassembled train chunk ",i,"/",chunks,"\n",sep ="")
  

  
  fit_forest <- randomForest(destination ~ LAT1 + LONG1 + LAT2 + LONG2 + 
                                         LAT3 + LONG3 + LAT4 + LONG4 + LAT5 + LONG5 +
                                         dayPhase + callType + dayWeek,
                                       data = train_ready,ntree=trees)
  cat("created forest ",i,"/",chunks,"\n",sep ="")
  
  N <- nrow(test)
  prediction <- read.csv(paste(getwd(),"/wtf_why_does_it_work.csv",sep=""),
                         stringsAsFactors=FALSE)
  
  ### PREDICTION ###
  for(j in 1:N){
    
    
    original_trip <- positions(test[j,])
    
    if(nrow(original_trip)<2){
      next
    }
    
    trip <- square_transform(clean_error(original_trip,0.8))
    
    if(nrow(trip)<=step){
      next
    } else {
      #dest_forest <- data.frame(LATITUDE = 1:chunks, LONGITUDE = 1:chunks)
      
      test_trip <- disassemble(trip, step, gap, start=0.3, end=1)
      
      dayPhase <- timeToPhase(test[j,]$TIME)
      dayWeek <- timeToDay(test[j,]$TIME)
      callType <- test[j,]$CALL_TYPE
      test_trip <- cbind(callType,dayWeek,dayPhase,test_trip)
      
      
      #for(i in 1:chunks){
        
        levels(test_trip$callType) <- levels(train_ready$callType)
        
        pred_forest <- predict(fit_forest, test_trip)
        dest <- rev(extractPrediction(as.vector(pred_forest)))
        #dest_forest[i,] <- rev(extractPrediction(as.vector(pred_forest))) 
      #}
      
      #dest <- handlePredictions(dest_forest)
      prediction[j,] <- c(test[j,"TRIP_ID"],dest[1],dest[2])
      
    }
  }
  cat("predicted chunk ",i,"/",chunks,"\n",sep ="")
  chunk_predictions[[i]] <- prediction
  
}

#NOW I EVALUATE THE CHUNKS TOGHETER!
final_prediction <- read.csv(paste(getwd(),"/wtf_why_does_it_work.csv",sep=""),
                             stringsAsFactors=FALSE)
N <- nrow(test)
for(i in 1:N){
  final_prediction[i,2:3] <- extractOverallPrediction(chunk_predictions,i)
}

### WRITE PREDICTION ###
write.csv(final_prediction, file = "smashing.csv", row.names = FALSE)

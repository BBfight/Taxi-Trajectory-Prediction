### SETUP ###

# Working directory
setwd("C:/Users/Benedetto/Desktop/Data Mining/Taxi Trajectory Prediction/Data")

# libraries
library(readr)
library(stats)
library(randomForest)
source("utils.r")
source("baggingForest_utils.r")

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

# Remove the last n digits from each point in positions, rounding the result
clusterize <- function(pos, approx=2) apply(pos, c(1,2), round, digits=6-approx)

# NON PARAMETRICA, ZERO SBATTI (ANCHE SE POI VERREBBE UTILE...)
takeFirst5 <- function(trip){
  first5 <- data.frame(LONG1 = 1,LAT1 = 1,LONG2 = 1,LAT2 = 1,LONG3 = 1,LAT3 = 1,LONG4 = 1,LAT4 = 1,LONG5 = 1,LAT5 = 1)
  for(i in 1:5){
    first5[1,2*i-1] <- trip[i,1]
    first5[1,2*i] <- trip[i,2]
  }
  return(first5)
}


# Import data set
train_full <- read.csv(paste(getwd(),"/train.csv",sep=""),stringsAsFactors=FALSE)

test <- read.csv(paste(getwd(),"/test.csv",sep=""),stringsAsFactors=FALSE)

### BAGGING ###
size <- 1000
chunks <- 3
compl <- 0
ms <- 15

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
  
  ready_trips <- list()
  N <- nrow(train_chunk)
  for(j in 1:N){
      original_trip <- positions(train_chunk[j,])
      
      if(nrow(original_trip)<2){
        next
      }
      
      trip <- square_transform(clean_error(original_trip,0.8),edge_lat=0.00225,edge_long=0.003)
      
      if(nrow(trip)>=6){
        first5 <- takeFirst5(trip)
        
        destination <- pointToJSON(clusterize(tail(trip,1),approx=2))
        
        dayPhase <- timeToPhase(train_chunk[j,]$TIME)
        
        ready_trips[[j]] <- cbind(dayPhase,first5,destination)
        
      }
      
      if(i%%5000==0){
        cat("processed up to ",j,"/",N,"\n",sep="")
      }
  }
  train_ready <- as.data.frame(do.call(rbind,ready_trips))
    
  rm(train_chunk)
    
  cat("disassembled train chunk ",i,"/",chunks,"\n",sep ="")
  
  ### 2 POINTS CLASSIFIER ###
  fit_2 <- rpart(destination ~ dayPhase + LAT1 + LONG1,data = train_ready,
                 cp=compl,minsplit=ms)
  
  ### 3 POINTS CLASSIFIER ###
  fit_3 <- rpart(destination ~ dayPhase + LAT1 + LAT2 + LONG1 + LONG2,
                 data = train_ready,cp=compl,minsplit=ms)
  
  ### 4 POINTS CLASSIFIER ###
  fit_4 <- rpart(destination ~ dayPhase + LAT1 + LAT2 + LAT3 + LONG1 + LONG2 + LONG3
                 ,data = train_ready,cp=compl,minsplit=ms)
  
  ### 5 POINTS CLASSIFIER ###
  fit_5 <- rpart(destination ~ dayPhase + LAT1 + LAT2 + LAT3 + LAT4 + LONG1 + LONG2 + LONG3 + LONG4
                 ,data = train_ready,cp=compl,minsplit=ms)
  
  ### 6 POINTS CLASSIFIER ###
  fit_6 <- rpart(destination ~ dayPhase + LAT1 + LAT2 + LAT3 + LAT4 + LAT5 + 
                 LONG1 + LONG2 + LONG3 + LONG4 + LONG5,
                 data = train_ready,cp=compl,minsplit=ms)
  
  cat("created forest ",i,"/",chunks,"\n",sep ="")
  
  N <- nrow(test)
  prediction <- data.frame(TRIP_ID = test$TRIP_ID, LATITUDE = rep(0,N), LONGITUDE = rep(0,N))
  
  ### PREDICTION ###
  for(j in 1:N){
    
    trip <- positions(test[j,])
    
    if(nrow(trip)<2){
      
      pos <- data.frame(dayPhase = timeToPhase(test[j,]$TIME), 
                        LAT1 = trip[1,2], LONG1 = trip[1,1])
      colnames(pos) <- c("dayPhase","LAT1","LONG1")
      pred <- predict(fit_2,pos)
      dest <- rev(extractPrediction(as.vector(pred)))
      prediction[j,] <- c(test[j,"TRIP_ID"],dest[1],dest[2])
      next
    }
    
    squared_trip <- rev(square_transform(clean_error(trip,0.8),
                                         edge_lat = 0.00225,edge_long = 0.003))
    cond <- nrow(squared_trip)
      
    if(cond==1){
      pos <- data.frame(dayPhase = timeToPhase(test[j,]$TIME), 
                        LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2])
      colnames(pos) <- c("dayPhase","LAT1","LONG1")
      pred <- predict(fit_2,pos)
      dest <- rev(extractPrediction(as.vector(pred)))
      prediction[j,] <- c(test[j,"TRIP_ID"],dest[1],dest[2])
      next
    }
    
    if(cond==2){
      pos <- data.frame(dayPhase = timeToPhase(test[j,]$TIME), 
                        LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                        LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2])
      colnames(pos) <- c("dayPhase","LAT1","LONG1","LAT2","LONG2")
      pred <- predict(fit_3,pos)
      dest <- rev(extractPrediction(as.vector(pred)))
      prediction[j,] <- c(test[j,"TRIP_ID"],dest[1],dest[2])
      next
    }
    
    if(cond==3){
      pos <- data.frame(dayPhase = timeToPhase(test[j,]$TIME), 
                        LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                        LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2],
                        LAT3 = squared_trip[3,1], LONG3 = squared_trip[3,2])
      colnames(pos) <- c("dayPhase","LAT1","LONG1","LAT2","LONG2",
                         "LAT3","LONG3")
      pred <- predict(fit_4,pos)
      dest <- rev(extractPrediction(as.vector(pred)))
      prediction[j,] <- c(test[j,"TRIP_ID"],dest[1],dest[2])
      next
    }
    
    if(cond==4){
      pos <- data.frame(dayPhase = timeToPhase(test[j,]$TIME), 
                        LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                        LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2],
                        LAT3 = squared_trip[3,1], LONG3 = squared_trip[3,2],
                        LAT4 = squared_trip[4,1], LONG4 = squared_trip[4,2])
      colnames(pos) <- c("dayPhase","LAT1","LONG1","LAT2","LONG2",
                         "LAT3","LONG3","LAT4","LONG4")
      pred <- predict(fit_5,pos)
      dest <- rev(extractPrediction(as.vector(pred)))
      prediction[j,] <- c(test[j,"TRIP_ID"],dest[1],dest[2])
      next
    }
    
    if(cond==5){
      pos <- data.frame(dayPhase = timeToPhase(test[j,]$TIME), 
                        LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                        LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2],
                        LAT3 = squared_trip[3,1], LONG3 = squared_trip[3,2],
                        LAT4 = squared_trip[4,1], LONG4 = squared_trip[4,2],
                        LAT5 = squared_trip[5,1], LONG5 = squared_trip[5,2])
      colnames(pos) <- c("dayPhase","LAT1","LONG1","LAT2","LONG2",
                         "LAT3","LONG3","LAT4","LONG4","LAT5","LONG5")
      pred <- predict(fit_6,pos)
      dest <- rev(extractPrediction(as.vector(pred)))
      prediction[j,] <- c(test[j,"TRIP_ID"],dest[1],dest[2])
      next
    }
    
    prediction[j,] <- c(test[j,"TRIP_ID"],0,0)
  }
  cat("predicted chunk ",i,"/",chunks,"\n",sep ="")
  chunk_predictions[[i]] <- prediction

}
    
#NOW I EVALUATE THE CHUNKS TOGHETER!
final_prediction <- data.frame(TRIP_ID = test$TRIP_ID, LATITUDE = rep(0,N), LONGITUDE = rep(0,N))

N <- nrow(test)
for(i in 1:N){
  final_prediction[i,2:3] <- extractOverallPrediction(chunk_predictions,i)
}

### WRITE PREDICTION ###
write.csv(final_prediction, file = "smashing.csv", row.names = FALSE)

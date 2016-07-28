### SETUP ###

# Working directory
setwd("C:/Users/Benedetto/Desktop/Data Mining/Taxi Trajectory Prediction/Data")

# libraries

library(rpart)
library(readr)
library(e1071)
source("../defaultPrediction/utils.r")
source("../defaultPrediction/defaultPrediction_utils.r")

# Import data set
train_full <- read.csv(paste(getwd(),"/train.csv",sep=""),stringsAsFactors=FALSE)
train <- train_full
train_full <- NULL

test <- read.csv(paste(getwd(),"/test.csv",sep=""),stringsAsFactors=FALSE)

train_positions_list <- list()
dayPhase_list <- list()
added <- 1

for(i in 1:nrow(train)){
  temp <- clean_error(positions(train[i,]),0.08)
  if(nrow(temp)>=6){
    dayPhase_list[added] <- timeToPhase(train[i,]$TIME)
    train_positions_list[[added]] <- temp
    added <- added + 1
  }
  if(i%%10000==0){
    cat("checked up to ",i,"/",nrow(train),", list size is ",added-1,"\n",sep="")
  }
}

N <- length(train_positions_list)
train <- NULL

### EXTRACT FIRST SIX POINTS ###
points <- data.frame(DAY_PHASE = 1:N,LAT1 = 1:N, LONG1 = 1:N,LAT2 = 1:N, LONG2 = 1:N,
                     LAT3 = 1:N, LONG3 = 1:N,LAT4 = 1:N, LONG4 = 1:N,
                     LAT5 = 1:N, LONG5 = 1:N,dest_lat = 1:N, dest_long = 1:N)

for(i in 1:N){
  temp <- rev(train_positions_list[[i]][1:6,])
  points[i,] <- c(dayPhase_list[i],temp[1,1],temp[1,2],temp[2,1],temp[2,2],temp[3,1],temp[3,2],
                  temp[4,1],temp[4,2],temp[5,1],temp[5,2],temp[6,1],temp[6,2])
  if(i%%10000==0){
    cat("added points of element ",i,"/",N,"\n",sep="")
  }
}

### TRAINING ###

compl <- 0
ms <- 20

### 2 POINTS CLASSIFIER ###
fit_lat_2 <- rpart(dest_lat ~ LAT1 + DAY_PHASE,data = points,cp=compl,minsplit=ms)
fit_long_2 <- rpart(dest_long ~ LONG1 + DAY_PHASE,data = points,cp=compl,minsplit=ms)

### 3 POINTS CLASSIFIER ###
fit_lat_3 <- rpart(dest_lat ~ LAT1 + LAT2 + DAY_PHASE,data = points,cp=compl,minsplit=ms)
fit_long_3 <-rpart(dest_long ~ LONG1 + LONG2 + DAY_PHASE,data = points,cp=compl,minsplit=ms)

### 4 POINTS CLASSIFIER ###
fit_lat_4 <- rpart(dest_lat ~ LAT1 + LAT2 + LAT3 + DAY_PHASE,data = points,cp=compl,minsplit=ms)
fit_long_4 <- rpart(dest_long ~ LONG1 + LONG2 + LONG3 + DAY_PHASE,data = points,cp=compl,minsplit=ms)

### 5 POINTS CLASSIFIER ###
fit_lat_5 <- rpart(dest_lat ~ LAT1 + LAT2 + LAT3 + LAT4
                   + DAY_PHASE,data = points,cp=compl,minsplit=ms)
fit_long_5 <-rpart(dest_long ~ LONG1 + LONG2 + LONG3 + LONG4
                   + DAY_PHASE,data = points,cp=compl,minsplit=ms)

### 6 POINTS CLASSIFIER ###
fit_lat_6 <- rpart(dest_lat ~ LAT1 + LAT2 + LAT3 + LAT4 + LAT5
                   + DAY_PHASE,data = points,cp=compl,minsplit=ms)
fit_long_6 <- rpart(dest_long ~ LONG1 + LONG2 + LONG3 + LONG4 + LONG5
                   + DAY_PHASE,data = points,cp=compl,minsplit=ms)

### CLASSIFICATION ###

step <- 5

N <- nrow(test)
prediction <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
for(i in 1:N){
  trip <- positions(test[i,])
  cond <- nrow(trip)
  
  if(cond<2){
    
    pos <- data.frame(DAY_PHASE = timeToPhase(test[i,]$TIME), 
                      LAT1 = trip[1,2], LONG1 = trip[1,1])
    dest_lat <- predict(fit_lat_2,pos)
    dest_long <- predict(fit_long_2,pos)
    prediction[i,] <- c(test[i,"TRIP_ID"], dest_lat,dest_long)
    
  }else{
    
    squared_trip <- rev(square_transform(clean_error(trip,0.8),edge_lat=0.00225,edge_long=0.003))
    cond <- nrow(squared_trip)

    if(cond<=step){
      
      if(cond==1){
        pos <- data.frame(DAY_PHASE = timeToPhase(test[i,]$TIME), 
                          LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2])
        colnames(pos) <- c("DAY_PHASE","LAT1","LONG1")
        dest_lat <- predict(fit_lat_2,pos)
        dest_long <- predict(fit_long_2,pos)
        prediction[i,] <- c(test[i,"TRIP_ID"], dest_lat,dest_long)
      }
      
      if(cond==2){
        pos <- data.frame(DAY_PHASE = timeToPhase(test[i,]$TIME), 
                          LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                          LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2])
        colnames(pos) <- c("DAY_PHASE","LAT1","LONG1","LAT2","LONG2")
        dest_lat <- predict(fit_lat_3,pos)
        dest_long <- predict(fit_long_3,pos)
        prediction[i,] <- c(test[i,"TRIP_ID"], dest_lat,dest_long)
      }
      
      if(cond==3){
        pos <- data.frame(DAY_PHASE = timeToPhase(test[i,]$TIME), 
                          LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                          LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2],
                          LAT3 = squared_trip[3,1], LONG3 = squared_trip[3,2])
        colnames(pos) <- c("DAY_PHASE","LAT1","LONG1","LAT2","LONG2",
                           "LAT3","LONG3")
        dest_lat <- predict(fit_lat_4,pos)
        dest_long <- predict(fit_long_4,pos)
        prediction[i,] <- c(test[i,"TRIP_ID"], dest_lat,dest_long)
      }
      
      if(cond==4){
        pos <- data.frame(DAY_PHASE = timeToPhase(test[i,]$TIME), 
                          LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                          LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2],
                          LAT3 = squared_trip[3,1], LONG3 = squared_trip[3,2],
                          LAT4 = squared_trip[4,1], LONG4 = squared_trip[4,2])
        colnames(pos) <- c("DAY_PHASE","LAT1","LONG1","LAT2","LONG2",
                           "LAT3","LONG3","LAT4","LONG4")
        dest_lat <- predict(fit_lat_5,pos)
        dest_long <- predict(fit_long_5,pos)
        prediction[i,] <- c(test[i,"TRIP_ID"], dest_lat,dest_long)
      }
      
      if(cond==5){
        pos <- data.frame(DAY_PHASE = timeToPhase(test[i,]$TIME), 
                          LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                          LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2],
                          LAT3 = squared_trip[3,1], LONG3 = squared_trip[3,2],
                          LAT4 = squared_trip[4,1], LONG4 = squared_trip[4,2],
                          LAT5 = squared_trip[5,1], LONG5 = squared_trip[5,2])
        colnames(pos) <- c("DAY_PHASE","LAT1","LONG1","LAT2","LONG2",
                           "LAT3","LONG3","LAT4","LONG4","LAT5","LONG5")
        dest_lat <- predict(fit_lat_6,pos)
        dest_long <- predict(fit_long_6,pos)
        prediction[i,] <- c(test[i,"TRIP_ID"], dest_lat,dest_long)
      }
    }else{
      prediction[i,] <- c(test[i,"TRIP_ID"],0,0)
    }
  }
  
}

### WRITE PREDICTION ###
write.csv(prediction, file = "default_prediction.csv", row.names = FALSE)

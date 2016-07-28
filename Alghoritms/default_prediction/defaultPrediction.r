### SETUP ###

# Working directory
### SET YOUR OWN ###

# libraries

library(rpart)
library(readr)
library(e1071)
source("utils.r")

# Import data set
train_full <- read.csv(paste(getwd(),"/train.csv",sep=""),stringsAsFactors=FALSE)
train <- train_full[1:1000,]
train_full <- NULL

test <- read.csv(paste(getwd(),"/test.csv",sep=""),stringsAsFactors=FALSE)


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


ready_trips <- list()
N <- nrow(train)
for(i in 1:N){
  original_trip <- positions(train[i,])
  
  if(nrow(original_trip)<2){
    next
  }
  
  trip <- square_transform(clean_error(original_trip,0.8),edge_lat=0.00225,edge_long=0.003)

  if(nrow(trip)>=6){
    first5 <- takeFirst5(trip)
    
    destination <- pointToJSON(clusterize(tail(trip,1),approx=2))
    
    dayPhase <- timeToPhase(train[i,]$TIME)
    
    ready_trips[[i]] <- cbind(dayPhase,first5,destination)
    
  }

  if(i%%10000==0){
    cat("processed up to ",i,"/",N,"\n",sep="")
  }
}
train_ready <- as.data.frame(do.call(rbind,ready_trips))

rm(train)

### TRAINING ###

compl <- 0.0005
ms <- 20

### 2 POINTS CLASSIFIER ###
fit_2 <- rpart(destination ~ dayPhase + LAT1 + LONG1,data = train_ready,cp=compl,minsplit=ms)

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
fit_6 <- rpart(destination ~ dayPhase + LAT1 + LAT2 + LAT3 + LAT4 + LAT5 +LONG1 + LONG2 + LONG3 + 
                     LONG4 + LONG5,data = train_ready,cp=compl,minsplit=ms)

### CLASSIFICATION ###

step <- 5

N <- nrow(test)
prediction <- data.frame(TRIP_ID = test$TRIP_ID, LATITUDE = rep(0,N), LONGITUDE = rep(0,N))
for(i in 1:N){
  trip <- positions(test[i,])
  cond <- nrow(trip)
  
  if(cond<2){
    
    pos <- data.frame(dayPhase = timeToPhase(test[i,]$TIME), 
                      LAT1 = trip[1,2], LONG1 = trip[1,1])
    dest <- predict(fit_2,pos,type="class")
    dest <- rev(t(as.data.frame(fromJSON(as.vector(dest)))))
    prediction[i,2:3] <- dest
    next 
  }
    
  squared_trip <- rev(square_transform(clean_error(trip,0.8),edge_lat=0.00225,edge_long=0.003))
  cond <- nrow(squared_trip)

    
  if(cond==1){
    pos <- data.frame(dayPhase = timeToPhase(test[i,]$TIME), 
                      LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2])
    colnames(pos) <- c("dayPhase","LAT1","LONG1")
    dest <- predict(fit_2,pos,type="class")
    dest <- rev(t(as.data.frame(fromJSON(as.vector(dest)))))
    prediction[i,2:3] <- dest
    next
  }
  
  if(cond==2){
    pos <- data.frame(dayPhase = timeToPhase(test[i,]$TIME), 
                      LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                      LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2])
    colnames(pos) <- c("dayPhase","LAT1","LONG1","LAT2","LONG2")
    dest <- predict(fit_3,pos,type="class")
    dest <- rev(t(as.data.frame(fromJSON(as.vector(dest)))))
    prediction[i,2:3] <- dest
    next
  }
  
  if(cond==3){
    pos <- data.frame(dayPhase = timeToPhase(test[i,]$TIME), 
                      LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                      LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2],
                      LAT3 = squared_trip[3,1], LONG3 = squared_trip[3,2])
    colnames(pos) <- c("dayPhase","LAT1","LONG1","LAT2","LONG2",
                       "LAT3","LONG3")
    dest <- predict(fit_4,pos,type="class")
    dest <- rev(t(as.data.frame(fromJSON(as.vector(dest)))))
    prediction[i,2:3] <- dest
    next
  }
  
  if(cond==4){
    pos <- data.frame(dayPhase = timeToPhase(test[i,]$TIME), 
                      LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                      LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2],
                      LAT3 = squared_trip[3,1], LONG3 = squared_trip[3,2],
                      LAT4 = squared_trip[4,1], LONG4 = squared_trip[4,2])
    colnames(pos) <- c("dayPhase","LAT1","LONG1","LAT2","LONG2",
                       "LAT3","LONG3","LAT4","LONG4")
    dest <- predict(fit_5,pos,type="class")
    dest <- rev(t(as.data.frame(fromJSON(as.vector(dest)))))
    prediction[i,2:3] <- dest
    next
  }
  
  if(cond==5){
    pos <- data.frame(dayPhase = timeToPhase(test[i,]$TIME), 
                      LAT1 = squared_trip[1,1], LONG1 = squared_trip[1,2],
                      LAT2 = squared_trip[2,1], LONG2 = squared_trip[2,2],
                      LAT3 = squared_trip[3,1], LONG3 = squared_trip[3,2],
                      LAT4 = squared_trip[4,1], LONG4 = squared_trip[4,2],
                      LAT5 = squared_trip[5,1], LONG5 = squared_trip[5,2])
    colnames(pos) <- c("dayPhase","LAT1","LONG1","LAT2","LONG2",
                       "LAT3","LONG3","LAT4","LONG4","LAT5","LONG5")
    dest <- predict(fit_6,pos,type="class")
    dest <- rev(t(as.data.frame(fromJSON(as.vector(dest)))))
    prediction[i,2:3] <- dest
    next
  }
}

### WRITE PREDICTION ###
write.csv(prediction, file = "default_prediction.csv", row.names = FALSE)

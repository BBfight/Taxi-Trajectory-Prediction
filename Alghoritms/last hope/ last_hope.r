### SETUP ###

# Working directory
setwd("~/Polimi/Magistrale/Anno I/Semestre II/Data Mining and Text Mining/Project Taxi/stepForest - Snake Edition")

# libraries
library(randomForest)
# library(party)
source("utils.r")
source("stepForest_utils.r")

#load the test however you prefer, but remember to load it
test <- read.csv(paste(getwd(),"/../original data/test.csv",sep=""),stringsAsFactors=FALSE)

#FIRST STEP: loading the raw train data into a list
train_list <- list()
for(i in 1:11){
  train_list[[i]]<- read.csv(paste("train_cluster_",i,".csv",sep=""),stringsAsFactors=FALSE)
  
}
print("Read every train cluster")

#SECOND STEP: disassembling the raw train data and putting it into another list
step <- 5
gap <- 3

train_ready_list <- list()
for(i in 1:11){
  
  if(nrow(train_list[[i]])<10000){
    #three cases where I don't feel like using the same parameters as for the others
    train_ready_list[[i]] <- disassembleData(train_list[[i]], step, 2, start=0.2, end=0.9)
  }else {
    #normal gap of three.
    train_ready_list[[i]] <- disassembleData(train_list[[i]], step, gap, start=0.2, end=0.9)
  }
  print(paste0("Disassembled cluster ", i))
}

#train_list is useless now and it takes 1.8 gb of data. U KNOW WHAT I MEAN? :@
train_list <- NULL
#R.I.P. train_list

#THIRD STEP: the longest one: creating the forest for every cluster. It's going to be heavy.
set.seed(137)
fit_list <- list()


for(i in 1:11){
  fit_list[[i]] <- randomForest(destination ~ callType + dayWeek + dayPhase + LONG1 + LAT1 + LONG2 + LAT2 + LONG3 + LAT3 + LONG4 + LAT4 + LONG5 + LAT5
                      , data=train_ready_list[[i]], ntree=1000)
  print(paste0("Created forest ", i))
}

### CLASSIFICATION ###

#a mystical function made by POL. We trust in POL as much as Emilio trusts in the bible
doPrediction <- function(fit,test_trip,i,whatCluster){
  #print(i)
  dayPhase <- timeToPhase(test[i,]$TIME)
  dayWeek <- timeToDay(test[i,]$TIME)
  callType <- test[i,]$CALL_TYPE
  
  test_trip <- cbind(callType,dayWeek,dayPhase,test_trip)
  #print(test_trip$dayPhase)
  #levels(test_trip$dayPhase) <- levels(train_ready_list[[whatCluster]]$dayPhase)
  #levels(test_trip$dayWeek) <- levels(train_ready_list[[1]]$dayWeek)
  levels(test_trip$callType) <- levels(train_ready_list[[1]]$callType)
  
  prediction_dest <- predict(fit,test_trip)
  dest <- rev(extractPrediction(as.vector(prediction_dest)))
  return(dest)
}

#my very reasonable function to get the cluster to which I belong. It's nothing like those
#magical POL's functions

#it needs a data frame to operate though:
important_clusters <- read.csv("important_clusters.csv",stringsAsFactors=FALSE)
important_clusters$X <- NULL
belongsToWhichCluster <- function (point,dist=2.2){
  for(i in 1:nrow(important_clusters)){
    actual_cluster <- important_clusters[i,]
    if(HaversineDistance(as.numeric(actual_cluster[3]),as.numeric(actual_cluster[2]),as.numeric(point[2]),as.numeric(point[1]))< dist){
      return(i)
    }
  }
  #default choice
  return(1)
}


x<-0 # sia originale che snakizzato <= step
y <- 0 # originale > step ma snakizzato <= step

N <- nrow(test)
#here I take the sample prediction maed by looking at the clusters and use it as the default
#for the too short trips (which in my opinion are way too many)
#prediction <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
prediction <- read.csv("default_prediction.csv",stringsAsFactors=FALSE)
for(i in 1:N){
  original_trip <- positions(test[i,])
  
  if(nrow(original_trip)<2){
    #we already have it written in out default_prediction, well, we should have..
    
    #dest <- rev(tail(original_trip,1))
    #prediction[i,] <- c(test[i,"TRIP_ID"], dest)
    next
  }
  
  trip <- square_transform(clean_error(original_trip,0.8),edge_lat=0.00225, edge_long=0.003)
  
  if(nrow(trip)<=step){
    
    if(nrow(original_trip)>step){
      y <- y+1
      #here I find out its cluster
      clus <- belongsToWhichCluster(original_trip[1,],2.1)
      print(clus)
      
      test_trip <- disassemble(original_trip, step, gap, start=0.3, end=1)
      dest <- doPrediction(fit_list[[clus]],test_trip,i,clus)
    } else {
      x <- x+1
      #again, we should already have a reasonable result
      #dest <- rev(tail(original_trip,1))
    }
    
  } else {
    clus <- belongsToWhichCluster(original_trip[1,],2.1)
    print(clus)
    
    test_trip <- disassemble(trip, step, gap, start=0.3, end=1)
    dest <- doPrediction(fit_list[[clus]],test_trip,i,clus)
  }
  prediction[i,] <- c(test[i,"TRIP_ID"], dest)
}

write.csv(prediction, file = "last_hope_prediction.csv", row.names = FALSE)

source("utils.r")
source("stepForest_utils.r")

#load the test however you prefer, but remember to load it
test <- read.csv("test.csv",stringsAsFactors=FALSE)

### CLASSIFICATION ###

step <- 5

x<-0 # sia originale che snakizzato <= step
N <- nrow(test)
whatIWant <- list()

#prediction <- read.csv("wtf_why_does_it_work.csv",stringsAsFactors=FALSE)
for(i in 1:N){
  #print(i)
  original_trip <- positions(test[i,])
  
  if(nrow(original_trip)<2){
    x <- x + 1
    whatIWant[[x]]<- i
    next
  }
  
  trip <- square_transform(clean_error(original_trip,0.8),edge_lat=0.00225, edge_long=0.003)
  #trip <- square_transform(clean_error(original_trip,0.8),edge_lat=0.0045, edge_long=0.006)
  if(nrow(trip)<=step){
    x <- x+1
    whatIWant[[x]]<- i

    
  } else {
    #nothing to do
  }
}

toBeConsidered <- as.data.frame(do.call(rbind,whatIWant))
#write.csv(toBeConsidered,"short_test.csv")

#usage: toBeConsidered$V1 gives the vector

#here u put your prediction
prediction <- read.csv("2_63++.csv",stringsAsFactors=FALSE)
default <- read.csv("2_137.csv",stringsAsFactors=FALSE)

newPrediction <- default
newPrediction[toBeConsidered$V1,] <- prediction[toBeConsidered$V1,]

#BRUTE FORCE TO BE FIXED
newPrediction[71,2:3]<-rev(positions(test[71,])[1,])

newPrediction[126,2:3]<-rev(positions(test[126,])[1,])

write.csv(newPrediction, file = "like_it_will_really_workerino.csv", row.names = FALSE)


#write the kaggle result, get the estimated actual weight of error
evaluate_result <- function(result){
  benchmark <- 3.66994
  wB<-(320-84)/320
  wF<- 84/320
  #result = benchmark * wB + myWeight * wF
  myWeight <- (result - benchmark *wB)/wF
  return(myWeight)
}

#INTERPOL

library(ppls) #for normalize.vector
prediction <- read.csv("2_63++.csv",stringsAsFactors=FALSE)
max_len <- 2
min_len <- 0.2
for(i in toBeConsidered$V1){
  trip <- positions(test[i,])
  n<- nrow(trip)
  if(n<2){
    prediction[i,2:3] <- trip[1,2:1]
    #last position. Modificabile, tipo downtown.
  }else{
   last <- tail(trip,1)[1,2:1] #last point
   first <- trip[1,2:1] #first point
   
   dist <- HaversineDistance(as.numeric(last[1]),as.numeric(last[2]),as.numeric(first[1]),as.numeric(first[2]))
   print(paste0("distance is ",dist))
   
   dir_lat <- 0
   dir_long<- 0
   if(n<=4){
     dir_lat <- as.numeric(last[1] - first[1])
     dir_long <- as.numeric(last[2] - first[2])
   }else {
     last4<- tail(trip,4)[1,2:1]
     dir_lat <- as.numeric(last[1] - last4[1])
     dir_long <- as.numeric(last[2] - last4[2])
   }
   dir <- normalize.vector(c(dir_lat,dir_long))
   print(paste0("normalized vector is ",dir[1]," ",dir[2]))
   
   #quanti chilometri devo muovermi ancora
   #al massimo 1, ma minimo min_len
   mul_factor <- min(max(max_len-dist,min_len),1)
   print(paste0("I keep moving for ",mul_factor," km"))
   
   #cool, huh? 0.009 is 1 km
   dest <- dir*mul_factor*0.009 + last
   print(paste0("destination is ",dest[1]," ",dest[2]))
   prediction[i,2:3] <- dest

  }
}

#BRUTE FORCE TO BE FIXED
prediction[71,2:3]<-rev(positions(test[71,])[1,])
prediction[126,2:3]<-rev(positions(test[126,])[1,])

write.csv(prediction,file="back_to_interpol.csv",row.names=FALSE)

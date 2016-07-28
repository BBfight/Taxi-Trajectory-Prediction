
source("utils.r")
source("stepForest_utils.r")

#load the test however you prefer, but remember to load it
test <- read.csv("test.csv",stringsAsFactors=FALSE)

### CLASSIFICATION ###

step <- 5

x<-0 # sia originale che snakizzato <= step
N <- nrow(test)
whatIWant <- list()

prediction <- read.csv("wtf_why_does_it_work.csv",stringsAsFactors=FALSE)
for(i in 1:N){
  #print(i)
  original_trip <- positions(test[i,])
  
  if(nrow(original_trip)<2){
    x <- x + 1
    whatIWant[[x]]<- i
    next
  }
  
  trip <- square_transform(clean_error(original_trip,0.8),edge_lat=0.00225, edge_long=0.003)
  
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
prediction <- read.csv("wtf_why_does_it_work.csv",stringsAsFactors=FALSE)
default <- read.csv("sampleSubmission.csv",stringsAsFactors=FALSE)

newPrediction <- default
newPrediction[toBeConsidered$V1,] <- prediction[toBeConsidered$V1,]

write.csv(newPrediction, file = "my_short_prediction.csv", row.names = FALSE)


#write the kaggle result, get the estimated actual weight of error
evaluate_result <- function(result){
  benchmark <- 3.66994
  wB<-(320-84)/320
  wF<- 84/320
  #result = benchmark * wB + myWeight * wF
  myWeight <- (result - benchmark *wB)/wF
  return(myWeight)
}

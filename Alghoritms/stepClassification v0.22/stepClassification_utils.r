source("utils.r")

# Remove the last n digits from each point in positions, rounding the result
clusterize <- function(pos, approx=2) apply(pos, c(1,2), round, digits=6-approx)

# Given a trip, it disassemble it in his last subparts.
# Note that this function doesn't do any check on the parameters and disassemble feasibility.
# @trip: the trip to disassemble
# @step: the number of points in each group
# @gap: the number of points that separate a group from the next one
# @start: the percentage of the trip from which start the disassemble
# @end: the percentage of the trip to which end the disassemble
disassemble <- function(trip, step=5, gap=3, start=0.4, end=0.8){  
  N <- nrow(trip)
  
  
  end <- max(ceiling(end*N) - step, N-step)
  start <- max(min(ceiling(start*N), end),1)
#   print(c(start,end,gap,N))
  loop <- seq(start, end, by=gap)
  
  subtrips <- list()
  for(i in loop){
    entry <- c(1:(2*step))
    for(j in 1:step){
      entry[2*j-1] <- trip[i+j,1]
      entry[2*j] <- trip[i+j,2]
    }
    subtrips[[i]] <- entry
  }
  disassembled <- as.data.frame(do.call(rbind,subtrips))
  colnames(disassembled) <- c(paste(c("LONG","LAT"),rbind(1:step,1:step),sep="")) # cose a caso
  return(disassembled)
}

# Given a training set, decompose each trip and append the clusterized destination to each row.
# The clusterization is done by dropping the last digits and round the result.
# @data: the original training set to be processed
# @step: the length of the subtrip
# @gap: the number of points between each subtrip
disassembleData <- function(data, step=5, gap=3, start=0.4, end=0.8){
  ready_trips <- list()
  
  for(i in 1:nrow(data)){
    trip <- positions(data[i,])
    
    if(nrow(trip)>step){
      disassembled <- disassemble(trip, step, gap, start, end)
      
      destination <- pointToJSON(clusterize(tail(trip,1),approx=2))
      
      ready_trips[[i]] <- cbind(disassembled,destination)
    }
  }
  
  ready_train <- as.data.frame(do.call(rbind,ready_trips))
  return(ready_train)
}

# Given a set of prediction, extract the final (latitude, longitude) prediction
# @prediction: set of JSON arrays representing the predictions
extractPrediction <- function(prediction){
  numeric <- as.data.frame(do.call(rbind, lapply(prediction,fromJSON)))
  
  weights <- 1:nrow(numeric)
  final <- apply(numeric,2,function(x) sum(weights * x) / sum(weights))
  
  return(final)
}


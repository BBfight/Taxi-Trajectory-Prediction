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
  disassembled <- as.matrix(do.call(rbind,subtrips))
  return(disassembled)
}

# Given a training set, decompose each trip and append the destination to each row.
# During the decomposition, points that are closer than a treshold are drop
# @data: the original training set to be processed
# @step: the length of the subtrip
# @gap: the number of points between each subtrip
# @start & end: percentage of each trip to be considered
# @clean: if two two subsequent points are closer than this treshold, the first one is drop
disassembleData <- function(data, step=5, gap=3, start=0.4, end=0.8, clean=0.1){
  ready_trips <- list()
  
  for(i in 1:nrow(data)){
    trip <- clean_proximity(positions(data[i,]),clean)
    
    if(nrow(trip)>step){
      disassembled <- disassemble(trip, step, gap, start, end)
      
      dest_long <- tail(trip,1)[1,1]
      dest_lat <- tail(trip,1)[1,2]
        
      ready_trips[[i]] <- cbind(disassembled,dest_long,dest_lat)
    }
  }
  
  ready_train <- as.data.frame(do.call(rbind,ready_trips))
  colnames(ready_train) <- c(paste(c("LONG","LAT"),rbind(1:step,1:step),sep=""),"dest_long","dest_lat") # cose a caso
  return(ready_train)
}

# Weightened average
avg <- function(x,weights) sum(weights * x) / sum(weights)


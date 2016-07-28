library(readr)
library(rjson)
source("evaluation_script.r")

### NOMINAL/NUMERICAL CONVERSION ###


# Extract a list of positions from the polyline of a row
# @row: the row of the table to be processed
# @colname: the name of the column containing the polyline
positions <- function(row,colname="POLYLINE") as.data.frame(do.call(rbind, fromJSON(row[,colname])))

#Convert a single point to json format
pointToJSON <- function(point) toJSON(as.list(point))

# Inverse of function "positions": convert a dataframe to json
# @pos: positions obtained with function positions
posToJSON <- function(pos){
  x<-rbind(pos[,1],pos[,2])

  json<-pointToJSON(x[,1])
  
  if(ncol(x)>1){
    for(i in 2:ncol(x)){
      json<-paste(json,pointToJSON(x[,i]),sep=",")
    }
  }
  
  json<-paste("[",json,"]",sep="")
  return(json)
}

### GENERAL PREPROCESSING ###

# Remove all the points from a polyline that are closer then a treshold from his subsequent
# @trip: positions dataframe
# @treshold: minimum treshold
clean_proximity <- function(trip,treshold) {
  i <- 1:(nrow(trip)-1)
  return(trip[HaversineDistance(trip[i,2],trip[i,1],trip[i+1,2],trip[i+1,1])>treshold,])
}


### RESULTS EVALUATION ###

# Evaluate the mean haversine distance from the two dataset
evaluate<-function(submission,answers)
{
  lat_sub<-submission[,2]
  lon_sub<-submission[,3]
  
  lat_real<-answers[,2]
  lon_real<-answers[,3]
  
  return (meanHaversineDistance(lat_sub,lon_sub,lat_real,lon_real))
}

# Create a testing dataset from the training set
# @data: the training dataset
create_test <- function(data){
  rows <- nrow(data)
  
  # percentages of trip to be removed  
  perc <- runif(rows, 0, 0.75)
  
  test <- data.frame(TRIP_ID=data$TRIP_ID, POLYLINE=1:rows)
  empty_rows <- c()
  for(i in 1:rows){
    trip <- positions(data[i,])
    length <- nrow(trip)
    
    if(length>0){
      n_removed <- floor(length * perc[i])
      trip_new <- trip[1:(length-n_removed),]
      test[i,"POLYLINE"] <- posToJSON(trip_new)
    } else{
      empty_rows <- append(empty_rows,c(i))
    }
  }
  
  return(test[-empty_rows,])
} 

# Create a dataset containing the destinations of the given set
create_answer <- function(data){
  rows <- nrow(data)
  
  answers <- data.frame(1:rows, 1:rows, 1:rows)
  empty_rows <- c()
  for(i in 1:rows){
    trip <- positions(data[i,])
    
    if(nrow(trip)>0){
      destination <- tail(positions(data[i,]),1)
      answers[i,] <- c(data[i,"TRIP_ID"], destination[1,2], destination[1,1])
    } else {
      empty_rows <- append(empty_rows,c(i))
    }
  }
  
  colnames(answers) <- c("TRIP_ID","LATITUDE","LONGITUDE")
  return(answers[-empty_rows,])
}

### VISUALIZATION ###

# Plot the given polyline
# @trip: positions dataframe 
plot_trip <- function(trip){
  plot(trip)
  lines(trip)
  text(trip[,1], trip[,2], c(1:nrow(trip)))
}

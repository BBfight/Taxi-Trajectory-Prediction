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

# Convert a UNIX timestamp in a string representing the phase of the day
timeToPhase <- function(time){
  hour <- as.POSIXlt(time, origin="1970-01-01", tz="WET")$hour
  
  if(hour<6) return(1)
  if(hour<11) return(1)
  if(hour<14) return(2)
  if(hour<19) return(3)
  if(hour<23) return(4)
  else return(5)
}

# Convert a UNIX timestamp in a string representing the day of the week
timeToDay <- function(time) weekdays(as.POSIXlt(time, origin="1970-01-01", tz="WET"))

### GENERAL PREPROCESSING ###

# Remove all the points from a polyline that are closer then a treshold from his subsequent
# @trip: positions dataframe
# @treshold: minimum treshold
clean_proximity <- function(trip,treshold){
  i <- 1:(nrow(trip)-1)
  selected <- HaversineDistance(trip[i,2],trip[i,1],trip[i+1,2],trip[i+1,1]) > treshold
  return(trip[c(selected,TRUE),])
}

# Remove all the points from a polyline that are more distant then a treshold from his subsequent
# @trip: positions dataframe
# @treshold: maximum treshold
clean_error <- function(trip,treshold){
  N <- nrow(trip)-1
  selected <- rep(TRUE,N)
  
  for(i in 1:N){
    if(selected[i] && HaversineDistance(trip[i,2],trip[i,1],trip[i+1,2],trip[i+1,1]) > treshold){
      for(j in (i+1):N){
        if(HaversineDistance(trip[i,2],trip[i,1],trip[j,2],trip[j,1]) > treshold){
          selected[j] <- FALSE
        }
      }
    }
  }
  return(trip[selected,])
}

# Transform a trip in sequence of square centers.
# The default parameters generate squares of 500m^2
# !!! Be careful that we are NOT in an euclidean space, compute the size of the edges appropriately !!!
# @trip: positions dataframe
# @edge_lat: the size of sqare's edge parallel to the latitude direction
# @edge_long: the size of sqare's edge parallel to the longitude direction
square_transform <- function(trip, edge_lat=0.0045, edge_long=0.006){
  trip[,1] <- apply(as.data.frame(trip[,1]),2,squarize,edge=edge_long)
  trip[,2] <- apply(as.data.frame(trip[,2]),2,squarize,edge=edge_lat)
  trip <- clean_proximity(trip,0)
  return(trip)
}

### AUXILIARY ###

# Given a coordinate return the closest center of the square having the given edge size
# @coord: coordinate number to analize
# @edge: size of the square edge
squarize <- function(coord, edge){
  side <- floor(coord/edge)*edge
  center <- side + (edge/2)
  return(center)
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
  
  test <- data.frame(TRIP_ID=data$TRIP_ID, CALL_TYPE=data$CALL_TYPE, TIMESTAMP=data$TIMESTAMP, POLYLINE=1:rows)
  empty_rows <- c()
  for(i in 1:rows){
    trip <- positions(data[i,])
    N <- nrow(trip)
    
    if(N>0){
      n_removed <- floor(N * perc[i])
      trip_new <- trip[1:(N-n_removed),]
      test[i,"POLYLINE"] <- posToJSON(trip_new)
    } else{
      empty_rows <- append(empty_rows,c(i))
    }
  }
  
  if(length(empty_rows)>0){
    test <- test[-empty_rows,]
  }
  
  return(test)
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
  
  if(length(empty_rows)>0){
    answers <- answers[-empty_rows,]
  }
  
  colnames(answers) <- c("TRIP_ID","LATITUDE","LONGITUDE")
  return(answers)
}

### VISUALIZATION ###

# Plot the given polyline
# @trip: positions dataframe 
plot_trip <- function(trip){
  plot(trip)
  lines(trip)
  text(trip[,1], trip[,2], c(1:nrow(trip)))
}


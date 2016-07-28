source("utils.r")
source("stepClassification_utils.r")

pol_smooth <- function(row,epsilon=0.00025, error = 0.02, fill = 0.0015){
  removed <- 0
  added <- 0
  points <- positions(row)
  N <- nrow(points)
  newPoints <- data.frame(V1 = 1:N*2, V2 = 1:N*2)
  for(i in 1:N){
    newPoint <- clusterize(points[i,])
    if(i!=1){
      diff <- abs(newPoint[1] - newPoints[i-1-removed+added,1]) + 
        abs(newPoint[2] - newPoints[i-1-removed+added,2])
      if(diff < epsilon | diff > error){
        removed <- removed +1
      }else{
        if(diff > fill){
          addedPoint <- clusterize( data.frame(V1=(newPoint[1] + newPoints[i-1-removed+added,1])/2,
                                               V2=(newPoint[2] + newPoints[i-1-removed+added,2])/2))
          newPoints[i-removed+added,] <- addedPoint
          added <- added +1
          
        }
        newPoints[i-removed + added,] <- newPoint
      }
    }else{
      newPoints[1,] <- newPoint
    }
  }
  if(removed != 0){
    newPoints <- newPoints[-c(N-removed+added+1:(N*2)),]
  }
  return (newPoints)
}

#extract all the final points and put them into a frame
#expects as input a json format
extract_destinations <- function(table){
  usedTrain <- table
  N <- nrow(usedTrain)
  finals <- data.frame(LAT = 1:N, LONG = 1:N)
  nullValues <- 0
  for(i in 1:N){
    points <- positions(usedTrain[i,])
    if(nrow(points)!=0){
      finals[i-nullValues,] <- points[nrow(points),]
    }else{
      nullValues <- nullValues + 1
    }
  }
  if(nullValues!=0){
    finals <- finals[-c((N -nullValues +1):N),]
  }
  return(finals)
}

# returns a table with smoothed polylines
smooth_table <- function(table,epsilon=0.00025, error = 0.02, fill = 0.0015){
  tableS <- table
  for(i in 1:nrow(table)){
    if(nrow(positions((table[i,]))) >= 1){
      tableS[i,"POLYLINE"] <- posToJSON(pol_smooth(table[i,],epsilon, error, fill))
    }
  }
  return(tableS)
}
source("../stepAnova v0.2/utils.r")
source("../stepAnova v0.2/stepAnova_utils.r")
source("../stepAnova v0.2/evaluation_script.r")

pol_smooth <- function(row,epsilon, error, fill){
  removed <- 0
  added <- 0
  points <- positions(row)
  N <- nrow(points)
  newPoints <- data.frame(V1 = 1:N*2, V2 = 1:N*2)
  for(i in 1:N){
    #newPoint <- clusterize(points[i,])
    newPoint <- points[i,]
    if(i!=1){
      diff <- HaversineDistance(points[i,1],points[i,2],
                                points[i-1,1],points[i-1,2])
      if(diff < epsilon | diff > error){
        removed <- removed +1
      }else{
          if(diff > fill){
            addedPoint <- data.frame(V1=(newPoint[1] + newPoints[i-1-removed+added,1])/2,
                                                 V2=(newPoint[2] + newPoints[i-1-removed+added,2])/2)
            newPoints[i-removed+added,] <- addedPoint
            added <- added +1
            
          }
        newPoints[i-removed + added,] <- newPoint
       }
    }else{
      newPoints[1,] <- newPoint
    }
  }
  newPoints <- newPoints[-c(N-removed+added+1:(N*2)),]
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
smooth_table <- function(table,epsilon=0.05, error = 1.5, fill = 0.5){
  tableS <- table
  start_time <- proc.time()
  for(i in 1:nrow(table)){
    if(nrow(positions((table[i,]))) >= 2){
      tableS[i,"POLYLINE"] <- posToJSON(pol_smooth(table[i,],epsilon, error, fill))
    }
    if(i%%50000==0){
      elapsed_time <- proc.time() - start_time
      cat("progress = ",i,"/",nrow(table),"\nelapsed_time = ",
                   elapsed_time/60," minutes\n")
      #write.csv(tables[1:i,],file="trainS_" + i/1000 + "k")
    }
  }
  cat("overall duration = ",(proc.time() - start_time))/60
  return(tableS)
}

#POLTer magico: scrivi il numero della riga e ti disegna una magica traiettoria numerata!
POLT_trip <- function(row,table){
  trace <- positions(table[row,])
  plot(trace)
  lines(trace)
  text(trace[,1], trace[,2], c(1:nrow(trace)))
}
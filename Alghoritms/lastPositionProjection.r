test <- read.csv("test.csv")

initClusters <- read.csv("importantClusters.csv")
initClusters <- initClusters[,-2]

#ADESSO FUNZIONA
projection <- function(p1,p2,v=1){
  vlong <- 0.012 * v
  vlat <- 0.009 * v
  #tang <- (p2[1,2]-p1[1,2])/(p2[1,1]-p1[1,1])
  #angle <- atan(tang)
  angle <-atan2(p2[1,2]-p1[1,2], p2[1,1]-p1[1,1])
  p2[1,1] <- p2[1,1] + vlong * cos(angle)
  p2[1,2] <- p2[1,2] + vlat * sin(angle)
  return(p2)
}

closestCluster <- function(point,clusters){
  distance <- 1:nrow(clusters)
  clusters <- cbind(clusters,distance)
  for(i in 1:nrow(clusters)){
    actual_cluster <- clusters[i,]
    clusters[i,4] <- HaversineDistance(as.numeric(actual_cluster[3]),as.numeric(actual_cluster[2]),as.numeric(point[2]),as.numeric(point[1]))
  }
  newCl <- clusters[order(clusters[,4]),]
  return(newCl[1,1])
}

belongsToWhichCluster <- function (point,clusters,dist=2){
  for(i in 1:nrow(clusters)){
    actual_cluster <- clusters[i,]
    if(HaversineDistance(as.numeric(actual_cluster[3]),as.numeric(actual_cluster[2]),as.numeric(point[2]),as.numeric(point[1]))< dist){
      return(clusters[i,1])
    }
  }
  #default choice
  return(3)
}

N <- nrow(test)
prediction <- data.frame(TRIP_ID=test$TRIP_ID,LATITUDE=1:N,LONGITUDE=1:N)
for(i in 1:N){
  trip <- positions(test[i,])
  trip <- clean_proximity(clean_error(trip,0.8),0.002)
  
  length <- nrow(trip)
  if(length>1){
    lastpos <- projection(trip[length,],trip[length-1,])
  } else {
    lastpos <- tail(trip,1)
  }
  
  index <- closestCluster(lastpos,initClusters)
  prediction[i,2:3] <- initClusters[index,3:2]
}

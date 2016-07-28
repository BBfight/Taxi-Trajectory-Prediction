source("utils.r")
test <- read.csv("test.csv",stringsAsFactors=FALSE)

myClusters <- read.csv("manualClusters.csv")
INDEX <- 1:nrow(myClusters)
myClusters <- cbind(INDEX,myClusters)

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

belongsToWhichCluster <- function (point,clusters){
  for(i in 1:nrow(clusters)){
    current <- clusters[i,]
    if(HaversineDistance(current$LATITUDE,current$LONGITUDE,point[1,2],point[1,1]) < current$RADIUS){
      return(clusters[i,1])
    }
  }

  return(NULL)
}

N <- nrow(test)
#prediction <- data.frame(TRIP_ID=test$TRIP_ID,LATITUDE=1:N,LONGITUDE=1:N)
prediction <- read.csv("wtf_why_does_it_work.csv")
for(i in 1:N){
  trip <- positions(test[i,])
#   trip <- clean_proximity(clean_error(trip,0.8),0.002)
  
#   length <- nrow(trip)
#   if(length>1){
#     lastpos <- projection(trip[length,],trip[length-1,])
#   } else {
#     lastpos <- tail(trip,1)
#   }
  
  lastpos <- tail(trip,1)

  index <- belongsToWhichCluster(lastpos,myClusters)
  if(!is.null(index)){
    prediction[i,2:3] <- myClusters[index,2:3]
  }
}

write.csv(prediction, file = "myprediction.csv", row.names = FALSE)


#ROBA DI ETTY

angle <- function (poi,pof){
  return(atan2(as.numeric(pof[2])-as.numeric(poi[2]),as.numeric(pof[1])-as.numeric(poi[1])) * 180 / pi)
}

#ritorna true se il cluster sta "davanti",else nope
isConsidered <- function(pof,pofMin3,clus,consider = 90){
  angl_traj <- angle(pof,pofMin3)
  angl_clus <- angle(pof,clus)
  
  min_tresh<- (angl_traj - consider) %% 360
  max_tresh<- (angl_traj + consider) %% 360
  
  if(max_tresh< min_tresh){
    if(angl_clus < min_tresh && angl_clus > max_tresh){
      return(TRUE)
    }
  }else {
    if(angl_clus > max_tresh || angl_clus < min_tresh){
      return(TRUE)
    }
  }
  return(FALSE)
}

belongsToWhichClusterAhead <- function (point,pointMin3,clusters){
  for(w in c(1,1.375,1.75)){
    for(i in 1:nrow(clusters)){
      current <- clusters[i,]
      if(isConsidered(point,pointMin3,current[2:3])){
        #print("considered!")
        #print(HaversineDistance(current$LONGITUDE,current$LATITUDE,point[1,2],point[1,1]))
        if(HaversineDistance(current$LONGITUDE,current$LATITUDE,point[1,2],point[1,1]) < (current$RADIUS*w)){
          #print("value found!")
          return(clusters[i,1])
        }
      }
    }
  }
  
  return(NULL)
}

N <- nrow(test)
#prediction <- data.frame(TRIP_ID=test$TRIP_ID,LATITUDE=1:N,LONGITUDE=1:N)
prediction <- read.csv("wtf_why_does_it_work.csv") #IRRILEVANTE
for(i in 1:N){
  trip <- positions(test[i,])
  lastpos <- rev(tail(trip,1))
  if(nrow(trip)>2){
    
    lastMin3pos <- rev(tail(trip,1)[1,])
    index <- belongsToWhichClusterAhead(lastpos,lastMin3pos,myClusters)
    if(!is.null(index)){
      print("yo biach")
      prediction[i,2:3] <- myClusters[index,2:3]
    }else {
      prediction[i,2:3] <- lastpos
      #print("yo biach")
    }
  }else {
    prediction[i,2:3] <- lastpos
  }
}

write.csv(prediction, file = "myprediction.csv", row.names = FALSE)


#last request: clusterize the results!

oldpred <- read.csv("2_63++.csv",stringsAsFactors=FALSE)
newpred <- oldpred
counter <- 0
for(i in 1:nrow(oldpred)){
  #switchare i results
  index <- belongsToWhichCluster(oldpred[i,3:2],myClusters)
  if(!is.null(index)){
    counter <- counter + 1
    newpred[i,2:3] <- myClusters[index,2:3]
    print(paste0("point ",oldpred[i,2]," ",oldpred[i,3]," clustered with cluster ",index," with pos ",myClusters[index,2]," ",myClusters[index,3]))
  }
}
print(paste0("total lines clusterized: ",counter))

write.csv(newpred,file="2_137.csv", row.names = FALSE)

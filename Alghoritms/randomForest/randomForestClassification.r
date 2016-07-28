#USAGE: getClusters(size) -> profit
getClusters <- function(size=10000){
  nCluster <- 0
  clustList <- list()
  for(i in 1:nrow(train[1:size,])){ #per il piacere di POL lo tengo ^^
    if(i %% 25000 == 0){
      print(paste0("Computed up to: ", i))
    }
    crappyPos <- positions(train[i,])
    
    if(nrow(crappyPos)<2){
      next
    }
    trip <- square_transform(clean_error(crappyPos,0.8))
    if(nrow(trip)<2){ #PERCHE' DA NAs A VOLTE?!??!
      next
    }
    initPoint <- trip[1,]
    
    belongsToCluster<- FALSE
    if(nCluster >0 ){
    for (name in 1:nCluster) {
        clusPoint <- clustList[[name]]
        
        if(HaversineDistance(as.numeric(clusPoint[2]),as.numeric(clusPoint[1]),as.numeric(initPoint[2]),as.numeric(initPoint[1]))< 2){
          clusPoint[3]<- as.numeric(clusPoint[3])+1
          clustList[[name]] <- clusPoint
          belongsToCluster<- TRUE
          break
          
        }
    }}
    if(belongsToCluster == FALSE){
      nCluster <- nCluster + 1
      clustList[[nCluster]] <- c(initPoint[1],initPoint[2],1)
    }  
  }

  clusters <- as.data.frame(do.call(rbind,clustList))
  clusters <- as.data.frame(lapply(clusters,unlist))
  return(clusters)
}

getClustersFinalPos <- function(size=10000){
  nCluster <- 0
  clustList <- list()
  for(i in 1:nrow(train[1:size,])){ #per il piacere di POL lo tengo ^^
    if(i %% 25000 == 0){
      print(paste0("Computed up to: ", i))
    }
    crappyPos <- positions(train[i,])
    
    if(nrow(crappyPos)<2){
      next
    }
    trip <- square_transform(clean_error(crappyPos,0.8))
    if(nrow(trip)<2){ #PERCHE' DA NAs A VOLTE?!??!
      next
    }
    finalPoint <- tail(trip,1)
    
    belongsToCluster<- FALSE
    if(nCluster >0 ){
    for (name in 1:nCluster) {
        clusPoint <- clustList[[name]]
        
        if(HaversineDistance(as.numeric(clusPoint[2]),as.numeric(clusPoint[1]),as.numeric(finalPoint[2]),as.numeric(finalPoint[1]))< 1){
          clusPoint[3]<- as.numeric(clusPoint[3])+1
          clustList[[name]] <- clusPoint
          belongsToCluster<- TRUE
          break
          
        }
    }}
    if(belongsToCluster == FALSE){
      nCluster <- nCluster + 1
      clustList[[nCluster]] <- c(finalPoint[1],finalPoint[2],1)
    }  
  }

  clusters <- as.data.frame(do.call(rbind,clustList))
  clusters <- as.data.frame(lapply(clusters,unlist))
  return(clusters)
}

assignTests <- function(clusters){
  clusters$NTESTS <- 0
  for(i in 1:nrow(test)){
    crappyPos <- positions(test[i,])
    
    if(nrow(crappyPos)<2){
      next
    }
    trip <- square_transform(clean_error(crappyPos,0.8))
    if(nrow(trip)<2){ #PERCHE' DA NAs A VOLTE?!??!
      next
    }
    initPoint <- trip[1,]

    for (name in 1:nrow(clusters)) { 
      if(HaversineDistance(as.numeric(clusters[name,2]),as.numeric(clusters[name,1]),as.numeric(initPoint[2]),as.numeric(initPoint[1]))< 2){
        clusters[name,4]<- as.numeric(clusters[name,4])+1
        belongsToCluster<- TRUE
        break
        
      }
    }
  }
  return(clusters)
} 

belongsToCluster <- function (row,lon,lat,dist=2.2){
  crappyPos <- positions(row)
  if(nrow(crappyPos)>2){
    initPoint <- crappyPos[1,]
    if(HaversineDistance(as.numeric(lat),as.numeric(lon),as.numeric(initPoint[2]),as.numeric(initPoint[1]))< dist){
      return(TRUE)
    }
  }
  return(FALSE)
}

get_train_subset <-function(data,lon,lat,dist=2.2){
  train_list <- list()
  nInTrain <- 0
  for(i in 1:nrow(data)){
    if(belongsToCluster(data[i,],lon,lat,dist)){
      nInTrain<- nInTrain + 1
      train_list[[nInTrain]] <- data[i,]
    }
  }
  clusters <- as.data.frame(do.call(rbind,train_list))
  clusters <- as.data.frame(lapply(clusters,unlist))
  return(clusters)
}

get_train_subset_dynamically <-function(data,VIclusters,dist=2.0){
  train_list <- list()
  importantClusters <- VIclusters
  nInTrain <- c(rep(0,nrow(importantClusters)))
  for (i in 1:nrow(importantClusters)){
    train_list[[i]]<- list()
  }
  
  for(i in 1:nrow(data)){
    if(i %% 25000 == 0){
      print(paste0("Computed up to: ", i))
    }
    for(j in 1:nrow(importantClusters)){
      lon<-importantClusters[j,2]
      lat<-importantClusters[j,3]
      if(belongsToCluster(data[i,],lon,lat,dist)){
        nInTrain[j]<- nInTrain[j] + 1
        train_list[[j]][[nInTrain[j]]] <- data[i,]
        break
      }
    }
    
  }
  clusters<-list()
  for(i in 1:nrow(importantClusters)){
    clusters[[i]] <- as.data.frame(do.call(rbind,train_list[[i]]))
    clusters[[i]] <- as.data.frame(lapply(clusters[[i]],unlist))
    
  }
  
  
  return(clusters)
}

# IMPORT TEST, IT'S ALL YOU NEED

print_all <- function(data){
  for(i in 1:nrow(data)){
    original_trip <- positions(data[i,])
    
    plot_trip(original_trip)
    readline(paste("original #",i))
    
    plot_trip(clean_proximity(clean_error(original_trip,0.8),0.05))
    readline(paste("clean #",i))
    
    plot_trip(square_transform(clean_error(original_trip,0.8),edge_lat=0.00225, edge_long=0.003))
    readline(paste("snake250 #",i))
    
    plot_trip(square_transform(clean_error(original_trip,0.8),edge_lat=0.00450, edge_long=0.006))
    readline(paste("snake500 #",i))
    
    ans <- readline("q to exit, enter to continue")
    if(ans=='q') break
  }
}



N <- nrow(test)
info_test <- data.frame(TRIP_ID = 1:N, LENGTH = 1:N, CLEAN = 1:N, SNAKE250 = 1:N, SNAKE500 = 1:N)
for(i in 1:N){
  original_trip <- positions(test[i,])
  
  clean_trip <- clean_proximity(clean_error(original_trip,0.8),0.05)
  snake250_trip <- square_transform(clean_error(original_trip,0.8),edge_lat=0.00225, edge_long=0.003)
  snake500_trip <- square_transform(clean_error(original_trip,0.8),edge_lat=0.00450, edge_long=0.006)
  
  info_test[i,"LENGTH"] <- nrow(original_trip) 
  info_test[i,"CLEAN"] <- nrow(clean_trip) 
  info_test[i,"SNAKE250"] <- nrow(snake250_trip) 
  info_test[i,"SNAKE500"] <- nrow(snake500_trip) 
}
summary(info_test[,-1])

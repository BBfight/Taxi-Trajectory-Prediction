### SETUP ###

# Working directory
setwd("~/Polimi/Magistrale/Anno I/Semestre II/Data Mining and Text Mining/Project Taxi/stepAnova v0.2")

# libraries
library(rpart)
source("utils.r")
source("stepAnova_utils.r")

# Import data set
#train_full <- read.csv(paste(getwd(),"/../original data/train.csv",sep=""),stringsAsFactors=FALSE)
#train_mini <- read.csv(paste(getwd(),"/../original data/minitrain.csv",sep=""),stringsAsFactors=FALSE)
#train_medium <- read.csv(paste(getwd(),"/../mydata/train_200k.csv",sep=""),stringsAsFactors=FALSE)

#test <- read.csv(paste(getwd(),"/../original data/test.csv",sep=""),stringsAsFactors=FALSE)
test <- read.csv(paste(getwd(),"/../mydata/test_1k.csv",sep=""),stringsAsFactors=FALSE)
#test <- create_test(tail(train_medium,1000))


answer <- read.csv(paste(getwd(),"/../mydata/answer_1k.csv",sep=""),stringsAsFactors=FALSE)
#answer <- create_answer(tail(train_medium,1000))

### PREPROCESSING ###
step <- 5
gap <- 3
clean <- 0.050
train_ready <- disassembleData(train, step, gap, start=0.6, end=0.9, clean) #i parametri da cambiare sono dentro alla funzione, ma possono essere facilmente portati qui
write.csv(train_ready, file = "train_ready_anova_from500k_clean50.csv", row.names = FALSE)
# train_ready <- read.csv(paste(getwd(),"/train_ready_anova_from50k.csv",sep=""),stringsAsFactors=FALSE)

### TRAINING ###

# check the documentation for more fun!
# THE NAMES OF THE COLUMNS MUST BE CHANGED BY HAND WHEN CHANGING THE STEP
fit_long <- rpart(dest_long ~ LONG1 + LAT1 + LONG2 + LAT2 + LONG3 + LAT3 + LONG4 + LAT4 + LONG5 + LAT5
             , data=train_ready, method="anova", minsplit=5, cp=0.001)
save(fit_long,file="tree_long_500k_split5_cp001_clean50")

fit_lat <- rpart(dest_lat ~ LONG1 + LAT1 + LONG2 + LAT2 + LONG3 + LAT3 + LONG4 + LAT4 + LONG5 + LAT5
                  , data=train_ready, method="anova", minsplit=5, cp=0.001)
save(fit_lat,file="tree_lat_1M_split5_cp001_clean50")

### CLASSIFICATION ###
test <- read.csv(paste(getwd(),"/../original data/test.csv",sep=""),stringsAsFactors=FALSE)

N <- nrow(test)
prediction <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
for(i in 1:N){
  trip <- clean_proximity(positions(test[i,]),0.03)
  
  if(nrow(trip)<=step){
    dest <- rev(tail(trip,1))
  } else {
    test_trip <- as.data.frame(disassemble(trip, step, gap, start=0.3, end=1))
    colnames(test_trip) <- c(paste(c("LONG","LAT"),rbind(1:step,1:step),sep="")) 
    weights <- 1:nrow(test_trip)
    
    dest_lat <- avg(predict(fit_lat, test_trip),weights)
    dest_long <- avg(predict(fit_long, test_trip),weights)
  }
  prediction[i,] <- c(test[i,"TRIP_ID"], dest_lat,dest_long)
}


### EVALUATION ###
#write.csv(prediction, file = "myprediction.csv", row.names = FALSE)

evaluation <- evaluate(prediction,answer)
print(evaluation)


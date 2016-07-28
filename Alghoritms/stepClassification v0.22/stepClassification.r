### SETUP ###

# Working directory
setwd("~/Polimi/Magistrale/Anno I/Semestre II/Data Mining and Text Mining/Project Taxi/stepClassification v0.22")

# libraries
library(rpart)
source("utils.r")
source("stepClassification_utils.r")

# Import data set
#train_full <- read.csv(paste(getwd(),"/../original data/train.csv",sep=""),stringsAsFactors=FALSE)
#train_mini <- read.csv(paste(getwd(),"/../original data/minitrain.csv",sep=""),stringsAsFactors=FALSE)
train_medium <- read.csv(paste(getwd(),"/../mydata/train_200k.csv",sep=""),stringsAsFactors=FALSE)
train <- train_medium[1:30000,]

test_kaggle <- read.csv(paste(getwd(),"/../original data/test.csv",sep=""),stringsAsFactors=FALSE)
test <- read.csv(paste(getwd(),"/../mydata/test_tail2k.csv",sep=""),stringsAsFactors=FALSE)
#test <- create_test(tail(train_medium,1000))


answer <- read.csv(paste(getwd(),"/../mydata/answer_tail2k.csv",sep=""),stringsAsFactors=FALSE)
#answer <- create_answer(tail(train_medium,1000))

### PREPROCESSING ###
step <- 5
gap <- 4
train_ready <- disassembleData(train, step, gap) #i parametri da cambiare sono dentro alla funzione, ma possono essere facilmente portati qui
write.csv(train_ready, file = "train_ready_from30k_approx2.csv", row.names = FALSE)
train_ready <- read.csv(paste(getwd(),"/train_ready_from30k_approx3.csv",sep=""),stringsAsFactors=FALSE)

### TRAINING ###

# check the documentation for more fun!
# THE NAMES OF THE COLUMNS MUST BE CHANGED BY HAND WHEN CHANGING THE STEP
fit <- rpart(destination ~ LONG1 + LAT1 + LONG2 + LAT2 + LONG3 + LAT3 + LONG4 + LAT4 + LONG5 + LAT5
             , data=train_ready, method="class", minsplit=50, cp=0.01)
save(fit,file="tree30k_approx3_split50_cp01")

### CLASSIFICATION ###

N <- nrow(test)
prediction <- data.frame(TRIP_ID = 1:N, LATITUDE = 1:N, LONGITUDE = 1:N)
for(i in 1:N){
  trip <- positions(test[i,])
  
  if(nrow(trip)<=step){
    dest <- rev(tail(trip,1))
  } else {
    test_trip <- disassemble(trip, step, gap, start=0, end=1)
    prediction_dest <- as.vector(predict(fit, test_trip, type="class"))
    dest <- rev(extractPrediction(prediction_dest))
  }
  prediction[i,] <- c(test[i,"TRIP_ID"], dest)
}


### EVALUATION ###

evaluation <- evaluate(prediction,answer)
print(evaluation)

write.csv(prediction, file = "myprediction.csv", row.names = FALSE)

# TODO
library(dplyr)
setwd("D:\\R-Machine-Learning\\Data")
#data: https://archive.ics.uci.edu/ml/datasets/Heart+Disease

#Prepare Data
heart <- read.csv("processed.cleveland.csv",
                  header = FALSE)

colnames(heart) <- c("age","sex","cp","trestbps",
                     "chol","fbs","restecg","thalach",
                     "exang","oldpeak","slope","ca",
                     "thal","heart.disease")

head(heart)

table(heart$heart.disease)

presence <- function(heart.disease){
  if (heart.disease == 0){
    return(0)
  }else{
    return(1)
  } 
}

heart$heart.disease <- sapply(heart$heart.disease,presence)

table(heart$heart.disease)

str(heart)


#Missing Data
heart$ca <- as.numeric(heart$ca)
heart$thal <- as.numeric(heart$thal)
str(heart)
any(is.na(heart))

library(Amelia)
missmap(heart)
heart <- na.omit(heart)
any(is.na(heart))

#Splitting Data
library(caTools)
split <- sample.split(heart$heart.disease,SplitRatio = 0.7)
train.data <- subset(heart, split==TRUE)
test.data <-subset(heart, split==FALSE)

str(heart)
#Building the Model
library(randomForest)
heart.rf <- randomForest(heart.disease ~., data = train.data,
                         importance=TRUE)

predicted.heart.disease <- predict(heart.rf,test.data)

Yes.or.No <- function(prob){
  if(prob>=0.5){
    return(1)
  }else{
    return(0)
  }
}
  
predicted.heart.disease <- sapply(predicted.heart.disease,
                                  Yes.or.No)
misclass.error <- mean(predicted.heart.disease != test.data$heart.disease)
misclass.error

table(predicted.heart.disease,test.data$heart.disease)

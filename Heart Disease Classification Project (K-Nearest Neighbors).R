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

#Standardising Data
heart.std <- as.data.frame(scale(heart[,-14]))
var(heart.std$chol)

heart.std$heart.disease <- heart$heart.disease 
head(heart.std)

#Splitting Data
library(caTools)
set.seed(101)
split <- sample.split(heart.std$heart.disease,SplitRatio = 0.7)
train.data <- subset(heart.std,split==TRUE)
test.data <- subset(heart.std,split==FALSE)
train.heart.disease <- train.data$heart.disease

#Building the Model
library(class)
predicted.disease <- knn(train.data[,1:13],
                         test.data[,1:13],
                         train.heart.disease,
                         k=1)
predicted.disease

misclass.error <- mean(predicted.disease != test.data$heart.disease)
misclass.error

#Choosing K Value
predicted.disease <- NULL
misclass.error <- NULL

for (i in 1:20) {
  set.seed(101)
  predicted.disease <- knn(train.data[,1:13],
                           test.data[,1:13],
                           train.heart.disease,
                           k=i)
  misclass.error[i] <- mean(predicted.disease != test.data$heart.disease)
  
}
misclass.error
k.values <- 1:20

df <- data.frame(misclass.error,k.values)
df

library(ggplot2)
ggplot(df,aes(x=k.values,y=misclass.error)) + geom_point(size=3) +
  geom_line(lty="dotted",color="red",size=1)

#We should choose 6 as k value, however approx. 13,5% misclass
#error is still pretty high.


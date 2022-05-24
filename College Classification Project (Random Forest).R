library(ISLR)
library(dplyr)

head(College)
str(College)
df <- College

#Exploratory Data Analysis
library(ggplot2)

ggplot(df,aes(x=Room.Board,y=Grad.Rate)) +
  geom_point(aes(color=Private),size=2)

ggplot(df,aes(x=F.Undergrad)) + geom_histogram(aes(fill=Private),
                                               color="black",
                                               size=1,bins = 60)

ggplot(df,aes(x=Grad.Rate)) + geom_histogram(aes(fill=Private),
                                           color="black",
                                           size=1,bins = 60)

head(df)

df[df$Grad.Rate>100,]
#Cazenovia College has Grad Rate > 100

df["Cazenovia College","Grad.Rate"] <- 100
df["Cazenovia College","Grad.Rate"]

#Splitting Data
library(caTools)
set.seed(101)
split <- sample.split(df$Private,SplitRatio = 0.7)
train.data <- subset(df,split==TRUE)
test.data <- subset(df,split==FALSE)

#Decision Tree
library(rpart)
library(rpart.plot)

tree <- rpart(Private ~ ., method = "class",
              data = train.data)
prp(tree)
summary(tree)

#Test the Tree
predicted.private <- predict(tree,test.data)
head(predicted.private)

str(test.data)

predicted.private <- as.data.frame(predicted.private)

predicted.private$Pred.private <- predicted.private$Yes

yes.or.no <- function(yes){
  if(yes>0.5){
    return("Yes")
  }else{
    return("No")
  }
}

predicted.private$Pred.private <- sapply(predicted.private$Pred.private, yes.or.no)

head(predicted.private)
tail(predicted.private)

#Confusion Matrix
table(predicted.private$Pred.private,test.data$Private)

#Build a Random Forest Model
library(randomForest)

rf.model <- randomForest(Private ~ ., data = train.data,
                         importance=TRUE)
rf.model$confusion
rf.model$importance

predicted.rf.private <- predict(rf.model,test.data)
table(predicted.rf.private, test.data$Private)

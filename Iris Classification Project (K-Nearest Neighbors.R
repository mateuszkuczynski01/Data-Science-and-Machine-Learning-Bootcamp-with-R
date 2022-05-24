install.packages("ISLR")
library(ISLR)
library(dplyr)

head(iris)
str(iris)

#Preparing Data
standardised <- scale(select(iris,-Species))
standardised <- as.data.frame(standardised)
var(standardised$Sepal.Length)

standardised$Species <- iris$Species
head(standardised)

#Splitting Data
library(caTools)
set.seed(101)
split <- sample.split(standardised$Species,SplitRatio = 0.7)
train.data <- subset(standardised,split==TRUE)
test.data <- subset(standardised,split==FALSE)

train.Species <- train.data$Species
library(class)

#Building a Model
predicted.species <- knn(train.data[1:4], test.data[1:4],
                         train.Species, k=1)
predicted.species

misclass.error <- mean(predicted.species != test.data$Species)
misclass.error

#Choosing a K Value

predicted.species <- NULL
error.rate <- NULL

for (i in 1:10) {
  set.seed(101)
  predicted.species <- knn(train.data[1:4], test.data[1:4],
                           train.Species, k=i)
  error.rate[i] <- mean(predicted.species != test.data$Species)
}

error.rate
k.values <- 1:10

df <- data.frame(error.rate,k.values)
df

ggplot(df,aes(x=k.values,y=error.rate)) + geom_point(size=4) +
  geom_line(lty="dotted",color="red",size=1)
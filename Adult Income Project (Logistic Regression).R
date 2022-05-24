setwd("D:\\R-Machine-Learning\\Data")
adult <- read.csv("adult_sal.csv")

head(adult)
library(dplyr)
adult <- select(adult,-X)

head(adult)
str(adult)
summary(adult)

#Data Cleaning
table(adult$type_employer)

unemp <- function(job){
  job <- as.character(job)
  if(job=="Never-worked" | job=="Without-pay"){
    return("Unemployed")
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, unemp)
table(adult$type_employer)

emp <- function(job){
  job <- as.character(job)
  if(job=="State-gov" | job=="Local-gov"){
    return("SL-gov")
  }else if(job=="Self-emp-inc" | job=="Self-emp-not-inc"){
    return("self-emp")
    }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, emp)
table(adult$type_employer)
table(adult$marital)

group.married <- function(mar){
  if(mar=="Married-AF-spouse" | mar=="Married-civ-spouse" |
     mar=="Married-spouse-absent"){
    return("Married")
  }else if(mar=="Divorced" | mar=="Separated" |
           mar=="Widowed"){
    return("Not-Married")
  }else{
    return("Never-Married")
  }
}

adult$marital <- sapply(adult$marital, group.married)
table(adult$marital)

table(adult$country)
adult$country <- factor(adult$country)
levels(adult$country)

Asia <- c("China","Laos","Hong","India","Thailand","Vietnam",
          "Taiwan", "Philippines", "Japan","Iran","Cambodia")

North.America <- c("United-States","Canada","Puerto-Rico")

Latin.and.South.America <- c("Cuba","Columbia","Dominican-Republic","Ecuador","El-Salvador",
                             "Guatemala","Haiti","Honduras","Jamaica",
                             "Mexico","Nicaragua","Peru", "Trinadad&Tobago" )
                            
                          
Europe <- c("England","France","Germany","Greece","Holand-Netherlands",
            "Hungary","Ireland","Italy","Poland","Portugal","Scotland",
            "Yugoslavia")

Other <- c("South","Outlying-US(Guam-USVI-etc)")

group.continents <- function(cnt){
  if(cnt %in% Asia){
    return("Asia")
  }else if(cnt %in% North.America){
    return("North.America")
  }else if(cnt %in% Latin.and.South.America){
    return("Latin.and.South.America")
  }else if(cnt %in% Europe){
    return("Europe")
  }else{
    return("Other")
  }
}

adult$country <- sapply(adult$country, group.continents) 
table(adult$country)

str(adult)

adult$type_employer <- factor(adult$type_employer)
adult$education <- factor(adult$education)
adult$marital <- factor(adult$marital)
adult$occupation <- factor(adult$occupation)
adult$relationship <- factor(adult$relationship)
adult$race <- factor(adult$race)
adult$sex <- factor(adult$sex )
adult$country <- factor(adult$country)
adult$income <- factor(adult$income)
str(adult)

#Missing Data
library(Amelia)

adult[adult=="?"] <- NA
table(adult$type_employer)

missmap(adult,main = "Missingness Map", y.at=c(1),
        y.labels = c(""),
        col = c("yellow","black"),legend = TRUE)

adult <- na.omit(adult)

missmap(adult,main = "Missingness Map", y.at=c(1),
        y.labels = c(""),
        col = c("yellow","black"),legend = TRUE)

str(adult)

#Exploratory Data Analysis
library(ggplot2)

ggplot(adult,aes(x=age,)) + geom_histogram(aes(fill=income),
                                           color="black",
                                           binwidth = 1)

ggplot(adult,aes(x=hr_per_week)) + geom_histogram()


adult <- rename(adult,"region"="country")

a <- ggplot(adult,aes(x=region)) + geom_bar(aes(fill=income),
                                            color="black",
                                            size=1)

a + scale_x_discrete(limits=c("North.America",
                              "Latin.and.South.America",
                              "Other",
                              "Asia",
                              "Europe"))+
  theme(axis.text.x = element_text(angle=90,hjust=1,size=10))
                      
head(adult)

#Building The Model

library(caTools)

set.seed(101)
split <- sample.split(adult$income,SplitRatio = 0.7)
final.train <- subset(adult,split==TRUE)
final.test <- subset(adult,split==FALSE)

?glm

final.model <- glm(income ~ .,family = binomial(link="logit"),
                   data = final.train)
summary(final.model)

new.model <- step(final.model)

summary(new.model)

final.test$predicted.income <- predict(final.model, 
                                 final.test, type = "response")

table(final.test$income, final.test$predicted.income > 0.5)

accuracy <- (6373+1423)/(6373+1423+874+547)
accuracy

recall.a <- 6372/(6372+874)
recall.a

recall.b <- 1421/(1421+547)
recall.b 

precision.a <- 6373/(6373+547)
precision.a

precision.b <- 1421/(1421+874)
precision.b
                     
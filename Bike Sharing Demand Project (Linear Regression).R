setwd("D:\\R-Machine-Learning\\Data")

library(ggplot2)
library(ggthemes)
library(dplyr)

bike <- read.csv("bikeshare.csv")
head(bike)
str(bike)
summary(bike)
#count vs temp plot
ggplot(bike,aes(x=temp,y=count)) + geom_point(aes(
  color=temp
),alpha=0.3,size=3)

#count vs datetime plot
class(bike$datetime)
bike$datetime <- as.POSIXct(bike$datetime, 
                            format = "%Y-%m-%d %H:%M:%S")
  
head(bike$datetime)

ggplot(bike,aes(x=datetime,y=count)) +
  geom_point(aes(color=temp),alpha=0.5,size=3) +
  scale_color_gradient(
    low = "pink",high = "red")

#Correlation between temp and count
cortc <- cor(bike[,c("temp","count")])
cortc

#count vs season plot
ggplot(bike,aes(x=factor(season),y=count)) +
  geom_boxplot(size=1.2,
               aes(color=factor(season)))

#Create an "hour" column
bike$hour <- bike$datetime
bike$hour <- format(bike$hour,"%H")
head(bike$hour)

#Count vs hours plot for working days
ggplot(bike %>% filter(workingday==1),
       aes(x=hour,y=count)
       ) + geom_jitter(aes(color=temp),alpha=0.6,size=3) +
  scale_color_gradientn(
    colors=c("dark blue","blue","light blue",
             "light green","yellow","orange","red")
  )

#Count vs hours plot for non-working days

ggplot(bike %>% filter(workingday==0),
       aes(x=hour,y=count)
) + geom_jitter(aes(color=temp),alpha=0.6,size=3) +
  scale_color_gradientn(
    colors=c("dark blue","blue","light blue",
             "light green","yellow","orange","red")
  )

#Build the model

temp.model <- lm(count ~ temp,bike)
summary(temp.model)

#Bike rentals if temperature was 25 degrees Celsius - 2 ways

bike.rentals <- 9.1705*25+6.0462
bike.rentals

bike.rentals2 <- predict(temp.model,data.frame(temp=c(25)))
bike.rentals2

######change hours to numeric
bike$hour <- sapply(bike$hour, as.numeric)

head(bike)

#Final Model: season, holiday, workingday, weather, temp
#humidity, windspeed, hour(factor)

bike.filtered <- bike[,c("season","holiday","workingday","weather",
                         "temp","humidity","windspeed",
                         "hour","count")]
  
model <- lm(count ~., data = bike.filtered)
summary(model)

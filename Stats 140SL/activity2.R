## activity
## Bike Example
library(readr)
library(ggmap)
library(ggplot2)
library(lubridate)

## load the basemap LAmap.RData

## raed the bike data Metro_Bike_Share_Trip_Data.csv


## create a day of week variablee to help filter the data

## limit the starting station latitude to 34.0521 < Starting Station Latitude < 34.0511


## use the limited dataset and choose a day of week, here is my example
ggmap(LA) +
  geom_segment(data = BD2[BD2$DOW==6,], aes(x = as.numeric(`Starting Station Longitude`), 
                               y = as.numeric(`Starting Station Latitude`), 
                               xend = as.numeric(`Ending Station Longitude`), 
                               yend = as.numeric(`Ending Station Latitude`)))

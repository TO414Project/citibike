
Read the csv file into R:
```{r}
bikesample <- read.csv("bikesamplelarge.csv")
```

Import the related libraries:
```{r}
if(!require('ggmap')){
  install.packages('ggmap', repos = "http://cran.us.r-project.org")
  library(ggmap)
}

if(!require('ggplot2')){
  install.packages('ggplot2', repos = "http://cran.us.r-project.org")
  library(ggplot2)
}
```

### Question 1
The top20 polpular citibike stations:
```{r}
#Extract the numbers of leaves and arrivals from the data set 
start_station <- as.data.frame(table(bikesample$start.station.name))
end_station <- as.data.frame(table(bikesample$end.station.name))

#Extract the location of stations from the data set and omit the repeaded lines
names_coor <- data.frame(bikesample$start.station.name,bikesample$start.station.latitude,bikesample$start.station.longitude)
deduped.names_coor <- unique(names_coor)

#Combine the data frames together and calculate the out-bike ratio
stations <- merge(start_station,end_station,by.x="Var1",by.y="Var1")
stations_coor <- merge(stations,deduped.names_coor,by.x="Var1",by.y="bikesample.start.station.name")

stations_coor$bike_avg <- (stations_coor$Freq.x+stations_coor$Freq.y)/2
popular_stations <- stations_coor[order(-stations_coor$bike_avg),][1:20,]

map1 <- get_googlemap(center = c(lon = -73.989, lat = 40.74), zoom = 13,size = c(640, 640))

#plot the selected bike stations onto the google map and displace the points' color according to the degree that the staion is running out of bikes
ggmap(map1)+geom_point(data = popular_stations , aes(x = bikesample.start.station.longitude, y = bikesample.start.station.latitude), size = 4, shape = 21,fill="Blue") 
```

#Conlcusion and recommendation:As we can see from the map above, the most popular 20 citibike stations are clusters at left-to-center Manhattan area.


### Question 3
#In order to figure out the assymetric traffic faced by citibike and make recommendations on bike addition in current satations or new station addition, we are going to look at the level of assymetry in terms of both percentages and absolute values:

a) assymetry in terms of percentages
```{r}
#select the stations to plot
stations_coor$shortage <- stations_coor$Freq.x-stations_coor$Freq.y
stations_coor$bike_shortage_ratio =stations_coor[,7]/stations_coor[,6]

stations_coor <- stations_coor[order(stations_coor$bike_shortage_ratio),]

pct_surplus_stations <- stations_coor[1:30,]
pct_shortage_stations <- stations_coor[(nrow(stations_coor)-29):nrow(stations_coor),]

pct_selected_stations <- rbind(pct_surplus_stations,pct_shortage_stations)

#plot the google map in R
map2 <- get_googlemap(center = c(lon = -73.97, lat = 40.73), zoom = 12,size = c(640, 640))

#plot the selected bike stations onto the google map and displace the points' color according to the degree that the staion is running out of bikes
ggmap(map2)+geom_point(data = pct_selected_stations , aes(x = bikesample.start.station.longitude, y = bikesample.start.station.latitude,fill = bike_shortage_ratio), size = 2, shape = 21,show.legend = TRUE) + scale_fill_gradient(low = "#0000FF", high = "#FF0000")
```

The top 30 citibike stations in terms of bike surplus percentage that might need to add more docks are the followings: 
```{r}
print(pct_surplus_stations[,1])
```

The top 30 citibike stations in terms of bike shortage percentage that might need to add more bikes are the followings: 
```{r}
print(pct_shortage_stations[,1])
```

#Conlcusion and recommendation:
#Areas in upper Manhattan and Brooklyn are demonstating a bike shortage. Those could be areas that citibike company can increase bike capacity to current stations or set up new bike stations in neighbour streets. 


b) assymetry in terms of absolute surplus and shortage
```{r}

stations_coor <- stations_coor[order(stations_coor$shortage),]
stations_coor$Surplus_Shortage <- ifelse(stations_coor$shortage>0,"Shortage","Surplus")

#Top 50 stations with shortages and surpluses
abs_surplus_stations<- head(stations_coor, n=70)
abs_shortage_stations <- tail(stations_coor, n =70)
abs_selected_stations <- rbind(abs_surplus_stations, abs_shortage_stations)


map3 <- get_googlemap(center = c(lon = -73.97, lat = 40.73), zoom = 12,size = c(640, 640))
ggmap(map3) + geom_point(aes(x = bikesample.start.station.longitude, y = bikesample.start.station.latitude), colour = ifelse(abs_selected_stations$Surplus=="Surplus",'navyblue','red3'), data = abs_selected_stations, size = 3,alpha = .5, show.legend = FALSE)
```

#Conlcusion and recommendation:
#By exploring the absolute surplus and shortage, we figured out the stations that need capacity expansion the most.
#The blue dots show the satations are running a bike surplus and the red dots are in bike shortage. We can see the pattern that people in Manhattan are moving from the edge (especially the right edge) to the center.
#We recommend to add bikes to those current stations / set up new bike stations in neighbour streets at the edge of Manhattan and add empty docks at the left-to-center side of Manhattan.


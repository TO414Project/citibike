```{r}
#import the data file
bikesample <- read.csv("bikesample.csv")
```

### Question 3
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
stations_coor$out_bike_ratio =stations_coor[,3]/stations_coor[,2]-1

#Import the related libraries
if(!require('ggmap')){
  install.packages('ggmap', repos = "http://cran.us.r-project.org")
  library(ggmap)
}

if(!require('ggplot2')){
  install.packages('ggplot2', repos = "http://cran.us.r-project.org")
  library(ggplot2)
}

#select the stations to plot
stations_coor_selected <- stations_coor[stations_coor$out_bike_ratio>0.2,]

#plot the google map in R
map <- get_googlemap(center = c(lon = -73.97, lat = 40.73), zoom = 12,size = c(640, 640))

#plot the selected bike stations onto the google map and displace the points' color according to the degree that the staion is running out of bikes
ggmap(map)+geom_point(data = stations_coor_selected, aes(x = bikesample.start.station.longitude, y = bikesample.start.station.latitude,fill = out_bike_ratio), size = 2, shape = 21,show.legend = TRUE) + scale_fill_gradient(low = "#0000FF", high = "#FF0000")

#Print the names
print(stations_coor_selected[,1])
```


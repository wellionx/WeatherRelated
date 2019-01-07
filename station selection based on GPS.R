load(file="E:/Data/datav/station information.rda")
#daystation -- station summary merged from the description data
#daystinfo -- station information from weather data

#1.根据地块GPS信息锁定附近气象站点
# Nearest Neighbor Search####
#Before we can find the k-nearest stations we need an accurate metric to measure distance between a location’s reference point and each station.

deg2rad <- function(deg) return(deg*pi/180)

gcd.slc <- function(long1, lat1, long2, lat2) {
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  R <- 6371 # Earth mean radius in km
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}
# Great cirle distance calculator:
kNStations <- function(coords, station.list, k = 5){
  kns.ix <- NULL # create a variable to track row index the stations
  kns.dist <- NULL # create a distance variable  
  dist <- gcd.slc(coords$LON, coords$LAT, station.list$LON, station.list$LAT)
  distSort <- sort(dist, ind=TRUE) 
  tmp.ix <- distSort$ix[1:k] # temporary index variable for the loop
  tmp.dist <- distSort$x[1:k] # temporary distance variable for the loop
  kns.ix <- c(kns.ix, tmp.ix) # append row index of stations for each location
  kns.dist <- c(kns.dist, tmp.dist) # append distances of stations for each location
  
  st <- station.list[kns.ix,1:ncol(station.list)] # Subset the full list with k-nearest stations
  st$Ref_Lat <- rep(coords$LAT,each=k) # Insert reference LAT
  st$Ref_Lon <- rep(coords$LON, each=k) # Insert reference LON
  
  st$kilo_distance <- kns.dist # Insert distance into result
  st <- st[with(st,order(kilo_distance)),]
  st$rank <- rep(1:k) # Rank closest to farthest (1 to k)
  
  return (st)
}

coords <- data.frame(109.200415,18.366201)
coords <- read.delim("clipboard",header = F)
names(coords) <- c("LON","LAT")

kns5 <- data.frame()

for (i in 1:length(rownames(coords))) {
  kns.tmp <- kNStations(coords[i,], daystation,k=5)
  kns5 <- rbind(kns5,kns.tmp)
}

write.csv(kns5, "nearest 5 weather stations of 10 sites.csv")

library(leafletCN)
library(leaflet)
stc <- read.csv(file = "E:/Data/datav/stations with weather data.csv")
pop <- paste("气象站名称：",stc$mc,"<br/>",
             
             "ID：",stc$stationID,"<br/>",
             
             "省份：",stc$province,"<br/>",
             
             "经度：",stc$longitude,"<br/>",
             
             "纬度：",stc$latitude,"<br/>")
# 吴桥地块展示 #
gps <- data.frame(116.6170700000,	37.6861300000)
#sanya
gps <- data.frame(109.200415,18.366201)
colnames(gps) <- c("lon","lat")
leaflet(stc)%>%amap()%>%
  addMarkers(popup=pop) %>%
  addCircleMarkers(lng = gps$lon,lat = gps$lat,
                   color="red") %>%
  setView(lng = gps$lon,lat = gps$lat,zoom = 8) 


#2.根据地址锁定附近气象站点
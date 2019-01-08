#Solar radiation Calc seperately ####
library(data.table)
#All weather stations
stations <- read.csv("E:/Data/datav/stations with weather data.csv")
#select HHH plain
NCPstaions <- subset(stations, Province %in% c("山东","河南","河北")) 
#check the selected stations in three Provinces
NCPstaions$Province <- factor(NCPstaions$Province)
table(NCPstaions$Province)
#load all the weather data
load("E:/Data/datav/气象数据1980to20181130.rda")
#get stationID
unique(NCPstaions$stationID)
wthssd <- DT_SSD[stationID %in% unique(NCPstaions$stationID)]
wthevp <- DT_EVP[stationID %in% unique(NCPstaions$stationID)]
#station info
head(NCPstaions)
stinfo <- NCPstaions[,c("stationID","LAT")]
stinfo <- unique(stinfo)
nrow(stinfo)

#Merge the station info to the data
wthssd2 <- merge(wthssd, stinfo ,by="stationID",allow.cartesian=TRUE,all = TRUE)
summary(wthssd2$SSD)
wthssd2[wthssd2=="32766"] <- NA
wthssd2 <- unique(wthssd2)
#impute the missing data ####
#KNN插值
library(DMwR)
im <- wthssd2[,-1]
knnOutput <- knnImputation(im)  # 使用KNN插值.
anyNA(knnOutput)
wthssd2_im <- cbind(wthssd2[,1],knnOutput)

wthssd2s <- wthssd2_im
#Calc solar radiation
wthssd2s$SSDi <- wthssd2s$SSD/10

#Creat Date variable
wthssd2s$Date <- as.Date(paste(wthssd2s$Year,wthssd2s$Month,wthssd2s$Day,sep = '-'))

# The original A–P formula is
# Rs/Ra =  a + b(n/N)  
# Clac params
doyFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%j"))
}
#DOY --day number of the year counting from first January
wthssd2s$DOY <- doyFromDate(wthssd2s$Date)


#write a function to run solar radaition estimate #####
solarestmt <- function(LAT, DOY, SSD) {
  # degree to radians
  phi <- pi/180*LAT
  # Gsc -- solar constant
  # Gsc is the solar constant (0.082 MJ m^2 min^1) the metric different
  Gsc = 0.082
  # delta-- solar declination (radians)
  delta <- 0.409*sin(2*pi*(DOY)/365 - 1.39)
  
  # omega--sunset hour angle
  omega <- acos(-tan(delta)*tan(phi))
  # The maximum possible sunshine hours--N -- day length
  N <- 24/pi*(omega)
  
  # dr is inverse relative distance between Earth and Sun
  dr <- (1 + 0.033*cos(2*pi*(DOY)/365))
  
  #Extraterrestrial solar radiation H0
  Ra <- (24*60)/pi*Gsc*dr*(omega*sin(phi)*sin(delta) + cos(phi)*cos(delta)*sin(omega))
  
  # The indicative calues for empirical constants in the Angstrom formula in relation
  # to latitude and climate used by FAO (Frere & Popov, 1979)
  # Cold and Temperate Zones: a=0.18, b=0.55
  # Dry tropical zones:       a=0.25, b=0.45
  # Humid tropical zones:     a=0.29, b=0.42
  
  #a=0.18;b=0.55
  a=0.2;b=0.51 #from wuqiao county
  Rs <- Ra*(a+b*(SSD/N))
  return(Rs)
}
wthssd2s$Rs <- solarestmt(wthssd2s$LAT,wthssd2s$DOY,wthssd2s$SSDi)
  
names(wthssd2s)
wthsolar <- wthssd2s[,c(1:4,8,16)]

save(wthsolar,file = "./data/solar radiatiion of HHH plain 59 stations.rda")

#Method 1 --ref:1.	Zhao N, Zeng X, Han S: Solar radiation estimation using sunshine hour and air pollution index in China. Energy Conversion and Management 2013, 76:846-851.

#constant --latitude of the place
# degree to radians

wthssd2s$phi <- pi/180*wthssd2s$LAT

# Gsc -- solar constant
# Gsc is the solar constant (0.082 MJ m^2 min^1) the metric different
Gsc = 0.082
# delta-- solar declination (radians)
wthssd2s$delta <- 0.409*sin(2*pi*(wthssd2s$DOY)/365 - 1.39)

# omega--sunset hour angle
wthssd2s$omega <- acos(-tan(wthssd2s$delta)*tan(wthssd2s$phi))
# The maximum possible sunshine hours--N -- day length
wthssd2s$N <- 24/pi*(wthssd2s$omega)

# dr is inverse relative distance between Earth and Sun
wthssd2s$dr <- (1 + 0.033*cos(2*pi*(wthssd2s$DOY)/365))

#Extraterrestrial solar radiation H0
wthssd2s$Ra <- (24*60)/pi*Gsc*wthssd2s$dr*(wthssd2s$omega*sin(wthssd2s$phi)*sin(wthssd2s$delta) + cos(wthssd2s$phi)*cos(wthssd2s$delta)*sin(wthssd2s$omega))

# The indicative calues for empirical constants in the Angstrom formula in relation
# to latitude and climate used by FAO (Frere & Popov, 1979)
# Cold and Temperate Zones: a=0.18, b=0.55
# Dry tropical zones:       a=0.25, b=0.45
# Humid tropical zones:     a=0.29, b=0.42

#a=0.18;b=0.55
a=0.2;b=0.51 #from wuqiao county
wthssd2s$Rs <- wthssd2s$Ra*(a+b*(wthssd2s$SSDi/wthssd2s$N))

#Pengman formula ####
###########
# Peanman-Monteith 公式需要计算地点的纬度和海拔高程，对于固定地点，
# 计算所需的气象数据主要有最高气温、最低气温、相对湿度、风速和日照时数5个参数

library(data.table)
#All weather stations
stations <- read.csv("E:/Data/datav/stations with weather data.csv")
setDT(stations)
#select HHH plain
NCPstaions <- subset(stations, Province %in% c("山东","河南","河北")) 
#check the selected stations in three Provinces
NCPstaions$Province <- factor(NCPstaions$Province)
table(NCPstaions$Province)
#load all the weather data
load("E:/Data/datav/气象数据1980to20181130.rda")
#get stationID
unique(NCPstaions$stationID)

wthtem <- DT_TEM[stationID %in% unique(NCPstaions$stationID)]
wthRHU <- DT_RHU[stationID %in% unique(NCPstaions$stationID)]
wthWIN <- DT_WIN[stationID %in% unique(NCPstaions$stationID)]
wthssd <- DT_SSD[stationID %in% unique(NCPstaions$stationID)]

wth <- merge(wthtem, wthRHU)
wth <- merge(wth, wthWIN)
wth <- merge(wth, wthssd)

wth2 <- unique(wth)
###########
#station info
head(NCPstaions)
stinfo <- NCPstaions[,c("stationID","LAT","elavation_ob")]
stinfo <- unique(stinfo)
nrow(stinfo)

#Merge the station info to the data
wth2s <- merge(wth2, stinfo ,by="stationID",allow.cartesian=TRUE,all = TRUE)
wth2s$Date <- as.Date(paste(wth2s$Year,wth2s$Month,wth2s$Day,sep='-'))
#DOY --day number of the year counting from first January
doyFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%j"))
}
wth2s$DOY <- doyFromDate(wth2s$Date)
#replace the missing data
summary(wth2s)
wth2s[wth2s==32766] <- NA
wth2s <- unique(wth2s)
#impute the missing data ####
#KNN插值
library(DMwR)
names(wth2s)

# too large data, cannot complete one time
sts <- unique(wth2s$stationID)
wth2sx <- data.table()
#连续缺失数据较多，无法进行KNN插补
for (st in sts) {
  #st <- 53399
  temp <- wth2s[stationID %in% st]
  im <- temp[,-c(1,14,15)]
  knnOutput <- knnImputation(im)  # 使用KNN插值.
  anyNA(knnOutput)
  temp_im <- cbind(temp[,c(1,14,15)],knnOutput)
  wth2sx <- rbind(wth2sx,temp_im)
}


wth2sx$ET0 <- ETpm(wth2sx$TEM_Max/10,wth2sx$TEM_Min/10,wth2sx$RHU,wth2sx$DOY,
                  wth2sx$LAT,wth2sx$SSD/10,wth2sx$elavation_ob,wth2sx$WIN_Mean/10)
# Penman formula function ####
ETpm <- function(TEM_Max, TEM_Min, RHU, DOY, LAT, SSD, ELV,WIN ) {
  #meant temp ####
  mT <- (TEM_Max + TEM_Min)/2
  #饱和水汽压
  eTmax <- 0.618*exp(17.27*TEM_Max/(TEM_Max+237.3))
  eTmin <- 0.618*exp(17.27*TEM_Min/(TEM_Min+237.3))
  #饱和水汽压并取平均值
  es <- (eTmax + eTmin)/2
  #实际水汽压
  ea <- RHU/100*es
  
  #温度-饱和水汽压关系曲线在 处的斜率deltaT
  delT <- 4098*0.618*exp(17.27*mT/(mT+237.3))/(mT + 237.3)^2
  # dr is inverse relative distance between Earth and Sun
  dr <- (1 + 0.033*cos(2*pi*(DOY)/365))
  # delta-- solar declination (radians)
  delta <- 0.409*sin(2*pi*(DOY)/365 - 1.39)
  # degree to radians
  phi <- pi/180*LAT
  # omega--sunset hour angle
  omega <- acos(-tan(delta)*tan(phi))
  # The maximum possible sunshine hours--N -- day length
  N <- 24/pi*(omega)
  #Extraterrestrial solar radiation H0
  Ra <- 37.6*dr*(omega*sin(phi)*sin(delta) + cos(phi)*cos(delta)*sin(omega))
  a=0.2;b=0.51 #from wuqiao county
  Rs <- Ra*(a+b*(SSD/N))
  #到达地球表面的晴空太阳辐射
  Rs0 <- (0.75 + 2/10^5*ELV)*Ra
  #净短波辐射
  Rns <- 0.77*Rs0
  #净长波辐射
  delr <- 4.903*10^(-9) #为斯蒂芬-玻尔兹曼（Stefan-Boltzmann）常数
  TmaxK <- TEM_Max + 273.16
  TminK <- TEM_Min + 273.16
  
  Rnl <- delr*((TmaxK^4 + TminK^4)/2)*(0.34-0.14*sqrt(ea))*(1.35*Rs/Rs0-0.35)
  #净辐射
  Rn <- Rns - Rnl
  #日尺度的土壤热通量相对很小，一般可以忽略
  #当地气压
  P <- 101.3*((293-0.0065*ELV)/293)^5.26
  #湿度表常数
  gama <- 0.665*10^(-3)
  #风标高度处风速与2m处风速换算
  z = 10 #风标高度10m
  miu2 <- WIN*4.87/(log(67.8*ELV-5.42))
  G = 0
  ET0 <- (0.408*delT*(Rn-G) + gama*900/(T + 273)*miu2*(es-ea))/(delT + gama*(1+0.34*miu2))
  return(ET0)
}






#load the weather data
load("E:/Data/datav/气象数据1980to20181130.rda")
#load the packages
library(data.table)
library(dplyr)
library(DMwR)
#calculate DTT of each day and stage ####
#1.提取温度、日照时数、降雨量的数据，并进行清洗
setwd("C:/Users/my/Desktop/生态源生态库")
station <- read.csv("试验点对应的气象站点.csv")
 
names(station)[c(2,5,8)] <- c("Location", "LAT","stationID")
unique(station$stationID)
wth1 <- DT_TEM[stationID %in% unique(station$stationID)]
wth2 <- DT_SSD[stationID %in% unique(station$stationID)]
wth3 <- DT_PRE[stationID %in% unique(station$stationID)]
#检查数据完整性####
#按站点统计每年的数据点(行)数
library(dplyr)
cnt1 <- wth1 %>% tbl_df() %>% group_by(stationID, Year) %>% summarise(counts =  n())
cnt2 <- wth2 %>% tbl_df() %>% group_by(stationID, Year) %>% summarise(counts =  n())
cnt3 <- wth3 %>% tbl_df() %>% group_by(stationID, Year) %>% summarise(counts =  n())
#没有缺失的日期,但是有重复数据#
#去掉重复数据#####
wth1 <- unique(wth1)
wth2 <- unique(wth2)
wth3 <- unique(wth3)

#合并数据
wth <- merge(wth1, wth2)
wth <- merge(wth, wth3)
unique(wth$stationID)

stinf <- station[,c("Location","stationID","Province","LAT")]
stinf <- unique(stinf)
nrow(stinf)
setDT(stinf)

#merge the name info to the data
wthnew <- merge(wth, stinf ,by="stationID",allow.cartesian=TRUE,all = TRUE)

summary(wthnew)
str(wthnew)

#### data clean 处理特殊数值####
# 国家级地面观测站历史日值数据，在历史气象日值数据处理过程中，针对降水量特征值处理方式如下：

# 1. 32700一般作为0mm进行处理；
#
# 2. 32XXX（除32744、32766外）的处理结果为（32XXX-32000）*0.1mm
#
# 3. 31XXX的处理结果为（31XXX-31000）*0.1mm
#
# 4. 30XXX的处理结果为（30XXX-30000）*0.1mm
#
# 5. 32766为数据缺测、32744为无数据，如出现，在统计前筛除。
#### clean all the data ####
setDT(wthnew)
wthnew[wthnew=="32700"] <- 0
wthnew[wthnew=="32766"] <- NA
summary(wthnew)

#降雨量仍然有部分特殊值需要处理
# 降水量	32700	表示降水"微量"
# 32XXX	XXX为纯雾露霜
# 31XXX	XXX为雨和雪的总量
# 30XXX	XXX为雪量(仅包括雨夹雪，雪暴）
           
wthnew$PRE[which(wthnew$PRE > 32000)] <- wthnew$PRE[which(wthnew$PRE > 32000)] - 32000
summary(wthnew)

#SSD数据缺失率为376/138800 = 0.27%，有连续整月数据缺失

#可以采用KNN插值的方式进行补齐 ####
anyNA(wthnew)
summary(wthnew)
#KNN插值
library(DMwR)
wth <- wthnew
names(wth)
im <- wth[,-c(1,10:12)]
knnOutput <- knnImputation(im)  # 使用KNN插值.
anyNA(knnOutput)
wth2 <- cbind(wth[,c(1,10:12)],knnOutput)

#或者对所有站点进行空间插值，补齐缺失数据
#利用所有站点的数据对SSD进行空间插值补齐，新的数据命名为DT_SSD2
wthnew <- wth2
# AcmTemp calculation fuction ####
# 适用于小麦积温计算 ####
AcmTemp <- function(TEM_Max, TEM_Min) {
  #TEM_Min=21.4 ; TEM_Max=23.1
  Tc <- (TEM_Min + TEM_Max)/2
  T0 = 0
  Th = 30
  DTT <-ifelse(Tc <= T0,T0, ifelse(Tc >= Th,Th,Tc -T0)) 
  return(DTT)
}
wthnew$DTTi <- AcmTemp(wthnew$TEM_Max/10, wthnew$TEM_Min/10)
# Rename the Month
wthnew$Mon <- factor(wthnew$Month, labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

library(dplyr)
DTTmonthly <- wthnew %>%
  group_by(stationID,Location,Province,Year,Month,Mon) %>%
  summarise(DTT_monthly = sum(DTTi),
            mTEM_Max = mean(TEM_Max/10),
            mTEM_Mean = mean(TEM_Mean/10),
            mTEM_Min = mean(TEM_Min/10))

#calculate SSD of each day and stage ####
wthnew$SSDi <- wthnew$SSD/10
#提取SSDi单独计算Solar radiation
# a function to run solar radaition estimate #####
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
wthnew$Date <- as.Date(paste(wthnew$Year, wthnew$Month, wthnew$Day, sep = '-'))
#Get DOY
doyFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%j"))
}

wthnew$DOY <- doyFromDate(wthnew$Date)
names(wthnew)
wthnew$SolaRad <- solarestmt(wthnew$LAT, wthnew$DOY,wthnew$SSDi)
#
# The monthly character of each statiion ####
names(wthnew)
SSDmonthly <- wthnew %>%
  group_by(stationID,Location,Province,Year,Month,Mon) %>%
  summarise(SSD_monthly = sum(SSDi),
            mSSD = mean(SSDi))

#calculate PRE of each day and stage ####

wthnew$PREi <- wthnew$PRE/10

#merge station name with weather data

PREmonthly <- wthnew %>%
  group_by(stationID,Location,Province,Year,Month,Mon) %>%
  summarise(PRE_monthly = sum(PREi))
  
  
#2017小麦生长季气象要素变化
# setDT(DTTmonthly)

wth_monthly <- merge(DTTmonthly,SSDmonthly)
wth_monthly <- merge(wth_monthly, PREmonthly)
wth_daily <- wthnew
#save data of the ten site in YieldGap Project ####
save(wth_daily, wth_monthly, file = "./data/Ten site Efactors dealed data.rda")



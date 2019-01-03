# Solar radiation estimate from SSD
library(openxlsx)
wqwth <- read.csv("吴桥县气象数据.csv",header=T)
wqwth[is.na(wqwth$日照时数),"日照时数"] <- 0
# The original A–P formula is
Rs/Ra =  a + b(n/N)  
#Clac params
doyFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%j"))
}
#DOY --day number of the year counting from first January
wqwth$DOY <- doyFromDate(wqwth$Date)

#Method 1 --ref:
#constant --latitude of the place
# degree to radians
LAT <- wqwth$纬度[1]/100 # 当地纬度
phi <- pi/180*LAT

# Gsc -- solar constant
# Gsc is the solar constant (0.082 MJ m^2 min^1) the metric different
Gsc = 0.082
# delta-- solar declination (radians)
wqwth$delta <- 0.409*sin(2*pi*(wqwth$DOY)/365 - 1.39)

# omega--sunset hour angle
wqwth$omega <- acos(-tan(wqwth$delta)*tan(phi))
# The maximum possible sunshine hours--N -- day length
wqwth$N <- 24/pi*(wqwth$omega)

# dr is inverse relative distance between Earth and Sun
wqwth$dr <- (1 + 0.033*cos(2*pi*(wqwth$DOY)/365))

#Extraterrestrial solar radiation H0
wqwth$Ra <- (24*60)/pi*Gsc*wqwth$dr*(wqwth$omega*sin(phi)*sin(wqwth$delta) + cos(phi)*cos(wqwth$delta)*sin(wqwth$omega))

# The indicative calues for empirical constants in the Angstrom formula in relation
# to latitude and climate used by FAO (Frere & Popov, 1979)
# Cold and Temperate Zones: a=0.18, b=0.55
# Dry tropical zones:       a=0.25, b=0.45
# Humid tropical zones:     a=0.29, b=0.42

a=0.18;b=0.55
a=0.2;b=0.51
wqwth$Rs <- wqwth$Ra*(a+b*(wqwth$日照时数/wqwth$N))


################
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.1  January 2009

yearFromDate <- function(date) {
  # date is a string like "2007-7-10"    YYYY-M-D
  # to avoid date shifts because of your local time zone if date is a POSIX. 
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%Y"))
}

monthFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%m"))
}

dayFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%d"))
}

doyFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%j"))
}

isLeapYear <- function(year) {
  year <- round(year)
  return( ((year %% 100 != 0) & (year %%4 ==0)) | (year %% 400==0) )
}

daysInYear <- function(year) {
  ifelse(isLeapYear(year), 366, 365)
}

daysOfYear <- function(year) {
  if (length(year) > 1) {
    stop('this function only accepts a single year as an argument')
  }
  firstday <- as.Date(paste(year, "-1-1", sep=""))
  lastday <- as.Date(paste(year, "-12-31", sep=""))
  d <- seq(firstday, to=lastday, by=1)
  return(d)
}	

dateFromDoy <- function(doy, year) {
  year <- round(year)
  doy <- round(doy)
  return(as.Date(doy, origin=paste(year-1, "-12-31", sep='')))
}
################
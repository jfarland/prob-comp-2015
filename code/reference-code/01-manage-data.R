#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# Short Term Load Forecasting Competiion - Tao Hong's Energy Analytics Course
#
# Prepare and manage data sets used in forecasting
#
# Author: Jon T Farland <jonfarland@gmail.com>
#
# Copywright September 2015
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#

#plotting and visual libraries
library("ggplot2")
library("lattice")
library("rworldmap")

#data management libraries
library("dplyr")
library("tidyr")
library("gdata")
library("reshape2")

#modeling and forecast libraries
library("forecast")

#weather daya forecasts
library("weatherData")




#-----------------------------------------------------------------------------#
#
# Setup / Options
#
#-----------------------------------------------------------------------------#

# Current Directory
getwd()

#set the raw data as the current directory
setwd("/home/rstudio/projects/comp-2015/data/rawdat")

#-----------------------------------------------------------------------------#
#
# Load Inputs
#
#-----------------------------------------------------------------------------#


#uncomment the next command to run a Python script to download PJM load data for the last 5 years
#system('python /home/rstudio/projects/comp-2015/data/rawdat/00-pull-historical-load-data.py')

#download just the 2015 data as the competition ensues
system('python /home/rstudio/projects/comp-2015/data/rawdat/00-pull-2015-load-data.py')

#system('python /home/rstudio/projects/comp-2015/data/rawdat/00-pull-2015-load-data.py')

#Read in only the dominion tab of the excel spreadsheets
load11 <- read.xls("load11.xls", sheet=22) %>%
  select(DATE:HE24)
load12 <- read.xls("load12.xls", sheet=22) %>%
  select(DATE:HE24)
load13 <- read.xls("load13.xls", sheet=22) %>%
  select(DATE:HE24)
load14 <- read.xls("load14.xls", sheet=22) %>%
  select(DATE:HE24)
load15 <- read.xls("load15.xls", sheet=22) %>%
  select(DATE:HE24)


#2015 data goes up until 10/1. We're going to need to download the other preliminary files as well
str02 <- readLines("20151002_dailyload.csv")
prelim02 <- read.csv(text=str02,skip=2)

str03 <- readLines("20151003_dailyload.csv")
prelim03 <- read.csv(text=str03,skip=2)

str04 <- readLines("20151004_dailyload.csv")
prelim04 <- read.csv(text=str04,skip=2)

str05 <- readLines("20151005_dailyload.csv")
prelim05 <- read.csv(text=str05,skip=2)

str06 <- readLines("20151006_dailyload.csv")
prelim06 <- read.csv(text=str06,skip=2)

str07 <- readLines("20151007_dailyload.csv")
prelim07 <- read.csv(text=str07,skip=2)


#-----------------------------------------------------------------------------#
#
# Processing
#
#-----------------------------------------------------------------------------#

load.data=rbind(load11, load12, load13, load14, load15)

#go from wide to long
load.long <- melt(load.data, id=c("DATE", "COMP")) %>%
  rename(hour = variable, load = value) %>%
  mutate(tindx = mdy_h(paste(DATE, substr(hour, 3, 4)))-duration(1,"hours"),
         hindx = hour(tindx),
         dindx = as.Date(tindx),
         mindx = month(tindx),
         dow   = weekdays(tindx)) %>%
  select(tindx, hindx, dindx, mindx, load, dow) %>%
  arrange(dindx, hindx)

#stack preliminary data
prelim.data = rbind(prelim02, prelim03, prelim04, prelim05, prelim06, prelim07) %>%
  select(Date, HourEnd, LoadAvgHourlyDOM) %>%
  rename(hour = HourEnd, load = LoadAvgHourlyDOM) %>%
  mutate(tindx = mdy_h(paste(Date, hour))-duration(1,"hours"),
         hindx = hour(tindx),
         dindx = as.Date(tindx),
         mindx = month(tindx),
         dow   = weekdays(tindx)) %>%
  select(tindx, hindx, dindx, mindx, load, dow) %>%
  arrange(dindx, hindx)

#stack historical and preliminary metering
load.long = rbind(load.long, prelim.data)

#shifted to hour beginning rather than hour ending

#quick checks
summary(load.long)

#-----------------------------------------------------------------------------#
#
# Graphics
#
#-----------------------------------------------------------------------------#

# load over time
plot1 <- plot(load.long$load ~ load.long$tindx)
plot2 <- plot(load.long$load ~ load.long$hindx)
plot3 <- plot(load.long$load ~ load.long$dindx)

#histograms and conditional histograms
histogram(~load | mindx, data = load.long, xlab="Load (MW)", ylab ="Density", col=c("red"))
histogram(~load | hindx, data = load.long, xlab="Load (MW)", ylab ="Density", col=c("red"))
histogram(~load , data = load.long, xlab="Load (MW)", ylab ="Density", col=c("red"))

#-----------------------------------------------------------------------------#
#
# Weather Data
#
#-----------------------------------------------------------------------------#

#use weatherData to pull data from Weather Underground
View(USAirportWeatherStations)

VA_stat <- 
  subset(USAirportWeatherStations, State=="VA")

View(VA_stat)

#map weather stations
newmap<- getMap(resolution="low")
plot(newmap, xlim=c(-81,-70),ylim=c(30,40))
points(VA_stat$Lon, VA_stat$Lat, col ="red")


#pull weather data

beg <- as.Date('2011/01/01',format= "%Y/%m/%d")
end <- as.Date('2015/10/08',format= "%Y/%m/%d")

s <- seq(beg, to = end, by = 'days')

wx_df <- list()

#wx_df <- getDetailedWeather("RIC", "2015-01-01", opt_all_columns = T)

# for ( i in seq_along(s))
# {
#   print(i)
#   print(s[i])
#   wx_df[[i]]<-getDetailedWeather("RIC", s[i], opt_all_columns = T)
#   wx_df[[i]]$Wind_SpeedMPH[wx_df[[i]]$Wind_SpeedMPH %in% ("Calm")] = 0
#   wx_df[[i]]$Wind_SpeedMPH = as.numeric(wx_df[[i]]$Wind_SpeedMPH)
# }

for ( i in seq_along(s))
{
  print(i)
  print(s[i])
  wx_df[[i]]<-getDetailedWeather("RIC", s[i], opt_temperature_columns = T)
}


#unpack the list
weather <- bind_rows(wx_df) %>%
  mutate(tindx = floor_date(Time, "hour"),
         hindx = hour(tindx),
         dindx = as.Date(Time),
         TemperatureF = replace(TemperatureF, TemperatureF < -1000, lag(TemperatureF, n=1))) %>%
  group_by(dindx,hindx) %>%
  summarize(TemperatureF = mean(TemperatureF)) %>%
  as.data.frame


summary(weather)
class(weather)
plot(weather$TemperatureF)

forecast <- read.csv("temp-forecasts-2015-10-08.csv") %>%
  mutate(tindx = ISOdatetime(year,mindx,dindx,hindx,0,0))

#quick plot
plot(forecast$tindx,forecast$temp)

#trim data and stack
temp_act <- 
  subset(weather, dindx != "2015-10-09") %>%
  rename(temp = TemperatureF) %>%
  mutate(type = "act",
         tindx = ymd_h(paste(dindx, hindx))) %>%
  select(temp, type, tindx)

temp_fcst <-
  subset(forecast, dindx != 8 | mindx != 10 ) %>%
  mutate(type = "fcst") %>%
  select(temp, type, tindx)

temp_final <-
  rbind(temp_act, temp_fcst)

#-----------------------------------------------------------------------------#
#
# Outputs
#
#-----------------------------------------------------------------------------#


#save out the data
setwd("/home/rstudio/projects/comp-2015/data")
save(load.long,file="load-long.Rda")
save(temp_act,file="temp-act.Rda")
save(temp_fcst,file="temp-fcst.Rda")


write.csv()
writeRDS()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
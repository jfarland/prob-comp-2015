#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# Probabilistic Forecasting Competiion - Tao Hong's Energy Analytics Course
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
library("lubridate")
library("timeDate")

#modeling and forecast libraries
library("forecast")
library("quantreg")
library("splines")
library("quantregForest")
library("mgcv")

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
setwd("/home/rstudio/projects/prob-comp-2015/data/rawdat")

#-----------------------------------------------------------------------------#
#
# Load Inputs
#
#-----------------------------------------------------------------------------#


#uncomment the next command to run a Python script to download PJM load data for the last 5 years
#system('python /home/rstudio/projects/comp-2015/data/rawdat/00-pull-historical-load-data.py')

#download just the 2015 data as the competition ensues
#system('python /home/rstudio/projects/comp-2015/data/rawdat/00-pull-2015-load-data.py')

#system('python /home/rstudio/projects/comp-2015/data/rawdat/00-pull-2015-load-data.py')


load0 <- read.csv("Release_1.csv") 

load1 <- read.csv("Release_2.csv")

names(load0)
sapply(load0,class)

plot(load0$T,load0$load)
plot(load0$Hour,load0$load)

#-----------------------------------------------------------------------------#
#
# Processing
#
#-----------------------------------------------------------------------------#

#load.data=rbind(load11, load12, load13, load14, load15)

#go from wide to long
load.long <- rbind(load0, load1) %>%
  mutate(tindx = mdy_h(paste(Date, Hour))-duration(1,"hours"),
         mindx = month(tindx),
         dow   = weekdays(tindx),
         year  = year(tindx)) %>%
  rename(dindx = Date, hindx = Hour, temp=T) %>%
  select(tindx, hindx, dindx, mindx, year, load, dow, temp) %>%
  arrange(dindx, hindx)

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
# Outputs
#
#-----------------------------------------------------------------------------#


#save out the data
setwd("/home/rstudio/projects/prob-comp-2015/data")
save(load.long,file="load-long.Rda")
#save(temp_act,file="temp-act.Rda")
#save(temp_fcst,file="temp-fcst.Rda")


write.csv()
writeRDS()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# Probabilistic Forecasting Competiion - Tao Hong's Energy Analytics Course
#
# Prepare and manage data sets used in forecasting
#
# Author: Jon T Farland <jonfarland@gmail.com>
#
# Copywright October 2015
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#

library("ggplot2")
library("lattice")
library("dplyr")
library("tidyr")
library("gdata")
library("reshape2")

library("lubridate")
library("timeDate")

library("forecast")
library("quantreg")
library("splines")
library("quantregForest")
library("mgcv")

#-----------------------------------------------------------------------------#
#
# Setup / Options
#
#-----------------------------------------------------------------------------#

# Current Directory
getwd()

#set the raw data as the current directory
setwd("/home/rstudio/projects/prob-comp-2015/data/")

#-----------------------------------------------------------------------------#
#
# Load Inputs - Use the most recent data for production level forecast
#
#-----------------------------------------------------------------------------#

load("model_dat.Rda")

model_dat <-
  model_dat %>%
  filter(!is.na(lag168))

load("mean_weather.Rda")

#estimate benchmark model to get some forecast of load in 2010
lmFit.NoLag <- lm(load~ temp + temp2 + temp3 + dow + dow*temp + dow*temp2 + dow*temp3 +
                    mindx + mindx*temp2 * mindx*temp3 +
                    factor(hindx) * temp + holiday, data=model_dat)

#-----------------------------------------------------------------------------#
#
# Create Design Matrix for 2010 Forecast
#
#-----------------------------------------------------------------------------#

setwd("/home/rstudio/projects/prob-comp-2015/data/rawdat")


#read in template and attached some simulated weather
forecast_datetime <- 
  read.csv("template.csv") %>%
  mutate(tindx = mdy_h(paste(Date, Hour))-duration(1,"hours"),
         mindx = month(tindx),
         dow   = weekdays(tindx),
         year  = year(tindx)) %>%
  rename(dindx = Date, hindx = Hour) %>%
  select(tindx, hindx, dindx, mindx, year, dow) %>%
  arrange(dindx, hindx, dow) %>%
  left_join(means, by = c("hindx", "dow", "mindx")) %>%
  rename(temp = mean_temp) %>%
  mutate(temp2 = temp * temp,
         temp3 = temp * temp * temp)

#create necessary indicators for forecast
holidays <- c(as.Date(USMemorialDay(2006)),
              as.Date(USMemorialDay(2007)),
              as.Date(USMemorialDay(2008)),
              as.Date(USMemorialDay(2009)),
              as.Date(USLaborDay(2006)),
              as.Date(USLaborDay(2007)),
              as.Date(USLaborDay(2008)),
              as.Date(USLaborDay(2009)),
              as.Date(USChristmasDay(2006)),
              as.Date(USChristmasDay(2007)),
              as.Date(USChristmasDay(2008)),
              as.Date(USChristmasDay(2009)),
              as.Date(USIndependenceDay(2006)),
              as.Date(USIndependenceDay(2007)),
              as.Date(USIndependenceDay(2008)),
              as.Date(USIndependenceDay(2009)),
              as.Date(USNewYearsDay(2006)),
              as.Date(USNewYearsDay(2007)),
              as.Date(USNewYearsDay(2008)),
              as.Date(USNewYearsDay(2009)),
              as.Date(USMLKingsBirthday(2006)),
              as.Date(USMLKingsBirthday(2007)),
              as.Date(USMLKingsBirthday(2008)),
              as.Date(USMLKingsBirthday(2009)), 
              as.Date(USThanksgivingDay(2006)),
              as.Date(USThanksgivingDay(2007)),
              as.Date(USThanksgivingDay(2008)),
              as.Date(USThanksgivingDay(2009)))


forecast_datetime$holiday <- ifelse(as.Date(forecast_datetime$dindx) %in% holidays, 1, 0)

#forecast of load using Tao's reference benchmark
forecast_datetime$load <- predict(lmFit.NoLag, newdata=forecast_datetime)


#stack actual and simulated data sets
forecast_final <-
    model_dat %>%
    select(tindx, dindx, mindx, hindx, year, dow, load, holiday, temp, temp2, temp3) %>%
    rbind(forecast_datetime) %>%
    arrange(tindx)


#finally create lags based off of reference model prediction and actual load
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

#create lag variables of load
displacements <- seq(24, 168, 24)

#vector of column names
lagnames <- paste("lag", displacements, sep = "")

cols <- dim(forecast_final)[2] #number of columns before we add lags

for (i in 1 : length(displacements))
{
  disp = displacements[i]
  forecast_final[,i+cols] <- unlist(shift(forecast_final$load, -1*disp))
  colnames(forecast_final)[c(i+cols)] = lagnames[i]
}

forecast_final$forecast <- ifelse(forecast_final$year > 2010, 1, 0)

forecast_final <- filter(forecast_final, !is.na(lag168))

train     <- filter(forecast_final, year < 2010) %>%
  filter(!is.na(lag168))

forecasts <- filter(forecast_final, year >= 2010) 


#-----------------------------------------------------------------------------#
#
# Fit Models and Produce "Sister Forecasts for 2010"
#
#-----------------------------------------------------------------------------#

### [1] - Semiparametric Hourly Models ####

hours = c(seq(1,24,by=1))

spFit = list()
fcst1 = list()

for(i in 1:length(hours)){spTrain        <- subset(train, hindx==hours[i])
                          x              <- model.matrix(load~bs(temp,df=6)+factor(dow)+factor(mindx)+holiday+lag24+lag48+lag72+lag96+lag120+lag144+lag168, data=subset(forecast_final, hindx==hours[i]))
                          spFit[[i]]     <- gam(load~bs(temp,df=6)+factor(dow)+factor(mindx)+holiday+lag24+lag48+lag72+lag96+lag120+lag144+lag168, data=subset(train, hindx==hours[i]))
                          fcst1[[i]]     <- x %*% spFit[[i]]$coef
}


timestamp <- arrange(forecast_final, hindx)
spFcst    <- unlist(fcst1)

forecast_f1  <- cbind(timestamp,spFcst) %>%
  mutate(f1_error = load-spFcst,
         f1_ape   = abs(f1_error/load))

summary(forecast_f1)

### [2] Naive Forecast - Straight Average ###

lmFit <- lm(load~ temp + temp2 + temp3 + dow + dow*temp + dow*temp2 + dow*temp3 +
                  factor(mindx) + factor(mindx)*temp2 + factor(mindx)*temp3 +
                  factor(hindx) +
                  holiday +
                  lag24+lag48+lag72+lag96+lag120+lag144+lag168 , data=train)


forecast_f1$lmFcst <- predict(lmFit, newdata=forecast_final)

forecast_f2 <- 
  forecast_f1 %>%
  mutate(f2_error = load-lmFcst,
         f2_ape   = abs(f2_error/load))

summary(forecast_f2)

### [4] Naive Forecast - Load Shapes ###

load_shapes <-
  train %>%
  group_by(hindx,mindx,dow) %>%
  summarize(mean_kwh = mean(load))

summary(load_shapes)

#merge second naive forecast
forecast_f3 <-
  left_join(forecast_f2, load_shapes, by = c("hindx", "dow", "mindx")) %>%
  rename(meanFcst = mean_kwh) %>%
  mutate(f3_error = load - meanFcst,
         f3_ape   = abs(f3_error/load))



#-----------------------------------------------------------------------------#
#
# Production level forecasts
#
#-----------------------------------------------------------------------------#

#Quantile Regression

final_train <- subset(forecast_f3, year < 2011)

#SORT THE DATA SET!
final_fcst <- subset(forecast_f3, year >= 2011) %>%
  arrange(tindx) %>%
  select(-load) 

rq1 = rq(load ~ spFcst + lmFcst + meanFcst, tau=seq(0.01, 0.99, 0.01), final_train)

postscript("quintiles.pdf", horizontal = FALSE, width = 6.5, height = 3.5)
plot(rq1, nrow=1, ncol=2)
dev.off()

#fit values
fcst <- predict.rq(rq1, newdata=final_fcst)

#-----------------------------------------------------------------------------#
#
# Plots
#
#-----------------------------------------------------------------------------#

setwd("/home/rstudio/projects/prob-comp-2015/data/rawdat")

round1_actuals <- read.csv("Release_2.csv") %>%
  rename(act_load = load,
         act_temp = T) %>%
  select(act_load, act_temp)

forecast_act <- cbind(forecasts, round1_actuals)

load_forecasts <- 
  final_fcst %>%
  select(spFcst, lmFcst, meanFcst) %>%
  cbind(forecast_act) %>%
  rename(fcst_load = load, fcst_temp = temp)


ggplot(final_train,aes(x=tindx,y=load))


#-----------------------------------------------------------------------------#
#
# Outputs
#
#-----------------------------------------------------------------------------#

#export data to csv
write.table(fcst, "/home/rstudio/projects/prob-comp-2015/results/forecasts_week2.csv", sep=",")
write.table(load_forecasts, "/home/rstudio/projects/prob-comp-2015/data/evaluation_week2.csv", sep=",")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
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
# Load Inputs
#
#-----------------------------------------------------------------------------#

load("load-long.Rda")
#load("temp-final.Rda")

weather <-
  load.long %>%
  arrange(tindx) %>%
  filter(!is.na(temp)) %>%
  select(-load)


### [4] Naive Forecast - Load Shapes ###

means <-
  weather %>%
  group_by(hindx,mindx,dow) %>%
  summarize(mean_temp = mean(temp))

summary(means)
tail(means)
tail(weather)

setwd("/home/rstudio/projects/prob-comp-2015/data/")

#output merged temperature
save(means, file="mean_weather.Rda")




#merge second naive forecast
naive2 <-
  left_join(train_f2, means, by = c("hindx", "dow", "mindx")) %>%
  rename(meanFcst = mean_kwh) %>%
  mutate(f3_error = load - meanFcst,
         f3_ape   = abs(f3_error/load))

View(naive2)



### [5] traditional time series forecast ###

etsFcst <-forecast(weather$temp, h=8760)

#plot traditional forecasts
plot.forecast(etsFcst, plot.conf=TRUE, xlab=" ", ylab=" ", 
              main="Univariate Time Series")

#performance metrics of the traditional time series forecast
accuracy(etsFcst)

#view forecasts and prediction intervals
summary(etsFcst)


### [6] Neural Network Time Series Forecast ###

nnet  <-nnetar(y$load)

#use the neural net to produce forecasts
fcst2 <- forecast(nnet)

#plot traditional forecasts
plot.forecast(fcst2, plot.conf=TRUE, xlab=" ", ylab=" ",
              main="Artificial Intelligence")#, ylim = c(0,25))

#performance metrics of the traditional time series forecast
accuracy(fcst2)

#view forecasts 
summary(fcst2)


#-----------------------------------------------------------------------------#
#
# Quintile Regression - Multivariate
#
#-----------------------------------------------------------------------------#


#Quantile Regression
rq1 = rq(load~temp+temp2+temp3+factor(dow)+factor(hindx)+factor(mindx)+lag24+lag48+lag72+lag96+lag120+lag144+lag168,tau=seq(0.01, 0.99, 0.01), load_weather)

postscript("quintiles.pdf", horizontal = FALSE, width = 6.5, height = 3.5)
plot(rq1, nrow=1, ncol=2)
dev.off()


#-----------------------------------------------------------------------------#
#
# Production level forecasts
#
#-----------------------------------------------------------------------------#

#make production forecast dataset
#step 1 - time series forecast of load to create lags

#only keep predictors and forecasted temperature within range

taus <- c(seq(0.01,0.99,by=0.01))

forecast_datetime <- 
  seq(as.Date("2010-01-01"), as.Date("2010-12-31"), by="1 hour")



for( i in 1:length(taus)){rqFit<-rq(load~ spFcst + meanFcst + etsFcst + , data = train, tau=taus[i])
                          fcst1 <- X %*% fit$coef}


#with-in sample MAPE
train_ape <- abs(fit$residuals / train$load)

summary(train_ape)

#out-of-sample MAPE
X = model.matrix(load~bs(temp,df=6)+dow+mindx+lag24+lag48+lag72+lag96+lag120+lag144+lag168, data = test)

for( i in 1:length(taus)){test.fit <- X %*% fit$coef}

test_ape <- abs((test.fit-test$load)/test$load)

summary(test_ape)

names(fit)


















#take forecasted timestamp and left join load


act <- model_dat %>%
  filter(dindx > "2015-09-01") %>%
  mutate(type = "act") %>%
  select(load,temp,dindx,hindx,mindx,dow,type)

fcst_1 <-
  temp_fcst %>%
  mutate(dindx = as.Date(tindx),
         hindx = hour(tindx),
         mindx = month(dindx),
         dow = weekdays(dindx),
         load = NA,
         type = "fcst") %>%
  select(load,temp,dindx,hindx,mindx,dow,type)


fcst_2 <-
  rbind(act, fcst_1)

#now create lags using most recent load data

cols <- dim(fcst_2)[2] #number of columns before we add lags

for (i in 1 : length(displacements))
{
  disp = displacements[i]
  fcst_2[,i+cols] <- unlist(shift(fcst_2$load, -1*disp))
  colnames(fcst_2)[c(i+cols)] = lagnames[i]
}


#isolate date to forecast using fitted model

forecast <-
  fcst_2 %>%
  filter(dindx > "2015-10-09") %>%
  filter(dindx < "2015-10-11") %>%
  select(temp,dindx,hindx,mindx,dow,type,lag72,lag96,lag120,lag144,lag168)

summary(forecast)

fcst_prime <- predict(sm1,newdata=forecast)

summary(fcst_prime)
plot(fcst_prime)

#todo: output forecasts         


#-----------------------------------------------------------------------------#
#
# Outputs
#
#-----------------------------------------------------------------------------#



write.csv(fcst_prime)
writeRDS()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
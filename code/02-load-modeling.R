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

load_weather <-
  load.long %>%
  arrange(tindx) %>%
  filter(!is.na(load))


#-----------------------------------------------------------------------------#
#
# Prepare Modeling Data Set
#
#-----------------------------------------------------------------------------#

#create lag and lead variables
#source <- http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
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

cols <- dim(load_weather)[2] #number of columns before we add lags

for (i in 1 : length(displacements))
{
  disp = displacements[i]
  load_weather[,i+cols] <- unlist(shift(load_weather$load, -1*disp))
  colnames(load_weather)[c(i+cols)] = lagnames[i]
}

#create holiday indicators: 
#source: http://stackoverflow.com/questions/19138309/create-indicator-variables-of-holidays-from-a-date-column


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


load_weather$holiday <- ifelse(as.Date(load_weather$dindx) %in% holidays, 1, 0)

#finally create higher order temperature variables

model_dat <-
  load_weather %>%
  mutate(temp2 = temp*temp,
         temp3 = temp*temp*temp)  

#summary(model_dat)
#tail(model_dat,100)

setwd("/home/rstudio/projects/prob-comp-2015/data")
save(model_dat,file="model_dat.Rda")



#-----------------------------------------------------------------------------#
#
# Assign Training and Testing Data Sets
#
#-----------------------------------------------------------------------------#

train <- filter(model_dat, year < 2010) %>%
  filter(!is.na(lag168))

test  <- filter(model_dat, year >= 2010)



#-----------------------------------------------------------------------------#
#
# Temperature - Load Spline Quintile Fit
#
#-----------------------------------------------------------------------------#

#set up new plot for lines to be plotted on top of each other
plot(train$temp, train$load, xlab="Temperature", ylab="Load")

#design matrix for model spec
X = model.matrix(load~bs(temp,df=10), data = train)

taus <- c(0.10,0.20,0.50,0.80,0.90)
for( i in 1:length(taus)){fit<-rq(load~bs(temp,df=10), data = train, tau=taus[i])
                          temp.fit <- X %*% fit$coef
                          lines(train$temp,temp.fit,col="red")
}

plot(fitted(fit), residuals(fit))
#gam.check(fit[1])
#summary(fit)
#plot(fit)
#anova(fit)

#accuracy(fit)
#vis.gam(fit, view=c("temp","temp2"),theta=200, thicktype="detailed",)

#-----------------------------------------------------------------------------#
#
# Produce Sister Forecasts for use in QRA
#
#-----------------------------------------------------------------------------#

### [1] - Semiparametric Hourly Models ####


#try using a for loop and model.matrix to multiplty out

hours = c(seq(1,24,by=1))

spFit = list()
fcst1 = list()

for(i in 1:length(hours)){spTrain        <- subset(train, hindx==hours[i])
                          x              <- model.matrix(load~bs(temp,df=6)+factor(dow)+factor(mindx)+holiday+lag24+lag48+lag72+lag96+lag120+lag144+lag168, data=subset(train, hindx==hours[i]))
                          spFit[[i]]     <- gam(load~bs(temp,df=6)+factor(dow)+factor(mindx)+holiday+lag24+lag48+lag72+lag96+lag120+lag144+lag168, data = spTrain)
                          fcst1[[i]]     <- x %*% spFit[[i]]$coef
}

timestamp <- arrange(train, hindx)
spFcst    <- unlist(fcst1)

train_f1  <- cbind(timestamp,spFcst) %>%
  mutate(f1_error = load-spFcst,
         f1_ape   = abs(f1_error/load))

summary(train_f1)


### [2] Linear Regression Models ###
# 
# lmFit = list()
# fcst2 = list()
# 
# for(i in 1:length(hours)){x              <- model.matrix(load ~ temp + temp2 + temp3 + factor(dow) + factor(mindx) + holiday + lag24 + lag48 + lag72 + lag96 + lag120 + lag144 + lag168, data=subset(train, hindx==hours[i]))
#                           lmFit[[i]]     <- lm(load ~ temp + temp2 + temp3 + factor(dow) + factor(mindx) + holiday + lag24 + lag48 + lag72 + lag96 + lag120 + lag144 + lag168, data = subset(train, hindx==hours[i]))
#                           fcst2[[i]]     <- x %*% lmFit[[i]]$coef
#                           }
# 
# timestamp <- arrange(train_f1, hindx)
# lmFcst    <- unlist(fcst2)
# 
# train_f2  <- cbind(timestamp,lmFcst) %>%
#   mutate(f2_error = load-lmFcst,
#          f2_ape   = abs(f1_error/load))
# 
# lmFits <-
#   train %>%
#   group_by(hindx) %>%
#   do(model1 = lm(load~ temp + temp2 + temp3 + dow + mindx + holiday + lag24+lag48+lag72+lag96+lag120+lag144+lag168, data = ., keepData = TRUE))
# 
# 
# 
# #TO DO: fix with presence of factors in design matrix
# params <- lapply(lmFits$model, coef)
# 
# fcts <-
#   Map(function(x, p) { cbind(1, x$model %>% select(-load) %>% as.matrix()) %*% p},
#       lmFits$model1,
#       params)
# 




lmFit.Lag <- lm(load~ temp + temp2 + temp3 + dow + dow*temp + dow*temp2 + dow*temp3 +
                    mindx + mindx*temp2 * mindx*temp3 +
                    factor(hindx) * temp + holiday +
                    lag24+lag48+lag72+lag96+lag120+lag144+lag168, data=model_dat)




### [3] Naive Forecast - Straight Average ###

#assign univariate vector of load
y <-
  train %>% 
  select(tindx, load)

naive1 <- naive(y$load)

#plot the naive forecasts
plot.forecast(naive1, plot.conf=TRUE, xlab=" ", ylab=" ",
              main="NAIVE", ) #ylim = c(0,25))

#performance metrics of the naive forecast
accuracy(naive1)

naiveFcst <- data.frame(naive1$fitted) 

train_f2 <- cbind(train_f1, naiveFcst) 


### [4] Naive Forecast - Load Shapes ###

means <-
  train %>%
  group_by(hindx,mindx,dow) %>%
  summarize(mean_kwh = mean(load))

summary(means)

#merge second naive forecast
naive2 <-
  left_join(train_f2, means, by = c("hindx", "dow", "mindx")) %>%
  rename(meanFcst = mean_kwh) %>%
  mutate(f3_error = load - meanFcst,
         f3_ape   = abs(f3_error/load))

View(naive2)

### [5] traditional time series forecast ###

etsFcst <-forecast(y$load, h=72)

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
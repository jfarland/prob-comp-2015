#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# [Project Title] - [Project Number]
#
# [Purpose of the code]
#
# Author  : Jonathan Farland <jon.farland@dnvgl.com>
#
# Date    : 
# Weather Underground reference for REST API: http://www.wunderground.com/weather/api/d/docs
#
#
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#



#-----------------------------------------------------------------------------#
#
# Setup / Options
#
#-----------------------------------------------------------------------------#

# Current Directory
getwd()
setwd()

#libraries
library("ggplot2")
library("dplyr")

library("RJSONIO")
library("plyr")



#-----------------------------------------------------------------------------#
#
# Load Inputs
#
#-----------------------------------------------------------------------------#

#csv file
data<-read.csv()

#R data set
data<-readRDS()


#-----------------------------------------------------------------------------#
#
# Processing
#
#-----------------------------------------------------------------------------#

getHistoricalWeather <- function(airport.code="SFO", date="Sys.Date()")
{
  base.url <- 'http://api.wunderground.com/api/c9be88eb723364fc/'
  # compose final url
  final.url <- paste(base.url, 'history_', date, '/q/', airport.code, '.json', sep='')
  
  
  # reading in as raw lines from the web service
  conn <- url(final.url)
  raw.data <- readLines(conn, n=-1L, ok=TRUE)
  
  # Convert to a JSON
  weather.data <- fromJSON(paste(raw.data, collapse=""))
  close(conn)
  return(weather.data)
}



# get data for 10 days - restriction by Weather Underground for free usage
date.range <- seq.Date(from=as.Date('2006-1-01'), to=as.Date('2006-1-10'), by='1 day')


# Initialize a data frame
hdwd <- data.frame()



# loop over dates, and fetch weather data
for(i in seq_along(date.range)) {
  weather.data <- getHistoricalWeather('BOS', format(date.range[i], "%Y%m%d"))                 
  hdwd <- rbind(hdwd, ldply(weather.data$history$dailysummary, 
                            function(x) c('SJC', date.range[i], x$fog, x$rain, x$snow,  x$meantempi, x$meanvism, x$maxtempi, x$mintempi)))
}
colnames(hdwd) <- c("Airport", "Date", 'Fog', 'Rain', 'Snow','AvgTemp', 'AvgVisibility','MaxTemp','MinTemp')


#-----------------------------------------------------------------------------#
#
# Outputs
#
#-----------------------------------------------------------------------------#

# save to CSV
write.csv(hdwd, file=gzfile('SFC-Jan2006.csv.gz'), row.names=FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#

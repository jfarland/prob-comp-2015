import sys, csv, json, urllib2, requests, datetime

f = urllib2.urlopen('http://api.wunderground.com/api/c9be88eb723364fc/hourly10day/q/VA/Richmond.json')

json_string = f.read()
parsed_json = json.loads(json_string)

#look at the top level
parsed_json.keys()

#check to make sure it's a dictionary
type(parsed_json[u'hourly_forecast'])

#we could output the json file as text now
with open('json-file.txt', 'w') as outfile:
  json.dump(parsed_json, outfile)

#look at just the forecasts
forecasts = parsed_json[u'hourly_forecast']

#we could pull apart each datetime element
#temp  = [int(x[u'temp'][u'english']) for x in forecasts]
#hour  = [int(x[u'FCTTIME'][u'hour']) for x in forecasts]
#year  = [int(x[u'FCTTIME'][u'year']) for x in forecasts]
#month = [int(x[u'FCTTIME'][u'mon'])  for x in forecasts]
#day   = [int(x[u'FCTTIME'][u'mday']) for x in forecasts]

#make lists of dates and time
#date =[datetime.date(2015,10,d) for d in days]
#time =[datetime.time(h,0,0) for h in hour]

#attempt to combine the data
#datetime = [datetime.datetime.combine(d,t) for d in date]

#do it all concisely and save as tuples.
data = [(int(x[u'temp'][u'english']),int(x[u'FCTTIME'][u'hour']), int(x[u'FCTTIME'][u'year']),int(x[u'FCTTIME'][u'mon']),int(x[u'FCTTIME'][u'mday']))for x in forecasts]

#assign the name of the output file with a time-stamp
filename = 'temp-forecasts-%s.csv'%datetime.date.today().strftime('%Y-%m-%d')

with open(filename,'w') as out:
  csv_out=csv.writer(out)
  csv_out.writerow(['temp','hindx','year','mindx','dindx'])
  for row in data:
    csv_out.writerow(row)

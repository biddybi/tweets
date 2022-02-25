library(plotly)
#Merge
library(data.table)
library(tidytext)
library(tidyr)
library(dplyr) 

library(doParallel)
library(foreach)

library(stringi)   #stri_replace_all_regex
library(stringr) #str_detect

#Anlaysis
library(tmap)  #Plot Map
library(maptools)
library(lubridate)  #dmy
library(rtweet)#lang recode langy
library(ggplot2)

#READ FILES
##########################################################################################################
setwd("C:/Users/bix/Downloads")

folder<-list.dirs()
folder<-folder[-1]

#folder<-folder[1]    #####

SUMMARY_day_main<-as.data.frame(matrix(0,0,10))
SUMMARY_moth<-as.data.frame(matrix(0,0,12))
Lang_tb<-as.data.frame(matrix(0,0,3))

Geo<-as.data.frame(matrix(0,0,2))

for ( d in folder){
  setwd(d)
  files<-list.files()
  month<-strsplit(d, split="_")[[1]][[2]]
  
  SUMMARY_day<-as.data.frame(matrix(0,0,10))
  
  for ( f in files){
  
  data <- as.data.frame(fread(f ,header = T, stringsAsFactors = FALSE))
  
  SUMMARY_day2<-as.data.frame(matrix(0,1,10))
  date<-strsplit(f, split="_Summary_Details.csv")[[1]]
  
  SUMMARY_day2[1,1]<-month
  SUMMARY_day2[1,2]<-date
  SUMMARY_day2[1,3]<-nrow(data[data$RT=='NO',])
  SUMMARY_day2[1,4]<-nrow(data[data$RT=='YES',])
  SUMMARY_day2[1,5]<-nrow(data)
  SUMMARY_day2[1,6]<-nrow(data[data$Geolocation_coordinate!='(,)',])
  SUMMARY_day2[1,7]<-max(data$Retweets, na.rm = T)
  SUMMARY_day2[1,8]<-median(data$Retweets,na.rm = T)
  SUMMARY_day2[1,9]<-max(data$Likes,na.rm = T)
  SUMMARY_day2[1,10]<-median(data$Likes,na.rm = T)
  
  SUMMARY_day<-rbind(SUMMARY_day,SUMMARY_day2)
  tb<-table(data$Language)
  Lang_tb2<-as.data.frame(matrix(0,length(tb),3))
  Lang_tb2[,1]<-rep(date, nrow(Lang_tb2))
  Lang_tb2[,2]<-names(tb)
  Lang_tb2[,3]<-tb
  
  Lang_tb<-rbind(Lang_tb, Lang_tb2)
  
  
  geo_jsons<-data[data$Geolocation_coordinate!="(,)" & !is.na(data$Geolocation_coordinate) & data$Geolocation_coordinate!="", "Geolocation_coordinate" ]
  if(length(geo_jsons)>=1){
  Geo2<-as.data.frame(matrix(0,length(geo_jsons),2))
  Geo2[,1]<-rep(date, nrow(Geo2))
  Geo2[, 2]<-geo_jsons
  Geo<-rbind(Geo, Geo2)
  }
  }
  SUMMARY_day_main<-rbind(SUMMARY_day_main, SUMMARY_day)
  colnames(SUMMARY_day)<-c('Month',	'Date',	'#OR',	'#RT',	'#Total'	,'#Geo',	'MaxRt',	'MDRT',	'MaxLike',	'MDLike')

  
  SUMMARY_moth2<-as.data.frame(matrix(0,1,12))
  
  SUMMARY_moth2[,1]<-month
  SUMMARY_moth2[,2]<-median(SUMMARY_day$`#OR`,na.rm = T)
  SUMMARY_moth2[,3]<-median(SUMMARY_day$`#RT`,na.rm = T)
  SUMMARY_moth2[,4]<-median(SUMMARY_day$`#Total`,na.rm = T)
  
  SUMMARY_moth2[,5]<-sum(SUMMARY_day$`#OR`,na.rm = T)
  SUMMARY_moth2[,6]<-sum(SUMMARY_day$`#RT`,na.rm = T)
  SUMMARY_moth2[,7]<-sum(SUMMARY_day$`#Total`,na.rm = T)
  
  SUMMARY_moth2[,8]<-sum(SUMMARY_day$`#Geo`,na.rm = T)
  
  SUMMARY_moth2[,9]<-max(SUMMARY_day$MaxRt,na.rm = T)
  SUMMARY_moth2[,10]<-median(SUMMARY_day$MDRT,na.rm = T)
  
  SUMMARY_moth2[,11]<-max(SUMMARY_day$MaxLike,na.rm = T)
  SUMMARY_moth2[,12]<-median(SUMMARY_day$MDLike,na.rm = T)
  
  SUMMARY_moth<-rbind(SUMMARY_moth, SUMMARY_moth2)
  
  setwd("..")
}






####ANALYSIS, EDA, PLOTS
##########################################################################################################







#MAP GEOCODED TWEETS
geotweets <- Geo[!Geo$V2%in%c("Point", "", "(,)"),]

geos <- str_remove(geotweets$V2, fixed("("))
geos <- str_remove(geos, fixed(")"))
geos <- str_split(geos, fixed(","))
geos <- do.call(rbind, geos)
geotweets$Lat <- as.numeric(geos[,1])
geotweets$Long <-  as.numeric(geos[,2])

geotweets <- geotweets[(geotweets$Long <180)&
                         (geotweets$Long >-180)&
                         (geotweets$Lat < 90)&
                         (geotweets$Lat > -90),]
##############################GEOLOcATION############
library(tidygeocoder)
library(dplyr, warn.conflicts = FALSE)
reverse <- geotweets %>%
  reverse_geocode(lat = Lat, long = Long, method = 'osm',
                  address = address_found, full_results = TRUE)%>%
  select(address_found,town,suburb,county,state,country)%>%
  group_by(county,state,country)


count <- reverse %>%
  summarise(count_county = n())

#dat<-read.csv("https://raw.githubusercontent.com/lopezbec/COVID19_Tweets_Dataset_2020/master/Summary_Details/2020_06/2020_06_02_13_Summary_Details.csv")
#write.csv(dat, "2020_06_02_13_Summary_Details.csv")
# new york time covid cases data
#ny <-read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid<-read.csv("https://raw.githubusercontent.com/biddybi/tweets/main/co.csv")
#covid <- covid %>%
  #count %>%
  #mutate(tweets = ifelse(covid$county == count$county, 1, 0)) %>%
##############################








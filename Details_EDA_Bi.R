args = commandArgs(trailingOnly=TRUE) 
args=c("C:/Users/Biddy Bi/Desktop/datascience/2020_01/2020_01_27_19_Summary_Details.csv",
       "C:/Users/Biddy Bi/Desktop")

# pass the absolute path

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
#library(tmap)  #Plot Map
#library(maptools)
library(lubridate)  #dmy
library(rtweet)#lang recode langy
library(ggplot2)

#READ FILES
##########################################################################################################




Geo<-as.data.frame(matrix(0,0,2))


  
  data <- as.data.frame(fread(args[1] ,header = T, stringsAsFactors = FALSE))
  
  
  
  geo_jsons<-data[data$Geolocation_coordinate!="(,)" & !is.na(data$Geolocation_coordinate) & data$Geolocation_coordinate!="", "Geolocation_coordinate" ]
  if(length(geo_jsons)>=1){
  Geo2<-as.data.frame(matrix(0,length(geo_jsons),2))
  Geo2[,1]<-rep(date, nrow(Geo2))
  Geo2[, 2]<-geo_jsons
  Geo<-Geo2
    }
  
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
library(tidygeocoder)
library(dplyr, warn.conflicts = FALSE)
reverse <- geotweets %>%
  reverse_geocode(lat = Lat, long = Long, method = 'osm',
                  address = address_found, full_results = TRUE)%>%
  select(address_found,town,suburb,county,state,country)%>%
  group_by(county,state,country)



subname<-substr(args[1],nchar(args[1])-32,nchar(args[1])-19)
name<-paste0(args[2], "/",subname,"Reverse.csv")
write.csv(reverse, name)

count <- reverse %>%
  summarise(count_county = n())

#covid<-read.csv("https://raw.githubusercontent.com/biddybi/tweets/main/co.csv")



###################

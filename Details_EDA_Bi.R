
# pass the absolute path
args = commandArgs(trailingOnly=TRUE) 
args=c("C:/Users/Biddy Bi/Desktop/datascience/2020_01/2020_01_27_19_Summary_Details.csv",
       "C:/Users/Biddy Bi/Desktop")

#Load all packages
library(data.table)
library(tidytext)
library(tidyr)
library(dplyr) 
library(doParallel)
library(foreach)
library(foreach)
library(stringi)   #stri_replace_all_regex
library(stringr) #str_detect
library(sjmisc)
install.packages("bit64")
library(tidygeocoder)
library(dplyr, warn.conflicts = FALSE)

#READ Summary_Details FILES

Geo<-as.data.frame(matrix(0,0,2))
data <- as.data.frame(fread(args[1] ,header = T, stringsAsFactors = FALSE))
name<-strsplit(args[1], split="_Summary_Details.csv")[[1]]
date<-substr(name,nchar(name)-12,nchar(name))
# Find all cells that has geolocation and its tweet ID
geo_jsons<-data[data$Geolocation_coordinate!="(,)" & !is.na(data$Geolocation_coordinate) & data$Geolocation_coordinate!="", "Geolocation_coordinate"]
twID<-data[data$Geolocation_coordinate!="(,)" & !is.na(data$Geolocation_coordinate) & data$Geolocation_coordinate!="", "Tweet_ID"]
if(length(geo_jsons)>=1){
  Geo2<-as.data.frame(matrix(0,length(geo_jsons),3))
  Geo2[, 1]<-rep(date, nrow(Geo2))
  Geo2[, 2]<-geo_jsons
  Geo2[, 3]<-twID
  Geo<-Geo2
}

#Change the pattern of geolocation and sort it in dataframe
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

# Use reverse_geo to get the county name of the location according to lat and longtitude
reverse <- geotweets %>%
  reverse_geocode(lat = Lat, long = Long, method = 'osm',
                  address = address_found, full_results = TRUE)%>%
  select(address_found,town,suburb,county,state,country)%>%
  group_by(county,state,country)
# Add tweet ID into the reverse table with the county information
reverse <- cbind(reverse,  geotweets["V3"])
colnames(reverse)[7] <- "TweetID"


# write the reverse out as a csv file
subname<-substr(args[1],nchar(args[1])-32,nchar(args[1])-19)
namea<-paste0(args[2], "/",subname,"Reverse.csv")
write.csv(reverse, namea)


# read the covid cases data from newyork times
co <- fread("C:/Users/Biddy Bi/Desktop/datascience/co.csv",header = T, stringsAsFactors = FALSE)
# change the form of date to the date form of newyork time table
dateslash <-substr(gsub("_","-",date),0,10)
# Find all the date in nytime data that correspond to this summary detail file date
subcovid <- filter(co,co$date==dateslash)

# Count # of times tweets from same county add two columns of cases and deaths
count <- reverse %>%
  summarise(count_county = n()) %>%
  mutate(cases ="NA") %>%
  mutate(deaths ="NA")

# Add the value of cases and deaths from nytimes to the count table of same county
count$cases[grepl(subcovid$county, count$county, fixed=TRUE)] <- co$cases
count$deaths[grepl(subcovid$county, count$county, fixed=TRUE)] <- co$deaths

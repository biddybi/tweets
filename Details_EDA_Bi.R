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

setwd("C:/Users/bix/Downloads")


#Show FILES
##########################################################################################################
colnames(SUMMARY_moth)<-c('Month', 	'Avg #OR',	'Avg #RT',	'Avg Tweets','# OR',	'# RT',	'#Total',	'Total Geo',	
                          'Max Rt',	'MD RT',	'Max Like',	'MD Like')
write.csv(SUMMARY_moth, "SUMMARY_moth.csv")
colnames(SUMMARY_day_main)<-c('Month',	'Date',	'#OR',	'#RT',	'#Total'	,'#Geo',	'MaxRt',	'MDRT',	'MaxLike',	'MDLike')
write.csv(SUMMARY_day_main, "SUMMARY_day_main.csv")

write.csv(Lang_tb, "Lang_Freq_table.csv")



####ANALYSIS, EDA, PLOTS
##########################################################################################################
#CREATE DATE OBJECT
SUMMARY_day_main$Hour<-rep(0,nrow(SUMMARY_day_main) )
for( i in 1:nrow(SUMMARY_day_main)){
  
  SUMMARY_day_main[i, "Hour"]<-strsplit(SUMMARY_day_main[i,'Date'], split="_")[[1]][4]
  
  
}

SUMMARY_day_main$Date<-as.Date(SUMMARY_day_main$Date, '%Y_%m_%d_%H')
SUMMARY_day_main$Month<-months(as.Date(SUMMARY_day_main$Date, '%Y_%m_%d_%H'),abbreviate =T)


#RECODE LANGUAGE
Lang_tb$LanguageLong <- langs$english[match(Lang_tb$V2, langs$alpha)]
Lang_tb$LanguageLong[Lang_tb$V2=="in"] <- "Bahasa"
Lang_tb$Hour<-rep(0,nrow(Lang_tb) )

for( i in 1:nrow(Lang_tb)){
  
  Lang_tb[i, "Hour"]<-strsplit(Lang_tb[i,'V1'], split="_")[[1]][4]
  
  
}
Lang_tb$Date<-as.Date(Lang_tb$V1, '%Y_%m_%d_%H')
Lang_tb$Month<-months(as.Date(Lang_tb$V1, '%Y_%m_%d_%H'),abbreviate =T)





#SUMMARIZE TWEETS BY Lang
languages<-Lang_tb %>%
  group_by(Date,LanguageLong) %>%
  summarize(total=sum(V3,na.rm = TRUE))

topLang_or<-Lang_tb %>%
  group_by(LanguageLong) %>%
  summarize(total=sum(V3,na.rm = TRUE))%>% arrange(desc(as.numeric(total)))

topLang_or[is.na(topLang_or$LanguageLong), "LanguageLong"]<-"Other"


topLang<-as.vector(topLang_or[c(1:5), 'LanguageLong'])

topLang_or$Percentage<-(topLang_or$total/sum(topLang_or$total))*100

write.csv(topLang_or, "topLang.csv")

languages$TopLanguages <- languages$LanguageLong

languages$TopLanguages[!languages$TopLanguages%in%topLang$LanguageLong] <- "Other"

theme_opts <- theme_bw() + theme(axis.title.x=element_text(size=24, colour="black", face="bold"),
                                 axis.text.x=element_text(size=16, colour="black", face="bold"),
                                 axis.title.y=element_text(size=24, colour="black", face="bold"),
                                 axis.text.y=element_text(size=16, colour="black", face="bold"),
                                 plot.title = element_text(size=24, colour="black", face="bold"),
                                 strip.text=element_text(size=16, colour="black", face="bold"),
                                 legend.title=element_text(size=24, colour="black", face="bold"),
                                 legend.text=element_text(size=16, colour="black", face="bold"),
                                 legend.position = "bottom")

#Plot by Lag

g<-ggplot(data=languages, aes(x=Date, y=total,fill=TopLanguages))+
  geom_bar(position="stack", stat = 'identity')+
  scale_fill_viridis_d("Language")+
  scale_x_date(date_breaks  ="1 month", date_labels="%b")+
  scale_y_continuous("Number of Tweets")+
  theme_bw()+
  theme_opts
 


png("Tweets by Language Bar plot.png", width=10, height=6, units="in", res=150)
print(g)
dev.off()


p<-ggplotly(
  p = g
)
p
htmlwidgets::saveWidget(as_widget(p), "Tweets by Language Bar plot.html")

#Plot all 

languagesday<- languages %>%
  group_by(Date) %>%
  summarize(total=sum(total,na.rm = TRUE))
  
  
g<-ggplot(data=languagesday, aes(x=Date, y=total))+
  geom_line(size=1, color='blue', linetype = 1)+
  scale_fill_viridis_d("Language")+
  scale_x_date(date_breaks  ="1 month", date_labels="%b")+
  scale_y_continuous("Number of Tweets")+
  theme_bw()+
  theme_opts



png("Tweets per Day.png", width=10, height=6, units="in", res=150)
print(g)
dev.off()

p<-ggplotly(
  p = g
)
p
htmlwidgets::saveWidget(as_widget(p), "Tweets per Day.html")

g<-ggplot(data=languages[languages$TopLanguages!="Other",], aes(x=Date, y=total,group=TopLanguages, color=TopLanguages))+
  geom_line(size=1)+
  scale_fill_viridis_d("Language")+
  scale_x_date(date_breaks  ="1 month", date_labels="%b")+
  scale_y_continuous("Number of Tweets")+
  theme_bw()+
  theme_opts

png("Tweets by Language Line plot.png", width=10, height=6, units="in", res=150)
print(g)
dev.off()

p<-ggplotly(
  p = g
)
p
htmlwidgets::saveWidget(as_widget(p), "Tweets by Language Line plot.html")

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
ny <-read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid<-read.csv("C:/Users/bix/Downloads/co.csv")
covid <- covid %>%
  count %>%
  mutate(tweets = ifelse(covid$county == count$county, 1, 0)) %>%
##############################




geotweets <- aggregate(rep(1, nrow(geotweets)), by=list(geotweets$Lat, geotweets$Long),
                       FUN=sum)

coordinates(geotweets) <- c("Group.2", "Group.1")
proj4string(geotweets) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
names(geotweets@data) <- "Tweets"

data("World")
m <- tm_shape(World)+
  tm_borders()+
  tm_shape(geotweets)+
  tm_bubbles(size="Tweets")+
  tm_style("bw")+
  tm_layout("Geocoded\nTweets", title.position = c("left", "bottom"))

tmap_save(tm=m, filename="GeoTweets.png", width=8, height=6, units="in", dpi=150)









if(length(table(SUMMARY_day_main$Date))*24==sum(table(SUMMARY_day_main$Date))){
  print("ALL DAY WITH 24 HRS")
}else{
  tb<-  table(SUMMARY_day_main$Date)
  tb[tb!=24]
}





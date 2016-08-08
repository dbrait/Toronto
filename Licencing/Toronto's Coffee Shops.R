#MAKE SURE THAT ALL FRANCHISES ARE COUNTED, CHECK REGEX STUFF TO MAKE SURE
#BAR GRAVES JUST SHOWING THE DISTRIBUTION OVER TIME


library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)
library(colorspace)
library(RColorBrewer)
library(lubridate)
library(ggthemes)
library(reshape2)
library(stringr)
library(leaflet)

#HAVE MAPS BUILT OUT FOR EACH OF THE MAJOR PLACES, NEED TO USE GREP TO MAKE SURE YOU DIDNT MISS ANY
#THEN FOCUS ON HOW YOU WANT TO STORY TELL AND VISUALIZE THEM
#LAST IDEA IS TO DO PROLIFERATION (SO PLACES THAT HAD THEIR LICENCE FIVE/TEN/TWENTY YEARS AGO)

licence <- read.csv("licence.csv")

#merge address columns

#currently operating licences
current_operating <- licence %>% filter(Cancel.Date == "")
current_operating$Cancel.Date <- as.Date(Sys.Date(), "%d-%B-%Y")
current_operating$Issued <- as.Date(current_operating$Issued, "%d-%B-%Y")
summary(current_operating)

#format dates properly FIGURE OUT HOW TO TAKE OUT LICENCES THAT HAVE NOT BEEN CANCELED
other_date <- licence %>% filter(Cancel.Date != "")
other_date$Cancel.Date <- as.Date(other_date$Cancel.Date, "%d-%B-%Y")
other_date$Issued <- as.Date(other_date$Issued, "%d-%B-%Y")

#five years ago
Date_1 <- as.Date("07-JUN-2011", "%d-%B-%Y")
five_years_ago_canceled <- other_date %>% filter(Cancel.Date >= Date_1 & Issued <= Date_1)
five_years_ago_stillopen <- current_operating %>% filter(Issued <= Date_1)
five_years_ago <- rbind(five_years_ago_canceled, five_years_ago_stillopen)

#ten years ago
Date_2 <- as.Date("07-JUN-2006", "%d-%B-%Y")
ten_years_ago_canceled <- other_date %>% filter(Cancel.Date >= Date_2 & Issued <= Date_2)
ten_years_ago_stillopen <- current_operating %>% filter(Issued <= Date_2)
ten_years_ago <- rbind(ten_years_ago_canceled, ten_years_ago_stillopen)

#fifteen years ago
Date_3 <- as.Date("07-JUN-2001", "%d-%B-%Y")
fifteen_years_ago_canceled <- other_date %>% filter(Cancel.Date >= Date_3 & Issued <= Date_3)
fifteen_years_ago_stillopen <- current_operating %>% filter(Issued <= Date_3)
fifteen_years_ago <- rbind(fifteen_years_ago_canceled, fifteen_years_ago_stillopen)

#twenty years ago 
Date_4 <- as.Date("07-JUN-1996", "%d-%B-%Y")
twenty_years_ago_canceled <- other_date %>% filter(Cancel.Date >= Date_4 & Issued <= Date_4)
twenty_years_ago_stillopen <- current_operating %>% filter(Issued <= Date_4)
twenty_years_ago <- rbind(twenty_years_ago_canceled, twenty_years_ago_stillopen)

#Creating a base Toronto map
mygeocode <- geocode("Toronto, ON")
mymap <- get_map(location="Toronto", zoom=11)

#WILLL NEED TO USE REG EXPRESSIONS TO DO THIS PROPERLY (LOTS OF INCONSISTENCIES WITH FRANCHISES OWNERS, NUMBERS AFTER THE NAME, ETC )

#Starbucks using regex
starbucks <- filter(current_operating, grepl("^STARBUCK", Client.Name))
summary(starbucks)

starbucks$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", starbucks$Licence.Address.Line.1)

sdata <- as.character(starbucks$Licence.Address.Line.1)
sdata1 <- as.character(starbucks$Licence.Address.Line.2)
sdata2 <- as.character(starbucks$Licence.Address.Line.3)
saddresses <- paste0(sdata, ", ", sdata1, ", ", sdata2)
star_geodata <- geocode(saddresses)  #there are issues with a few locations on the geo pull, likely because of unit numbers or something similar
star_geodata <- unique(star_geodata)  #find unique addresses
star_geodata$Franchise <- "Starbucks"
star_geodata$Year <- as.factor(2016)

starbucks_five_years <- filter(five_years_ago, grepl("^STARBUCK", Client.Name))
starbucks_five_years$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", starbucks_five_years$Licence.Address.Line.1)
sdata_five <- as.character(starbucks_five_years$Licence.Address.Line.1)
sdata1_five <- as.character(starbucks_five_years$Licence.Address.Line.2)
sdata2_five <- as.character(starbucks_five_years$Licence.Address.Line.3)
saddresses_five <- paste0(sdata_five, ", ", sdata1_five, ", ", sdata2_five)
star_five_geodata <- geocode(saddresses_five)
star_five_geodata <- unique(star_five_geodata)
star_five_geodata$Franchise <- "Starbucks"
star_five_geodata$Year <- as.factor(2011)

starbucks_ten_years <- filter(ten_years_ago, grepl("^STARBUCK", Client.Name))
starbucks_ten_years$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", starbucks_ten_years$Licence.Address.Line.1)
sdata_ten <- as.character(starbucks_ten_years$Licence.Address.Line.1)
sdata1_ten <- as.character(starbucks_ten_years$Licence.Address.Line.2)
sdata2_ten <- as.character(starbucks_ten_years$Licence.Address.Line.3)
saddresses_ten <- paste0(sdata_ten, ", ", sdata1_ten, ", ", sdata2_ten)
star_ten_geodata <- geocode(saddresses_ten)
star_ten_geodata <- unique(star_ten_geodata)
star_ten_geodata$Franchise <- "Starbucks"
star_ten_geodata$Year <- as.factor(2006)

starbucks_fifteen_years <- filter(fifteen_years_ago, grepl("^STARBUCK", Client.Name))
starbucks_fifteen_years$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", starbucks_fifteen_years$Licence.Address.Line.1)
sdata_fifteen <- as.character(starbucks_fifteen_years$Licence.Address.Line.1)
sdata1_fifteen <- as.character(starbucks_fifteen_years$Licence.Address.Line.2)
sdata2_fifteen <- as.character(starbucks_fifteen_years$Licence.Address.Line.3)
saddresses_fifteen <- paste0(sdata_fifteen, ", ", sdata1_fifteen, ", ", sdata2_fifteen)
star_fifteen_geodata <- geocode(saddresses_fifteen)
star_fifteen_geodata <- unique(star_fifteen_geodata)
star_fifteen_geodata$Franchise <- "Starbucks"
star_fifteen_geodata$Year <- as.factor(2001)

starbucks_twenty_years <- filter(twenty_years_ago, grepl("^STARBUCK", Client.Name))
starbucks_twenty_years$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", starbucks_twenty_years$Licence.Address.Line.1)
sdata_twenty <- as.character(starbucks_twenty_years$Licence.Address.Line.1)
sdata1_twenty <- as.character(starbucks_twenty_years$Licence.Address.Line.2)
sdata2_twenty <- as.character(starbucks_twenty_years$Licence.Address.Line.3)
saddresses_twenty <- paste0(sdata_twenty, ", ", sdata1_twenty, ", ", sdata2_twenty)
star_twenty_geodata <- geocode(saddresses_twenty)
star_twenty_geodata <- unique(star_twenty_geodata)
star_twenty_geodata$Franchise <- "Starbucks"
star_twenty_geodata$Year <- as.factor(1996)

star_geodata_all <- rbind(star_geodata, star_five_geodata, star_ten_geodata, star_fifteen_geodata)

#stat bar
star_total <- star_geodata_all %>% group_by(Year) %>% summarise(count = n())
star_total_graph <- ggplot(star_total, aes(x=Year, y=count)) + geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("2001", "2006", "2011", "2016")) +
  labs(title="Starbucks", x="Year", y="Number of Locations") +
  theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.title.x = element_text(size=14, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold"))
star_total_graph

star_Map_Points <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                       data=star_geodata, alpha=.5, color="blue") +
  ggtitle("Starbucks 2016") + theme(plot.title=element_text(face="bold", size=16)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none")
#use facet_grid to compare store growth over time
star_Map_Points

star_map_five <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                           data=star_five_geodata, alpha=.5, color="blue") +
  ggtitle("Starbucks 2011") + theme(plot.title=element_text(face="bold", size=16)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "none")
star_map_five

star_map_ten <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                          data=star_ten_geodata, alpha=.5, color="blue") + 
  ggtitle("Starbucks 2006") + theme(plot.title=element_text(face="bold", size=16)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "none")
star_map_ten

star_map_fifteen <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                              data=star_fifteen_geodata, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
star_map_fifteen

star_map_twenty <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                             data=star_twenty_geodata, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
star_map_twenty

star_map_all <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                          data=star_geodata_all, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
star_map_all + facet_grid(. ~ Year)

#starbucks five years ago
starbucks_five_years <- five_years_ago %>% filter(Client.Name == "STARBUCKS COFFEE CANADA INC")
summary(starbucks_five_years)

#ten years ago
starbucks_ten_years <- ten_years_ago %>% filter(Client.Name == "STARBUCKS COFFEE CANADA INC")
summary(starbucks_ten_years)

#fifteen years ago
starbucks_fifteen_years <- fifteen_years_ago %>% filter(Client.Name == "STARBUCKS COFFEE CANADA INC")
summary(starbucks_fifteen_years)

#Tim hortons, using regex
tim_hortons <- filter(current_operating, grepl("^TIM HORTON", Operating.Name))
tim_hortons$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", tim_hortons$Licence.Address.Line.1)
tdata <- as.character(tim_hortons$Licence.Address.Line.1)
tdata1 <- as.character(tim_hortons$Licence.Address.Line.2)
tdata2 <- as.character(tim_hortons$Licence.Address.Line.3)
taddresses <- paste0(tdata, ", ", tdata1, ", ", tdata2)
tim_geodata <- geocode(taddresses)
tim_geodata <- unique(tim_geodata)
tim_geodata$Franchise <- "Tim Hortons"
tim_geodata$Year <- as.factor(2016)

tim_hortons_five <- filter(five_years_ago, grepl("^TIM HORTON", Operating.Name))
tim_hortons_five$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", tim_hortons_five$Licence.Address.Line.1)
tdata_five <- as.character(tim_hortons_five$Licence.Address.Line.1)
tdata1_five <- as.character(tim_hortons_five$Licence.Address.Line.2)
tdata2_five <- as.character(tim_hortons_five$Licence.Address.Line.3)
taddresses_five <- paste0(tdata_five, ", ", tdata1_five, ", ", tdata2_five)
tim_five_geodata <- geocode(taddresses_five)
tim_five_geodata <- unique(tim_five_geodata)
tim_five_geodata$Franchise <- "Tim Hortons"
tim_five_geodata$Year <- as.factor(2011)

tim_hortons_ten <- filter(ten_years_ago, grepl("^TIM HORTON", Operating.Name))
tim_hortons_ten$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", tim_hortons_ten$Licence.Address.Line.1)
tdata_ten <- as.character(tim_hortons_ten$Licence.Address.Line.1)
tdata1_ten <- as.character(tim_hortons_ten$Licence.Address.Line.2)
tdata2_ten <- as.character(tim_hortons_ten$Licence.Address.Line.3)
taddresses_ten <- paste0(tdata_ten, ", ", tdata1_ten, ", ", tdata2_ten)
tim_ten_geodata <- geocode(taddresses_ten)
tim_ten_geodata <- unique(tim_ten_geodata)
tim_ten_geodata$Franchise <- "Tim Hortons"
tim_ten_geodata$Year <- as.factor(2006)

tim_hortons_fifteen <- filter(fifteen_years_ago, grepl("^TIM HORTON", Operating.Name))
tim_hortons_fifteen$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", tim_hortons_fifteen$Licence.Address.Line.1)
tdata_fifteen <- as.character(tim_hortons_fifteen$Licence.Address.Line.1)
tdata1_fifteen <- as.character(tim_hortons_fifteen$Licence.Address.Line.2)
tdata2_fifteen <- as.character(tim_hortons_fifteen$Licence.Address.Line.3)
taddresses_fifteen <- paste0(tdata_fifteen, ", ", tdata1_fifteen, ", ", tdata2_fifteen)
tim_fifteen_geodata <- geocode(taddresses_fifteen)
tim_fifteen_geodata <- unique(tim_fifteen_geodata)
tim_fifteen_geodata$Franchise <- "Tim Hortons"
tim_fifteen_geodata$Year <- as.factor(2001)

tim_hortons_twenty <- filter(twenty_years_ago, grepl("^TIM HORTON", Operating.Name))
tim_hortons_twenty$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", tim_hortons_twenty$Licence.Address.Line.1)
tdata_twenty <- as.character(tim_hortons_twenty$Licence.Address.Line.1)
tdata1_twenty <- as.character(tim_hortons_twenty$Licence.Address.Line.2)
tdata2_twenty <- as.character(tim_hortons_twenty$Licence.Address.Line.3)
taddresses_twenty <- paste0(tdata_twenty, ", ", tdata1_twenty, ", ", tdata2_twenty)
tim_twenty_geodata <- geocode(taddresses_twenty)
tim_twenty_geodata <- unique(tim_twenty_geodata)
tim_twenty_geodata$Franchise <- "Tim Hortons"
tim_twenty_geodata$Year <- as.factor(1996)

tim_geodata_all <- rbind(tim_geodata, tim_five_geodata, tim_ten_geodata, tim_fifteen_geodata)

#tims bar graphs
tims_total <- tim_geodata_all %>% group_by(Year) %>% summarise(count = n())
tims_total_graph <- ggplot(tims_total, aes(x=Year, y=count)) + geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("2001", "2006", "2011", "2016")) + 
  labs(title = "Tim Horton's", x = "Year", y = "Number of Locations") +
  theme(plot.title = element_text(face="bold", size=16)) + 
  theme(axis.title.x = element_text(size=14, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold"))
tims_total_graph

tims_Map_Points <- ggmap(mymap) + geom_point(aes(x = lon, y = lat, size=1),
                                             data=tim_geodata, alpha=.5, color="blue") +
  ggtitle("Tim Horton's 2016") + theme(plot.title=element_text(face="bold", size=16)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none")
tims_Map_Points

tims_map_five <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                           data=tim_five_geodata, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
tims_map_five

tims_map_ten <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                          data=tim_ten_geodata, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
tims_map_ten

tims_map_fifteen <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                              data=tim_fifteen_geodata, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
tims_map_fifteen

tims_map_twenty <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                             data=tim_twenty_geodata, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
tims_map_twenty

tims_map_all <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                          data=tim_geodata_all, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
tims_map_all + facet_grid(. ~ Year)

#using regex

#unique addresses (some duplicates in data)


#timmys ten years
tims_ten_years <- ten_years_ago %>% filter(Operating.Name == "TIM HORTONS"|Operating.Name=="TIM HORTON'S") %>% filter(Category == "EATING ESTABLISHMENT"|
                                                                                                                        Category == "RETAIL STORE (FOOD)"|
                                                                                                                        Category == "BOULEVARD CAFE")
summary(tims_ten_years)

#timmys fifteen years
tims_fifteen_years <- fifteen_years_ago %>% filter(Operating.Name == "TIM HORTONS"|Operating.Name=="TIM HORTON'S") %>% filter(Category == "EATING ESTABLISHMENT"|
                                                                                                                                Category == "RETAIL STORE (FOOD)"|
                                                                                                                                Category == "BOULEVARD CAFE")
summary(tims_fifteen_years)

#check an example of a duplicate
finch_location <- tim_hortons %>% filter(Licence.Address.Line.1 == "1500 FINCH AVE E")

#read addresses into an easily digestable format  
data <- as.character(licence$Licence.Address.Line.1)
data1 <- as.character(licence$Licence.Address.Line.2)
data2 <- as.character(licence$Licence.Address.Line.3)
addresses <- paste0(data, ", ", data1,", ", data2)


#mapping 
#write to ouput file
saveRDS(data, paste0("../data/", infile, "_geocoded.rds"))
write.table(data, file=paste0("../data/", infile, "_geocoded.csv"), sep=",", row.names=FALSE)

mygeocode <- geocode("Toronto, ON")
mymap <- qmap(location = c(lon = mygeocode$lon, lat=mygeocode$lat), zoom=11, maptype="roadmap", crop=TRUE)
mymap <- get_map(location="Toronto", zoom=15)

mapPoints <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=sqrt(totalrev)),
                                       data=fulldata, alpha=1, color="blue")
mapPoints

tmaps <- get_map(location=c(43.59, -79.63, 43.83, -79.16), source="osm")
ggmap(tmaps) + geom_point(size=5, alpha=1/2, aes(lon, lat, color=type))

get_map(location=c(lon, lat), zoom="auto", scale="auto", maptype="roadmap", source=c("osm"), )



#Coffee Time
coffee_time <- filter(current_operating, grepl("^COFFEE TIME", Operating.Name)
coffee_time$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", coffee_time$Licence.Address.Line.1)
ct_data <- as.character(coffee_time$Licence.Address.Line.1)
ct_data1 <- as.character(coffee_time$Licence.Address.Line.2)
ct_data2 <- as.character(coffee_time$Licence.Address.Line.3)
ct_addresses <- paste0(ct_data, ", ", ct_data1, ", ", ct_data2)
ct_geodata <- geocode(ct_addresses)
ct_geodata <- unique(ct_geodata)
ct_geodata$Franchise <- "Coffee Time"
ct_geodata$Year <- as.factor(2016)

coffee_time_five <- filter(five_years_ago, grepl("^COFFEE TIME", Operating.Name))
coffee_time_five$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", coffee_time$Licence.Address.Line.1)
ct_data_five <- as.character(coffee_time_five$Licence.Address.Line.1)
ct_data1_five <- as.character(coffee_time_five$Licence.Address.Line.2)
ct_data2_five <- as.character(coffee_time_five$Licence.Address.Line.3)
ct_addresses_five <- paste0(ct_data_five, ", ", ct_data1_five, ", ", ct_data2_five)
ct_geodata_five <- geocode(ct_addresses_five)
ct_geodata_five <- unique(ct_geodata_five)
ct_geodata_five$Franchise <- "Coffee Time"
ct_geodata_five$Year <- as.factor(2011)

coffee_time_ten <- filter(ten_years_ago, grepl("^COFFEE TIME", Operating.Name))
coffee_time_ten$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", coffee_time_ten$Licence.Address.Line.1)
ct_data_ten <- as.character(coffee_time_ten$Licence.Address.Line.1)
ct_data1_ten <- as.character(coffee_time_ten$Licence.Address.Line.2)
ct_data2_ten <- as.character(coffee_time_ten$Licence.Address.Line.3)
ct_addresses_ten <- paste0(ct_data_ten, ", ", ct_data1_ten, ", ", ct_data2_ten)
ct_geodata_ten <- geocode(ct_addresses_ten)
ct_geodata_ten <- unique(ct_geodata_ten)
ct_geodata_ten$Franchise <- "Coffee Time"
ct_geodata_ten$Year <- as.factor(2006)

coffee_time_fifteen <- filter(fifteen_years_ago, grepl("^COFFEE TIME", Operating.Name))
coffee_time_fifteen$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", coffee_time_fifteen$Licence.Address.Line.1)
ct_data_fifteen <- as.character(coffee_time_fifteen$Licence.Address.Line.1)
ct_data1_fifteen <- as.character(coffee_time_fifteen$Licence.Address.Line.2)
ct_data2_fifteen <- as.character(coffee_time_fifteen$Licence.Address.Line.3)
ct_addresses_fifteen <- paste0(ct_data_fifteen, ", ", ct_data1_fifteen, ", ", ct_data2_fifteen)
ct_geodata_fifteen <- geocode(ct_addresses_fifteen)
ct_geodata_fifteen <- unique(ct_geodata_fifteen)
ct_geodata_fifteen$Franchise <- "Coffee Time"
ct_geodata_fifteen$Year <- as.factor(2001)

coffee_time_twenty <- filter(twenty_years_ago, grepl("^COFFEE TIME", Operating.Name))
coffee_time_twenty$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", coffee_time_twenty$Licence.Address.Line.1)
ct_data_twenty <- as.character(coffee_time_twenty$Licence.Address.Line.1)
ct_data1_twenty <- as.character(coffee_time_twenty$Licence.Address.Line.2)
ct_data2_twenty <- as.character(coffee_time_twenty$Licence.Address.Line.3)
ct_addresses_twenty <- paste0(ct_data_twenty, ", ", ct_data1_twenty, ", ", ct_data2_twenty)
ct_geodata_twenty <- geocode(ct_addresses_twenty)
ct_geodata_twenty <- unique(ct_geodata_twenty)
ct_geodata_twenty$Franchise <- "Coffee Time"
ct_geodata_twenty$Year <- as.factor(1996)

ct_geodata_all <- rbind(ct_geodata, ct_geodata_five, ct_geodata_ten, ct_geodata_fifteen)

#coffee time total store counts
ct_total <- ct_geodata_all %>% group_by(Year) %>% summarize(count = n())
ct_total_graph <- ggplot(ct_total, aes(x=Year, y=count)) + geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("2001", "2006", "2011", "2016")) +
  labs(title="Coffee Time", x="Year", y="Number of Locations") +
  theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.title.x = element_text(size=14, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold"))
  
ct_total_graph

ct_Map_Points <- ggmap(mymap) + geom_point(aes(x = lon, y = lat, size=1),
                                           data=ct_geodata, alpha=.5, color="blue") +
  ggtitle("Coffee Time 2016") + theme(plot.title=element_text(face="bold", size=16)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none")
ct_Map_Points

Ct_Map_Five <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                         data=ct_geodata_five, alpha=.5, color="blue")
Ct_Map_Five

Ct_Map_Ten <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                        data=ct_geodata_ten, alpha=.5, color="blue")
Ct_Map_Ten

Ct_Map_Fifteen <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                            data=ct_geodata_fifteen, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
Ct_Map_Fifteen

Ct_Map_Twenty <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                           data=ct_geodata_twenty, alpha=1, color="blue")
Ct_Map_Twenty

Ct_Map_All <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                        data=ct_geodata_all, alpha=1, color="blue")
Ct_Map_All + facet_grid(. ~ Year)

#regex
Ct_Client <- filter(current_operating, grepl("^COFFEE TIME", Client.Name))
Ct_Ops <- filter(current_operating, grepl("^COFFEE TIME", Operating.Name))
coffee_time <- rbind(Ct_Client, Ct_Ops)


#Timothy's
timothys <- current_operating %>% filter(Operating.Name == "TIMOTHY'S WORLD COFFEE")
timothys$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", timothys$Licence.Address.Line.1)
timothys_data <- as.character(timothys$Licence.Address.Line.1)
timothys_data1 <- as.character(timothys$Licence.Address.Line.2)
timothys_data2 <- as.character(timothys$Licence.Address.Line.3)
timothys_addresses <- paste0(timothys_data, ", ", timothys_data1, ", ", timothys_data2)
timothys_geodata <- geocode(timothys_addresses)
timothys_geodata <- unique(timothys_geodata)
timothys_geodata$Franchise <- "Timothys"
timothys_geodata$Year <- as.factor(2016)

timothys_five <- filter(five_years_ago, grepl("^TIMOTHY", Operating.Name))
timothys_five$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", timothys_five$Licence.Address.Line.1)
timothys_data_five <- as.character(timothys_five$Licence.Address.Line.1)
timothys_data1_five <- as.character(timothys_five$Licence.Address.Line.2)
timothys_data2_five <- as.character(timothys_five$Licence.Address.Line.3)
timothys_addresses_five <- paste0(timothys_data_five, ", ", timothys_data1_five, ", ", timothys_data2_five)
timothys_geodata_five <- geocode(timothys_addresses_five)
timothys_geodata_five <- unique(timothys_geodata_five)
timothys_geodata_five$Franchise <- "Timothys"
timothys_geodata_five$Year <- as.factor(2011)

timothys_ten <- filter(ten_years_ago, grepl("^TIMOTHY", Operating.Name))
timothys_ten$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", timothys_ten$Licence.Address.Line.1)
timothys_data_ten <- as.character(timothys_ten$Licence.Address.Line.1)
timothys_data1_ten <- as.character(timothys_ten$Licence.Address.Line.2)
timothys_data2_ten <- as.character(timothys_ten$Licence.Address.Line.3)
timothys_addresses_ten <- paste0(timothys_data_ten, ", ", timothys_data1_ten, ", ", timothys_data2_ten)
timothys_geodata_ten <- geocode(timothys_addresses_ten)
timothys_geodata_ten <- unique(timothys_geodata_ten)
timothys_geodata_ten$Franchise <- "Timothys"
timothys_geodata_ten$Year <- as.factor(2006)

timothys_fifteen <- filter(fifteen_years_ago, grepl("^TIMOTHY", Operating.Name))
timothys_fifteen$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", timothys_fifteen$Licence.Address.Line.1)
timothys_data_fifteen <- as.character(timothys_fifteen$Licence.Address.Line.1)
timothys_data1_fifteen <- as.character(timothys_fifteen$Licence.Address.Line.2)
timothys_data2_fifteen <- as.character(timothys_fifteen$Licence.Address.Line.3)
timothys_addresses_fifteen <- paste0(timothys_data_fifteen, ", ", timothys_data1_fifteen, ", ", timothys_data2_fifteen)
timothys_geodata_fifteen <- geocode(timothys_addresses_fifteen)
timothys_geodata_fifteen <- unique(timothys_geodata_fifteen)
timothys_geodata_fifteen$Franchise <- "Timothys"
timothys_geodata_fifteen$Year <- as.factor(2001)

timothys_twenty <- filter(twenty_years_ago, grepl("^TIMOTHY", Operating.Name))
timothys_twenty$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", timothys_twenty$Licence.Address.Line.1)
timothys_data_twenty <- as.character(timothys_twenty$Licence.Address.Line.1)
timothys_data1_twenty <- as.character(timothys_twenty$Licence.Address.Line.2)
timothys_data2_twenty <- as.character(timothys_twenty$Licence.Address.Line.3)
timothys_addresses_twenty <- paste0(timothys_data_twenty, ", ", timothys_data1_twenty, ", ", timothys_data2_twenty)
timothys_geodata_twenty <- geocode(timothys_addresses_twenty)
timothys_geodata_twenty <- unique(timothys_geodata_twenty)
timothys_geodata_twenty$Franchise <- "Timothys"
timothys_geodata_twenty$Year <- as.factor(1996)

timothys_geodata_all <- rbind(timothys_geodata, timothys_geodata_five, timothys_geodata_ten, timothys_geodata_fifteen)

#bar chart
timothys_total <- timothys_geodata_all %>% group_by(Year) %>% summarize(count = n())

timothys_total_graph <- ggplot(timothys_total, aes(x=Year, y=count)) + geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("2001", "2006", "2011", "2016")) +
  labs(title="Timothys", x="Year", y="Number of Locations") +
  theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.title.x = element_text(size=14, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold"))
timothys_total_graph


timothys_Map_Points <- ggmap(mymap) + geom_point(aes(x = lon, y = lat, size=1),
                                             data=timothys_geodata, alpha=.5, color="blue") + 
  ggtitle("Timothy's 2016") + theme(plot.title=element_text(face="bold", size=16)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "none")
timothys_Map_Points

timothys_map_five <- ggmap(mymap) + geom_point(aes(x=lon, y = lat, size=1),
                                               data=timothys_geodata_five, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
timothys_map_five

timothys_map_ten <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                              data=timothys_geodata_ten, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
timothys_map_ten

timothys_map_fifteen <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                                  data=timothys_geodata_fifteen, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
timothys_map_fifteen

timothys_map_twenty <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                                 data=timothys_geodata_twenty, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
timothys_map_twenty

timothys_map_all <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                              data=timothys_geodata_all, alpha=1, color="blue")

timothys_map_all + facet_grid(. ~ Year)

#regex
timothys_ops <- filter(current_operating, grepl("^TIMOTHY", Operating.Name))
timothy <- rbind(timothys_client, timothys_ops)

#second cup
second_cup <- filter(current_operating, grepl("^SECOND CUP", Operating.Name))
second_cup$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", second_cup$Licence.Address.Line.1)
second_data <- as.character(second_cup$Licence.Address.Line.1)
second_data1 <- as.character(second_cup$Licence.Address.Line.2)
second_data2 <- as.character(second_cup$Licence.Address.Line.3)
second_addresses <- paste0(second_data, ", ", second_data1, ", ", second_data2)
second_geodata <- geocode(second_addresses)
second_geodata <- unique(second_geodata)
second_geodata$Franchise <- "Second Cup"
second_geodata$Year <- as.factor(2016)

second_cup_five <- filter(five_years_ago, grepl("^SECOND CUP", Operating.Name))
second_cup_five$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", second_cup_five$Licence.Address.Line.1)
second_data_five <- as.character(second_cup_five$Licence.Address.Line.1)
second_data1_five <- as.character(second_cup_five$Licence.Address.Line.2)
second_data2_five <- as.character(second_cup_five$Licence.Address.Line.3)
second_addresses_five <- paste0(second_data_five, ", ", second_data1_five, ", ", second_data2_five)
second_geodata_five <- geocode(second_addresses_five)
second_geodata_five <- unique(second_geodata_five)
second_geodata_five$Franchise <- "Second Cup"
second_geodata_five$Year <- as.factor(2011)

second_cup_ten <- filter(ten_years_ago, grepl("^SECOND CUP", Operating.Name))
second_cup_ten$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", second_cup_ten$Licence.Address.Line.1)
second_data_ten <- as.character(second_cup_ten$Licence.Address.Line.1)
second_data1_ten <- as.character(second_cup_ten$Licence.Address.Line.2)
second_data2_ten <- as.character(second_cup_ten$Licence.Address.Line.3)
second_addresses_ten <- paste0(second_data_ten, ", ", second_data1_ten, ", ", second_data2_ten)
second_geodata_ten <- geocode(second_addresses_ten)
second_geodata_ten <- unique(second_geodata_ten)
second_geodata_ten$Franchise<- "Second Cup"
second_geodata_ten$Year <- as.factor(2006)

second_cup_fifteen <- filter(fifteen_years_ago, grepl("^SECOND CUP", Operating.Name))
second_cup_fifteen$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", second_cup_fifteen$Licence.Address.Line.1)
second_data_fifteen <- as.character(second_cup_fifteen$Licence.Address.Line.1)
second_data1_fifteen <- as.character(second_cup_fifteen$Licence.Address.Line.2)
second_data2_fifteen <- as.character(second_cup_fifteen$Licence.Address.Line.3)
second_addresses_fifteen <- paste0(second_data_fifteen, ", ", second_data1_fifteen, ", ", second_data2_fifteen)
second_geodata_fifteen <- geocode(second_addresses_fifteen)
second_geodata_fifteen <- unique(second_geodata_fifteen)
second_geodata_fifteen$Franchise <- "Second Cup"
second_geodata_fifteen$Year <- as.factor(2001)

second_cup_twenty <- filter(twenty_years_ago, grepl("^SECOND CUP", Operating.Name))
second_cup_twenty$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", second_cup_twenty$Licence.Address.Line.1)
second_data_twenty <- as.character(second_cup_twenty$Licence.Address.Line.1)
second_data1_twenty <- as.character(second_cup_twenty$Licence.Address.Line.2)
second_data2_twenty <- as.character(second_cup_twenty$Licence.Address.Line.3)
second_addresses_twenty <- paste0(second_data_twenty, ", ", second_data1_twenty, ", ", second_data2_twenty)
second_geodata_twenty <- geocode(second_addresses_twenty)
second_geodata_twenty <- unique(second_geodata_twenty)
second_geodata_twenty$Franchise <- "Second Cup"
second_geodata_twenty$Year <- as.factor(1996)

second_geodata_all <- rbind(second_geodata, second_geodata_five, second_geodata_ten, second_geodata_fifteen)

#bar of count data
second_total <- second_geodata_all %>% group_by(Year) %>% summarise(count = n())
second_total_graph <- ggplot(second_total, aes(x=Year, y=count)) + geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("2001", "2006", "2011", "2016")) +
  labs(title="Second Cup", x="Year", y="Number of Locations") +
  theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.title.x = element_text(size=14, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold"))
second_total_graph

second_Map_Points <- ggmap(mymap) + geom_point(aes(x = lon, y = lat, size=1),
                                             data=second_geodata, alpha=.5, color="blue") +
  ggtitle("Second Cup 2016") + theme(plot.title=element_text(face="bold", size=16)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none")
second_Map_Points

second_map_five <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                             data=second_geodata_five, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
second_map_five

second_map_ten <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                            data=second_geodata_ten, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
second_map_ten

second_map_fifteen <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                                data=second_geodata_fifteen, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
second_map_fifteen

second_map_twenty <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                               data=second_geodata_twenty, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
second_map_twenty

second_map_all <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                            data=second_geodata_all, alpha=1, color="blue") + facet_wrap(. ~ Year)
second_map_all


#regex
second_ops <- filter(current_operating, grepl("^SECOND CUP", Operating.Name))

#Aroma
aroma <- filter(current_operating, grepl("^AROMA ESPRESSO", Operating.Name))
aroma$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", aroma$Licence.Address.Line.1)
aroma_data <- as.character(aroma$Licence.Address.Line.1)
aroma_data1 <- as.character(aroma$Licence.Address.Line.2)
aroma_data2 <- as.character(aroma$Licence.Address.Line.3)
aroma_addresses <- paste0(aroma_data, ", ", aroma_data1, ", ", aroma_data2)
aroma_geodata <- geocode(aroma_addresses)
aroma_geodata <- unique(aroma_geodata)
aroma_geodata$Franchise <- "Aroma"
aroma_geodata$Year <- as.factor(2016)

aroma_five <- filter(five_years_ago, grepl("^AROMA ESPRESSO", Operating.Name))
aroma_five$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", aroma_five$Licence.Address.Line.1)
aroma_data_five <- as.character(aroma_five$Licence.Address.Line.1)
aroma_data1_five <- as.character(aroma_five$Licence.Address.Line.2)
aroma_data2_five <- as.character(aroma_five$Licence.Address.Line.3)
aroma_addresses_five <- paste0(aroma_data_five, ", ", aroma_data1_five, ", ", aroma_data2_five)
aroma_geodata_five <- geocode(aroma_addresses_five)
aroma_geodata_five <- unique(aroma_geodata_five)
aroma_geodata_five$Franchise <- "Aroma"
aroma_geodata_five$Year <- as.factor(2011)

aroma_ten <- filter(ten_years_ago, grepl("^AROMA ESPRESSO", Operating.Name))
aroma_ten$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", aroma_ten$Licence.Address.Line.1)
aroma_data_ten <- as.character(aroma_ten$Licence.Address.Line.1)
aroma_data1_ten <- as.character(aroma_ten$Licence.Address.Line.2)
aroma_data2_ten <- as.character(aroma_ten$Licence.Address.Line.3)
aroma_addresses_ten <- paste0(aroma_data_ten, ", ", aroma_data1_ten, ", ", aroma_data2_ten)
aroma_geodata_ten <- geocode(aroma_addresses_ten)
aroma_geodata_ten <- unique(aroma_geodata_ten)
aroma_geodata_ten$Franchise <- "Aroma"
aroma_geodata_ten$Year <- as.factor(2006)
  
aroma_fifteen <- filter(fifteen_years_ago, grepl("^AROMA ESPRESSO", Operating.Name))
aroma_fifteen$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", aroma_fifteen$Licence.Address.Line.1)
aroma_data_fifteen <- as.character(aroma_fifteen$Licence.Address.Line.1)
aroma_data1_fifteen <- as.character(aroma_fifteen$Licence.Address.Line.2)
aroma_data2_fifteen <- as.character(aroma_fifteen$Licence.Address.Line.3)
aroma_addresses_fifteen <- paste0(aroma_data_fifteen, ", ", aroma_data1_fifteen, ", ", aroma_data2_fifteen)
aroma_geodata_fifteen <- geocode(aroma_addresses_fifteen)
aroma_geodata_fifteen <- unique(aroma_geodata_fifteen)
aroma_geodata_fifteen$Franchise <- "Aroma"
aroma_geodata_fifteen$Year <- as.factor(2001)

aroma_twenty <- filter(twenty_years_ago, grepl("^AROMA ESPRESSO", Operating.Name))
aroma_twenty$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", aroma_twenty$Licence.Address.Line.1)
aroma_data_twenty <- as.character(aroma_twenty$Licence.Address.Line.1)
aroma_data1_twenty <- as.character(aroma_twenty$Licence.Address.Line.2)
aroma_data2_twenty <- as.character(aroma_twenty$Licence.Address.Line.3)
aroma_addresses_twenty <- paste0(aroma_data_twenty, ", ", aroma_data1_twenty, ", ", aroma_data2_twenty)
aroma_geodata_twenty <- geocode(aroma_addresses_twenty)
aroma_geodata_twenty <- unique(aroma_geodata_twenty)
aroma_geodata_twenty$Franchise <- "Aroma"
aroma_geodata_twenty$Year <- as.factor(1996)

aroma_geodata_all <- rbind(aroma_geodata, aroma_geodata_five)

#aroma total counts
aroma_total <- aroma_geodata_all %>% group_by(Year) %>% summarise(count = n())
aroma_total_graph <- ggplot(aroma_total, aes(x=Year, y=count)) + geom_bar(stat="identity") + 
  scale_x_discrete(limit=c("2011", "2016")) +
  labs(title="Aroma", x="Year", y="Number of Locations") +
  theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.title.x = element_text(size=14, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold"))
aroma_total_graph


aroma_Map_Points <- ggmap(mymap) + geom_point(aes(x = lon, y = lat, size=1),
                                             data=aroma_geodata, alpha=.5, color="blue") +
  ggtitle("Aroma 2016") + theme(plot.title=element_text(face="bold", size=16)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "none")
aroma_Map_Points

aroma_map_five <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                            data=aroma_geodata_five, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
aroma_map_five

aroma_map_ten <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                           data=aroma_geodata_ten, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
aroma_map_ten

aroma_map_fifteen <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                               data=aroma_geodata_fifteen, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
aroma_map_fifteen

aroma_map_twenty <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                              data=aroma_geodata_twenty, alpha=1, color="blue")
aroma_map_twenty

aroma_map_all <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                           data=aroma_geodata_all, alpha=1, color="blue")
aroma_map_all + facet_grid(. ~ Year)


#regex
aroma_ops <- filter(current_operating, grepl("^AROMA", Operating.Name))

#La Prep
la_prep <- filter(current_operating, grepl("^LA PREP", Operating.Name))
la_prep$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", la_prep$Licence.Address.Line.1)
prep_data <- as.character(la_prep$Licence.Address.Line.1)
prep_data1 <- as.character(la_prep$Licence.Address.Line.2)
prep_data2 <- as.character(la_prep$Licence.Address.Line.3)
prep_addresses <- paste0(prep_data, ", ", prep_data1, ", ", prep_data2)
prep_geodata <- geocode(prep_addresses)
prep_geodata <- unique(prep_geodata)
prep_geodata$Franchise <- "La Prep"
prep_geodata$Year <- as.factor(2016)

la_prep_five <- filter(five_years_ago, grepl("^LA PREP", Operating.Name))
la_prep_five$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", la_prep$Licence.Address.Line.1)
prep_data_five <- as.character(la_prep_five$Licence.Address.Line.1)
prep_data1_five <- as.character(la_prep_five$Licence.Address.Line.2)
prep_data2_five <- as.character(la_prep_five$Licence.Address.Line.3)
prep_addresses_five <- paste0(prep_data_five, ", ", prep_data1_five, ", ", prep_data2_five)
prep_geodata_five <- geocode(prep_addresses_five)
prep_geodata_five <- unique(prep_geodata_five)
prep_geodata_five$Franchise <- "La Prep"
prep_geodata_five$Year <- as.factor(2011)

la_prep_ten <- filter(ten_years_ago, grepl("^LA PREP", Operating.Name))
la_prep_ten$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", la_prep_Licence.Address.Line.1)
prep_data_ten <- as.character(la_prep_ten$Licence.Address.Line.1)
prep_data1_ten <- as.character(la_prep_ten$Licence.Address.Line.2)
prep_data2_ten <- as.character(la_prep_ten$Licence.Address.Line.3)
prep_addresses_ten <- paste0(prep_data_ten, ", ", prep_data1_ten, ", ", prep_data2_ten)
prep_geodata_ten <- geocode(prep_addresses_ten)
prep_geodata_ten <- unique(prep_geodata_ten)
prep_geodata_ten$Franchise <- "La Prep"
prep_geodata_ten$Year <- as.factor(2006)

la_prep_fifteen <- filter(fifteen_years_ago, grepl("^LA PREP", Operating.Name))
la_prep_fifteen$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", la_prep_fifteen$Licence.Address.Line.1)
prep_data_fifteen <- as.character(la_prep_fifteen$Licence.Address.Line.1)
prep_data1_fifteen <- as.character(la_prep_fifteen$Licence.Address.Line.2)
prep_data2_fifteen <- as.character(la_prep_fifteen$Licence.Address.Line.3)
prep_addresses_fifteen <- paste0(prep_data_fifteen, ", ", prep_data1_fifteen, ", ", prep_data2_fifteen)
prep_geodata_fifteen <- geocode(prep_addresses_fifteen)
prep_geodata_fifteen <- unique(prep_geodata_fifteen)
prep_geodata_fifteen$Franchise <- "La Prep"
prep_geodata_fifteen$Year <- as.factor(2001)

la_prep_twenty <- filter(twenty_years_ago, grepl("^LA PREP", Operating.Name))
la_prep_twenty$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", la_prep_twenty$Licence.Address.Line.1)
prep_data_twenty <- as.character(la_prep_twenty$Licence.Address.Line.1)
prep_data1_twenty <- as.character(la_prep_twenty$Licence.Address.Line.2)
prep_data2_twenty <- as.character(la_prep_twenty$Licence.Address.Line.3)
prep_addresses_twenty <- paste0(prep_data_twenty, ", ", prep_data1_twenty, ", ", prep_data2_twenty)
prep_geodata_twenty <- geocode(prep_addresses_twenty)
prep_geodata_twenty <- unique(prep_geodata_twenty)
prep_geodata_twenty$Franchise <- "La Prep"
prep_geodata_twenty$Year <- as.factor(1996)

prep_geodata_all <- rbind(prep_geodata, prep_geodata_five, prep_geodata_ten)

#la prep total graphs
prep_total <- prep_geodata_all %>% group_by(Year) %>% summarise(count = n())
prep_total_graph <- ggplot(prep_total, aes(x=Year, y=count)) + geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("2006", "2011", "2016")) +
  labs(title="La Prep", x="Year", y="Number of Locations") +
  theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.title.x = element_text(size=14, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold"))
prep_total_graph

prep_Map_Points <- ggmap(mymap) + geom_point(aes(x = lon, y = lat, size=1),
                                             data=prep_geodata, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
prep_Map_Points

prep_map_five <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                           data=prep_geodata_five, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
prep_map_five

prep_map_ten <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                          data=prep_geodata_ten, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
prep_map_ten

prep_map_fifteen <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                              data=prep_geodata_fifteen, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
prep_map_fifteen

prep_map_twenty <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                             data=prep_geodata_twenty, alpha=.5, color="blue") + theme_nothing(legend=FALSE)
prep_map_twenty

prep_map_all <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                          data=prep_map_twenty, alpha=1, color="blue")
prep_map_all + facet_grid(. ~ Year)

prep_ops <- filter(current_operating, grepl("^LA PREP", Operating.Name))

#Mcdonalds 
mcdonalds <- current_operating %>% filter(Operating.Name == "MCDONALD'S RESTAURANT" | Operating.Name == "MCDONALD'S")
mcdonalds$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", mcdonalds$Licence.Address.Line.1)
mcdonalds_data <- as.character(mcdonalds$Licence.Address.Line.1)
mcdonalds_data1 <- as.character(mcdonalds$Licence.Address.Line.2)
mcdonalds_data2 <- as.character(mcdonalds$Licence.Address.Line.3)
mcdonalds_addresses <- paste0(mcdonalds_data, ", ", mcdonalds_data1, ", ", mcdonalds_data2)
mcdonalds_geodata <- geocode(mcdonalds_addresses)
mcdonalds_geodata <- unique(mcdonalds_geodata)
mcdonalds_geodata$Franchise <- "Mcdonalds"
mcdonalds_geodata$Year <- as.factor(2016)

mcdonalds_five <- filter(five_years_ago, grepl("^MCDONALD", Operating.Name))
mcdonalds_five$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", mcdonalds$Licence.Address.Line.1)
mcdonalds_data_five <- as.character(mcdonalds_five$Licence.Address.Line.1)
mcdonalds_data1_five <- as.character(mcdonalds_five$Licence.Address.Line.2)
mcdonalds_data2_five <- as.character(mcdonalds_five$Licence.Address.Line.3)
mcdonalds_addresses_five <- paste0(mcdonalds_data_five, ", ", mcdonalds_data1_five, ", ", mcdonalds_data2_five)
mcdonalds_geodata_five <- geocode(mcdonalds_addresses_five)
mcdonalds_geodata_five <- unique(mcdonalds_geodata_five)
mcdonalds_geodata_five$Franchise <- "Mcdonalds"
mcdonalds_geodata_five$Year <- as.factor(2011)

mcdonalds_ten <- filter(ten_years_ago, grepl("^MCDONALD", Operating.Name))
mcdonalds_ten$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", mcdonalds_ten$Licence.Address.Line.1)
mcdonalds_data_ten <- as.character(mcdonalds_ten$Licence.Address.Line.1)
mcdonalds_data1_ten <- as.character(mcdonalds_ten$Licence.Address.Line.2)
mcdonalds_data2_ten <- as.character(mcdonalds_ten$Licence.Address.Line.3)
mcdonalds_addresses_ten <- paste0(mcdonalds_data_ten, ", ", mcdonalds_data1_ten, ", ", mcdonalds_data2_ten)
mcdonalds_geodata_ten <- geocode(mcdonalds_addresses_ten)
mcdonalds_geodata_ten <- unique(mcdonalds_geodata_ten)
mcdonalds_geodata_ten$Franchise <- "Mcdonalds"
mcdonalds_geodata_ten$Year <- as.factor(2006)

mcdonalds_fifteen <- filter(fifteen_years_ago, grepl("^MCDONALD", Operating.Name))
mcdonalds_fifteen$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", mcdonalds_fifteen$Licence.Address.Line.1)
mcdonalds_data_fifteen <- as.character(mcdonalds_fifteen$Licence.Address.Line.1)
mcdonalds_data1_fifteen <- as.character(mcdonalds_fifteen$Licence.Address.Line.2)
mcdonalds_data2_fifteen <- as.character(mcdonalds_fifteen$Licence.Address.Line.3)
mcdonalds_addresses_fifteen <- paste0(mcdonalds_data_fifteen, ", ", mcdonalds_data1_fifteen, ", ", mcdonalds_data2_fifteen)
mcdonalds_geodata_fifteen <- geocode(mcdonalds_addresses_fifteen)
mcdonalds_geodata_fifteen <- unique(mcdonalds_geodata_fifteen)
mcdonalds_geodata_fifteen$Franchise <- "Mcdonalds"
mcdonalds_geodata_fifteen$Year <- as.factor(2001)

mcdonalds_twenty <- filter(twenty_years_ago, grepl("^MCDONALD", Operating.Name))
mcdonalds_twenty$Licence.Address.Line.1 <- gsub("(.*),.*", "\\1", mcdonalds_twenty$Licence.Address.Line.1)
mcdonalds_data_twenty <- as.character(mcdonalds_twenty$Licence.Address.Line.1)
mcdonalds_data1_twenty <- as.character(mcdonalds_twenty$Licence.Address.Line.2)
mcdonalds_data2_twenty <- as.character(mcdonalds_twenty$Licence.Address.Line.3)
mcdonalds_addresses_twenty <- paste0(mcdonalds_data_twenty, ", ", mcdonalds_data1_twenty, ", ", mcdonalds_data2_twenty)
mcdonalds_geodata_twenty <- geocode(mcdonalds_addresses_twenty)
mcdonalds_geodata_twenty <- unique(mcdonalds_geodata_twenty)
mcdonalds_geodata_twenty$Franchise <- "Mcdonalds"
mcdonalds_geodata_twenty$Year <- as.factor(1996)

mcdonalds_geodata_all <- rbind(mcdonalds_geodata, mcdonalds_geodata_five, mcdonalds_geodata_ten, mcdonalds_geodata_fifteen, mcdonalds_geodata_twenty)

mcdonalds_Map_Points <- ggmap(mymap) + geom_point(aes(x = lon, y = lat, size=1),
                                             data=mcdonalds_geodata, alpha=1, color="blue")
mcdonalds_Map_Points

mcdonalds_map_five <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                                data=mcdonalds_geodata_five, alpha=1, color="blue")
mcdonalds_map_five

mcdonalds_map_ten <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                               data=mcdonalds_geodata_ten, alpha=1, color="blue")
mcdonalds_map_ten

mcdonalds_map_fifteen <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                                   data=mcdonalds_geodata_fifteen, alpha=1, color="blue")
mcdonalds_map_fifteen

mcdonalds_map_twenty <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                                  data=mcdonalds_geodata_twenty, alpha=1, color="blue")
mcdonalds_map_twenty

mcdonalds_map_all <- ggmap(mymap) + geom_point(aes(x=lon, y=lat, size=1),
                                               data=mcdonalds_geodata_all, alpha=1, color="blue")
mcdonalds_map_all + facet_grid(. ~ Year)

mcdonalds_client <- filter(current_operating, grepl("^MCDONALD", Client.Name))
mcdonalds_ops <- filter(current_operating, grepl("^MCDONALD", Operating.Name))

#histogram of different franchises

#big three in 2016 (second cup, tim hortons, starbucks) create a bar graph of locations and a map
big_three <- rbind(star_geodata, tim_geodata, second_geodata)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=big_three) + facet_grid(. ~ Franchise)
ggplot(big_three, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")

big_three_five <- rbind(star_five_geodata, tim_five_geodata, second_geodata_five)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=big_three_five) + facet_grid(. ~ Franchise)
ggplot(big_three_five, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")

big_three_ten <- rbind(star_ten_geodata, tim_ten_geodata, second_geodata_ten)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=big_three_ten) + facet_grid(. ~ Franchise)
ggplot(big_three_ten, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")

big_three_fifteen <- rbind(star_fifteen_geodata, tim_fifteen_geodata, second_geodata_fifteen)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=big_three_fifteen) + facet_grid(. ~ Franchise)
ggplot(big_three_fifteen, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")

big_three_twenty <- rbind(star_twenty_geodata, tim_twenty_geodata, second_geodata_twenty)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=big_three_twenty) + facet_grid(. ~ Franchise)
ggplot(big_three_twenty, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")


smaller_shops <- rbind(ct_geodata, prep_geodata, aroma_geodata, timothys_geodata)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=smaller_shops) + facet_grid(. ~ Franchise)
ggplot(smaller_shops, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")

smaller_shops_five <- rbind(ct_geodata_five, prep_geodata_five, aroma_geodata_five, timothys_geodata_five)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=smaller_shops_five) + facet_grid(. ~ Franchise)
ggplot(smaller_shops_five, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")

smaller_shops_ten <- rbind(ct_geodata_ten, prep_geodata_ten, aroma_geodata_ten, timothys_geodata_ten)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=smaller_shops_ten) + facet_grid(. ~ Franchise)
ggplot(smaller_shops_ten, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")

smaller_shops_fifteen <- rbind(ct_geodata_fifteen, prep_geodata_fifteen, aroma_geodata_fifteen, timothys_geodata_fifteen)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=smaller_shops_fifteen) + facet_grid(. ~ Franchise)
ggplot(smaller_shops_fifteen, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")

smaller_shops_twenty <- rbind(ct_geodata_twenty, prep_geodata_twenty, aroma_geodata_twenty, timothys_geodata_twenty)
ggmap(mymap) + geom_point(aes(x=lon, y=lat), data=smaller_shops_twenty) + facet_grid(. ~ Franchise, nrow=2)
ggplot(smaller_shops_twenty, aes(x=Franchise)) + geom_bar(stat="count") + labs(y="Count", x="Franchise")


all_franchise <- rbind(star_geodata, tim_geodata, ct_geodata, prep_geodata, aroma_geodata, second_geodata, timothys_geodata)


#specific graphs

#the niche shop (ex. prep and aroma)
niche <- rbind(aroma_total, prep_total)







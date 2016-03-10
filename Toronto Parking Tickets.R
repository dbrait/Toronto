library(ggplot2)
library(plyr)
library(dplyr)
library(ggmap)
library(maps)
library(colorspace)
library(RColorBrewer)


Park1 <- read.csv("Parking_Tags_Data_2014_1.csv")
summary(Park1)
str(Park1)
Park2 <- read.csv("Parking_Tags_Data_2014_2.csv")
Park2$infraction_code <- as.factor(Park2$infraction_code)
Park2$date_of_infraction <- as.factor(Park2$date_of_infraction)
Park2$time_of_infraction <- as.factor(Park2$time_of_infraction)
summary(Park2)
str(Park2)
Park3 <- read.csv("Parking_Tags_Data_2014_3.csv")
Park3$infraction_code <- as.factor(Park3$infraction_code)
Park3$date_of_infraction <- as.factor(Park3$date_of_infraction)
Park3$time_of_infraction <- as.factor(Park3$time_of_infraction)
summary(Park3)
Park4 <- read.csv("Parking_Tags_Data_2014_4.csv")
summary(Park4)
str(Park4)
Park4$infraction_code <- as.factor(Park4$infraction_code)
Park4$date_of_infraction <- as.factor(Park4$date_of_infraction)
Park4$time_of_infraction <- as.factor(Park4$time_of_infraction)

#merge parking datasets
FullPark <- rbind(Park1, Park2, Park3, Park4)
summary(FullPark)

#Most ticketed locations
Edward <- FullPark %>% filter(location2 == "20 EDWARD ST")
summary(Edward)
Sunnybrook <- FullPark %>% filter(location2=="2075 BAYVIEW AVE")
summary(Sunnybrook)
UofTScar <- FullPark %>% filter(location2=="1265 MILITARY TRL")
summary(UofTScar)
HighPark <- FullPark %>% filter(location2=="1873 BLOOR ST W")
summary(HighPark)
ScarBluffs <- FullPark %>% filter(location2=="1 BRIMLEY RD S")
summary(ScarBluffs)

#Tickets at Yorkdale
Yorkdale <- FullPark %>% filter(location2=="3401 DUFFERIN ST")
summary(Yorkdale)

#Highest number of provinces
summary(FullPark$province)
ggplot(data=Park1, aes(x=province, y=set_fine_amount)) + geom_bar()


#Liberty Village Metro
FullPark %>% filter(location2 == "100 LYNN WILLIAMS ST")
FullPark %>% filter(location2 == "HANNA AVE")


#Worst times of day
summary(FullPark$time_of_infraction)
ggplot(data=Park1, aes(x=time_of_infraction, y=))

#Worst locations

#Most likely infractions

#most abused fire hydrants
Hydrant = FullPark %>% filter(infraction_code == 15)
summary(Hydrant)

#freight loading
Freight = FullPark %>% filter(infraction_code == 82)
summary(Freight)

#Park Longer than 3 hours
ParkLong = FullPark %>% filter(infraction_code == 2)
summary(ParkLong)

#10 million total revenue infractions
Infrac1 <- FullPark %>% filter(infraction_code == 16) #parking within 9metres of intersecting road ($40)
summary(Infrac1)
Infrac2 <- FullPark %>% filter(infraction_code == 5) #parking prohibited at day or time ($40)
summary(Infrac2)

#3 high price infractions ($450)
HighInfrac1 <- FullPark %>% filter(infraction_code == 355) #parking in handicapped spot
summary(HighInfrac1)
HighInfrac2 <- FullPark %>% filter(infraction_code == 363) #parking on street handicapped
summary(HighInfrac2)
HighInfrac3 <- FullPark %>% filter(infraction_code == 367) #stand on street accessible no permit
summary(HighInfrac3)

#2 fairly high price infractions (>$150)
MidInfrac1 <- FullPark %>% filter(infraction_code == 347) #Park in a fire route
summary(MidInfrac1)
MidInfrac2 <- FullPark %>% filter(infraction_code == 403) #stop signed highway rush hour
summary(MidInfrac2)

#Most costly fines
TotalCostly <- FullPark %>% filter(set_fine_amount >= 300)
summary(TotalCostly)

#Costly locations
ParkCollege <- FullPark %>% filter(location2 == "410 COLLEGE ST") %>% filter(set_fine_amount == 450)
summary(ParkCollege)
Grenville <- Park1 %>% filter(location2 == "18 GRENVILLE ST") %>% filter(set_fine_amount == 450)
summary(Grenville)

#Mount Sinai
MountSinai <- FullPark %>% filter(location2 == "60 MURRAY ST") %>% filter(set_fine_amount == 450)
summary(MountSinai)

#Woodbine
Woodbine <- FullPark %>% filter(location2 == "555 REXDALE BLVD") %>% filter(set_fine_amount == 450)
summary(Woodbine)

#Balmuto
Balmuto <- FullPark %>% filter(location2 == "35 BALMUTO ST") %>% filter(set_fine_amount==450)
Balmuto
summary(Balmuto)


#Top revenue producing infraction_codes, dates, locations and times
#do this by multiplying the count by mean set_fine_amount

by_Infrac <- group_by(FullPark, infraction_code)
Infraction <- summarise(by_Infrac, 
                  count = n(),
                  fin = mean(set_fine_amount),
                  totrev = count*fin)
Infraction
HighRevInfraction <- Infraction %>% filter(totrev > 1000000)
HighRevInfraction

by_loc <- group_by(FullPark, location2)
ByLocation <- summarise(by_loc,
                        count = n(),
                        fin = mean(set_fine_amount),
                        totrev = count*fin)
HighRevLoc <- ByLocation %>% filter(totrev > 175000)
HighRevLoc

#Unique addresses


by_loc <- group_by(FullPark, location2, mean(set_fine_amount))

ggplot(HighRevLoc, aes(location2, totrev)) + geom_bar(stat="identity",fill="blue", colour="darkgreen")

#google restricts to 2500 api requests a day, so just mapt the top 2500 locations
#sort by 2500 top totreve locations
toplocations <- arrange(ByLocation, desc(totrev))
toplocations <- slice(toplocations, 1:2000)

data <- as.character(toplocations$location2)
addresses <- paste0(data, ", Toronto, ON")

geodata <- geocode(addresses)
summary(geodata)
fulldata <- mutate(geodata, toplocations$totrev)
fulldata <- fulldata %>% rename(c("toplocations$totrev"="totalrev"))
summary(fulldata)
#By month


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
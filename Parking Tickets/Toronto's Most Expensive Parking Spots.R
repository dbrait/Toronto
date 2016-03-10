library(ggplot2)
library(dplyr)
library(ggmap)
library(maps)
library(colorspace)
library(RColorBrewer)
library(lubridate)
library(stringr)


setwd("C:/Users/My Files/Desktop/Data Science Resources/Toronto Data/Parking Tickets/2015")
Tag_1_2015 <- read.csv("Parking_Tags_Data_2015_1.csv")
Tag_2_2015 <- read.csv("Parking_Tags_Data_2015_2.csv")
Tag_3_2015 <- read.csv("Parking_Tags_Data_2015_3.csv")

setwd("C:/Users/My Files/Desktop/Data Science Resources/Toronto Data/Parking Tickets/2014")
Tag_1_2014 <- read.csv("Parking_Tags_Data_2014_1.csv")

#few of tag 1's columns are differ from standard format
Tag_1_2014$date_of_infraction <- as.integer(Tag_1_2014$date_of_infraction)
Tag_1_2014$infraction_code <- as.integer(Tag_1_2014$infraction_code)
Tag_1_2014$time_of_infraction <- as.integer(Tag_1_2014$time_of_infraction)

Tag_2_2014 <- read.csv("Parking_Tags_Data_2014_2.csv")
Tag_3_2014 <- read.csv("Parking_Tags_Data_2014_3.csv")
Tag_4_2014 <- read.csv("Parking_Tags_Data_2014_4.csv")

Park_2014 <- rbind(Tag_1_2014, Tag_2_2014, Tag_3_2014, Tag_4_2014)

setwd("C:/Users/My Files/Desktop/Data Science Resources/Toronto Data/Parking Tickets/2013")
Park_2013 <- read.csv("Parking_Tags_Data_2013.csv")


#fine of 450
Highest_Fine_2015 <- Park_2015 %>% filter(set_fine_amount == 450) %>% mutate(Year = 2015)
summary(Highest_Fine_2015)

Highest_Fine_2014 <- Park_2014 %>% filter(set_fine_amount == 450) %>% mutate(Year = 2014)
summary(Highest_Fine_2014)

Highest_Fine_2013 <- Park_2013 %>% filter(set_fine_amount == 450) %>% mutate(Year = 2013)
summary(Highest_Fine_2013)

Highest_Fine <- rbind(Highest_Fine_2015, Highest_Fine_2014, Highest_Fine_2013)

#infraction types that produce highest fine
Infrac_Highest_Fine_2015 <- Highest_Fine_2015 %>% group_by(infraction_code) %>% summarise(count = n(),
                                                                                          amount = sum(set_fine_amount))

Infrac_Highest_Fine_2014 <- Highest_Fine_2014 %>% group_by(infraction_code) %>% summarise(count = n(),
                                                                                          amount = sum(set_fine_amount))

Infrac_Highest_Fine_2013 <- Highest_Fine_2013 %>% group_by(infraction_code) %>% summarise(count = n(),
                                                                                          amount = sum(set_fine_amount))

#410 College St
College_410 <- Highest_Fine %>% filter(location2 == "410 COLLEGE ST") %>% group_by(Year, infraction_code) %>% summarise(count = n(),
                                                                                                                        amount = sum(set_fine_amount))


#18 Grenville St
Grenville_18 <- Highest_Fine %>% filter(location2 == "18 GRENVILLE ST") %>% group_by(Year, infraction_code) %>% summarise(count = n(),
                                                                                                                          amount = sum(set_fine_amount))


#35 Balmuto St
Balmuto_35 <- Highest_Fine %>% filter(location2 == "35 BALMUTO ST") %>% group_by(Year, infraction_code) %>% summarise(count = n(),
                                                                                                                      amount = sum(set_fine_amount))


#60 Murray St
Murray_60 <- Highest_Fine %>% filter(location2 == "60 MURRAY ST") %>% group_by(Year, infraction_code) %>% summarise(count = n(),
                                                                                                                    amount = sum(set_fine_amount))


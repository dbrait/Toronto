library(ggplot2)
library(dplyr)

setwd("C:/Users/My Files/Desktop/Data Science Resources/Toronto/Parking Tickets/2015")
Tag_1_2015 <- read.csv("Parking_Tags_Data_2015_1.csv")
Tag_2_2015 <- read.csv("Parking_Tags_Data_2015_2.csv")
Tag_3_2015 <- read.csv("Parking_Tags_Data_2015_3.csv")

Park_2015 <- rbind(Tag_1_2015, Tag_2_2015, Tag_3_2015)

setwd("C:/Users/My Files/Desktop/Data Science Resources/Toronto/Parking Tickets/2014")
Tag_1_2014 <- read.csv("Parking_Tags_Data_2014_1.csv")

#few of tag 1's columns are differ from standard format
Tag_1_2014$date_of_infraction <- as.integer(Tag_1_2014$date_of_infraction)
Tag_1_2014$infraction_code <- as.integer(Tag_1_2014$infraction_code)
Tag_1_2014$time_of_infraction <- as.integer(Tag_1_2014$time_of_infraction)

Tag_2_2014 <- read.csv("Parking_Tags_Data_2014_2.csv")
Tag_3_2014 <- read.csv("Parking_Tags_Data_2014_3.csv")
Tag_4_2014 <- read.csv("Parking_Tags_Data_2014_4.csv")

Park_2014 <- rbind(Tag_1_2014, Tag_2_2014, Tag_3_2014, Tag_4_2014)

setwd("C:/Users/My Files/Desktop/Data Science Resources/Toronto/Parking Tickets/2013")
Park_2013 <- read.csv("Parking_Tags_Data_2013.csv")


#fire hydrant infractions

#bind data for years


#do searches on the highest grossing addresses one by one with summary tables
Fire_Hydrant_2015 <- Park_2015 %>% filter(infraction_code==15) %>% mutate(Year = 2015)

Fire_Hydrant_2014 <- Park_2014 %>% filter(infraction_code==15) %>% mutate(Year = 2014)

Fire_Hydrant_2013 <- Park_2013 %>% filter(infraction_code==15) %>% mutate(Year = 2013)

Fire_Hydrant <- rbind(Fire_Hydrant_2015, Fire_Hydrant_2014, Fire_Hydrant_2013)


#99 Atlantic Avenue
Atlantic_99 <- Fire_Hydrant %>% filter(location2 == "99 ATLANTIC AVE") %>% group_by(Year) %>% summarise(count = n(),
                                                                                                        amount = sum(set_fine_amount))

#393 University Ave
University_393 <- Fire_Hydrant %>% filter(location2 == "393 UNIVERSITY AVE") %>% group_by(Year) %>% summarise(count = n(),
                                                                                                              amount = sum(set_fine_amount))


#33 Elmhurst Ave
Elmhurst_33 <- Fire_Hydrant %>% filter(location2 == "33 ELMHURST AVE") %>% group_by(Year) %>% summarise(count = n(),
                                                                                                        amount = sum(set_fine_amount))

#361 University Ave
University_361 <- Fire_Hydrant %>% filter(location2 == "361 UNIVERSITY AVE") %>% group_by(Year) %>% summarise(count = n(),
                                                                                                              amount = sum(set_fine_amount))


#56 The Esplanade
Esplanade_56 <- Fire_Hydrant %>% filter(location2 == "56 THE ESPLANADE") %>% group_by(Year) %>% summarise(count = n(),
                                                                                                         amount = sum(set_fine_amount))


#592 Markham St
Markham_592 <- Fire_Hydrant %>% filter(location2 == "592 MARKHAM ST") %>% group_by(Year) %>% summarise(count = n(),
                                                                                                       amount = sum(set_fine_amount))

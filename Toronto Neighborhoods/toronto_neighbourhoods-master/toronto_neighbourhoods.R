# Toronto Neighbourhood Population Chloropleths

library(RColorBrewer)
library(maptools)
library(ggmap)
library(rgeos)

# Read the neighborhood shapefile data and plot
shpfile <- "NEIGHBORHOODS_WGS84_2.shp"
sh <- readShapePoly(shpfile)
plot(sh)

# Add demographic data
# The neighbourhood ID is a string - change it to a integer
sh@data$AREA_S_CD <- as.numeric(sh@data$AREA_S_CD)

# Read in the demographic data and merge on Neighbourhood Id
demo <- read.csv(file="WB-Demographics.csv", header=T)
sh2 <- merge(sh, demo, by.x='AREA_S_CD', by.y='Neighbourhood.Id')

#read in recent immigrant data and merge on Neighbourhood Id
immigrant_demo <- read.csv(file="Recent Immigrants.csv", header=T)
sh3 <- merge(sh, immigrant_demo, by.x="AREA_S_CD", by.y="Neighbourhood.Id")

#read in visible minority data and merge on Neighbourhood Id
vis_minority_demo <- read.csv(file="Visible Minorities.csv", header=T)
sh4 <- merge(sh, vis_minority_demo, by.x="AREA_S_CD", by.y="Neighbourhood.Id")

# Set the palette
p <- colorRampPalette(c("white", "red"))(128)
palette(p)

# Scale the total population to the palette
pop <- sh2@data$Total.Population
cols <- (pop - min(pop))/diff(range(pop))*127+1
plot(sh, col=cols)

#RColorBrewer, spectral
p <- colorRampPalette(brewer.pal(11, 'Spectral'))(128)
palette(rev(p))
plot(sh2, col=cols)

#GGPLOT 
points <- fortify(sh, region = 'AREA_S_CD')

# Plot the neighborhoods
toronto <- qmap("Toronto, Ontario", zoom=10)
toronto + geom_polygon(aes(x=long,y=lat, group=group, alpha=0.25), data=points, fill='white') + geom_polygon(aes(x=long,y=lat, group=group), data=points, color='black', fill=NA)

# merge the shapefile data with the social housing data, using the neighborhood ID
points2 <- merge(points, demo, by.x='id', by.y='Neighbourhood.Id', all.x=TRUE)

#merge shapefile data with recent immigrant data using neigbourhood ID
points3 <- merge(points, immigrant_demo, by.x="id", by.y="Neighbourhood.Id", all.x=TRUE)

#merge shapefile data with visible minority data using neighbourhood ID
points4 <- merge(points, vis_minority_demo, by.x="id", by.y="Neighbourhood.Id", all.x=TRUE)

# Plot
toronto + geom_polygon(aes(x=long,y=lat, group=group, fill=Total.Population), data=points2, color='black') + 
  scale_fill_gradient(low='white', high='red')

# Spectral plot
toronto + geom_polygon(aes(x=long,y=lat, group=group, fill=Total.Population), data=points2, color='black') + 
  scale_fill_distiller(palette='Spectral') + scale_alpha(range=c(0.5,0.5))


#age breakdowns
mid_twenties <- sh2@data$Pop.20...24.years
twenties_cols <- (mid_twenties - min(mid_twenties))/diff(range(mid_twenties))*127+1
plot(sh, col=twenties_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop.20...24.years), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop.20...24.years), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


#gender imbalances
males <- sh2@data$Pop...Males
males_cols <- (males - min(males))/diff(range(males))*127+1
plot(sh, col=males_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop...Males), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop...Males), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


females <- sh2@data$Pop...Females
females_cols <- (females - min(females))/diff(range(females))*127+1
plot(sh, col=females_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop...Females), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop...Females), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

#neighborhood with most babies
babies <- sh2@data$Pop.0...4.years
babies_cols <- (babies - min(babies))/diff(range(babies))*127+1
plot(sh, col=babies_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop.0...4.years), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop.0...4.years), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


#neighborhood with most children
child <- sh2@data$Child.0.14
child_cols <- (child - min(child))/diff(range(child))*127+1
plot(sh, col=child_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Child.0.14), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Child.0.14), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


#neighborhood with most teenagers
teen <- sh2@data$Pop.15..19.years
teen_cols <- (teen - min(teen))/diff(range(teen))*127+1
plot(sh, col=teen_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop.15..19.years), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Pop.15..19.years), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

#seniors
seniors <- sh2@data$Seniors.65.and.over
seniors_cols <- (seniors - min(seniors))/diff(range(seniors))*127+1
plot(sh, col=seniors_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Seniors.65.and.over), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Seniors.65.and.over), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

#ethnic breakdowns
portuguese <- sh2@data$Language...Portuguese
portuguese_cols <- (portuguese - min(portuguese))/diff(range(portuguese))*127+1
plot(sh, col=portuguese_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Portuguese), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Portuguese), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


tamil <- sh2@data$Language...Tamil
tamil_cols <- (tamil - min(tamil))/diff(range(tamil))*127+1
plot(sh, col=tamil_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Tamil), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Tamil), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


italian <- sh2@data$Language...Italian
italian_cols <- (italian - min(italian))/diff(range(italian))*127+1
plot(sh, col=italian_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Italian), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Italian), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5,0.5))


russian <- sh2@data$Language...Russian
russian_cols <- (russian - min(russian))/diff(range(russian))*127+1
plot(sh, col=russian_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Russian), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Russian), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


chinese <- sh2@data$Language...Chinese
chinese_cols <- (chinese - min(chinese))/diff(range(chinese))*127+1
plot(sh, col=chinese_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Chinese), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Chinese), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


urdu <- sh2@data$Language...Urdu
urdu_cols <- (urdu - min(urdu))/diff(range(urdu))*127+1
plot(sh, col=urdu_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Urdu), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Urdu), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


tagalog <- sh2@data$Language...Tagalog
tagalog_cols <- (tagalog - min(tagalog))/diff(range(tagalog))*127+1
plot(sh, col=tagalog_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Tagalog), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Tagalog), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


spanish <- sh2@data$Language...Spanish
spanish_cols <- (spanish - min(spanish))/diff(range(spanish))*127+1
plot(sh, col=spanish_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Spanish), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Spanish), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


persian <- sh2@data$Language...Persian..Farsi.
persian_cols <- (persian - min(persian))/diff(range(persian))*127+1
plot(sh, col=persian_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Persian..Farsi.), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Persian..Farsi.), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


korean <- sh2@data$Language...Korean
korean_cols <- (korean - min(korean))/diff(range(korean))*127+1
plot(sh, col=korean_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Korean), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Language...Korean), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


#most recent immigrants (ADD IN ALL THE SUB GROUPS OF IMMIGRANTS)
recent_immigrants <- sh3@data$Recent.Immigrants.Category
recent_immigrants_cols <- (recent_immigrants - min(recent_immigrants))/diff(range(recent_immigrants))*127+1
plot(sh, col=recent_immigrants_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Recent.Immigrants.Category), data=points3, color="black") + 
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Recent.Immigrants.Category), data=points3, color="black") + 
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

South_Asian <- sh3@data$X...Southern.Asia
South_Asian_cols <- (South_Asian - min(South_Asian))/diff(range(South_Asian))*127+1
plot(sh, col=South_Asian_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Southern.Asia), data=points3, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Southern.Asia), data=points3, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

South_East_Asian <- sh3@data$X...South.East.Asia
South_East_Asian_cols <- (South_East_Asian - min(South_East_Asian))/diff(range(South_East_Asian))*127+1
plot(sh, col=South_East_Asian_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...South.East.Asia), data=points3, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...South.East.Asia), data=points3, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

East_Asian <- sh3@data$X...Eastern.Asia
East_Asian_cols <- (East_Asian - min(East_Asian))/diff(range(East_Asian))*127+1
plot(sh, col=East_Asian_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Eastern.Asia), data=points3, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Eastern.Asia), data=points3, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Mid_East <- sh3@data$X...West.Asia.Middle.East
Mid_East_cols <- (Mid_East - min(Mid_East))/diff(range(Mid_East))*127+1
plot(sh, col=Mid_East_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...West.Asia.Middle.East), data=points3, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...West.Asia.Middle.East), data=points3, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Africa <- sh3@data$X...Africa
Africa_cols <- (Africa - min(Africa))/diff(range(Africa))*127+1
plot(sh, col=Africa_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Africa), data=points3, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Africa), data=points3, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Europe <- sh3@data$X...Europe
Europe_cols <- (Europe - min(Europe))/diff(range(Europe))*127+1
plot(sh, col=Europe_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Europe), data=points3, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Europe), data=points3, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Americas <- sh3@data$x...Caribbean.Central.S..America
Americas_cols <- (Americas - min(Americas))/diff(range(Americas))*127+1
plot(sh, col=Americas_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Caribbean.Central.S..America), data=points3, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Caribbean.Central.S..America), data=points3, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


#visible minorities   (#ADD IN ALL THE SUB GROUPS OF VIS MINORITIES)
visible_minorities <- sh4@data$Visible.Minority.Category 
visible_minorities_cols <- (visible_minorities - min(visible_minorities))/diff(range(visible_minorities))*127+1
plot(sh, col=visible_minorities_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Visible.Minority.Category), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Visible.Minority.Category), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Chinese <- sh4@data$X...Chinese
Chinese_cols <- (Chinese - min(Chinese))/diff(range(Chinese))*127+1
plot(sh, col=Chinese_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Chinese), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Chinese), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

South_Asian <- sh4@data$X...South.Asian
South_Asian_cols <- (South_Asian - min(South_Asian))/diff(range(South_Asian))*127+1
plot(sh, col=South_Asian_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...South.Asian), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red") 

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...South.Asian), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Black <- sh4@data$X...Black
Black_cols <- (Black - min(Black))/diff(range(Black))*127+1
plot(sh, col=Black_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Black), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Black), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Filipino <- sh4@data$X...Filipino
Filipino_cols <- (Filipino - min(Filipino))/diff(range(Filipino))*127+1
plot(sh, col=Filipino_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Filipino), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Filipino), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Latin_American <- sh4@data$X...Latin.American
Latin_American_cols <- (Latin_American - min(Latin_American))/diff(range(Latin_American))*127+1
plot(sh, col=Latin_American_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Latin.American), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Latin.American), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Southeast_Asian <- sh4@data$X...Southeast.Asian 
Southeast_Asian_cols <- (Southeast_Asian - min(Southeast_Asian))/diff(range(Southeast_Asian))*127+1
plot(sh, col=Southeast_Asian_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Southeast.Asian), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Southeast.Asian), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Arab <- sh4@data$X...Arab
Arab_cols <- (Arab - min(Arab))/diff(range(Arab))*127+1
plot(sh, col=Arab_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Arab), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Arab), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

West_Asian <- sh4@data$X...West.Asian
West_Asian_cols <- (West_Asian - min(West_Asian))/diff(range(West_Asian))*127+1
plot(sh, col=West_Asian_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...West.Asian), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...West.Asian), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Japanese <- sh4@data$X...Japanese
Japanese_cols <- (Japanese - min(Japanese))/diff(range(Japanese))*127+1
plot(sh, col=Japanese_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Japanese), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Japanese), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Korean <- sh4@data$X...Korean
Korean_cols <- (Korean - min(Korean))/diff(range(Korean))*127+1
plot(sh, col=Korean_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Korean), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Korean), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

Other <- sh4@data$X...Other.Visible.Minority
Other_cols <- (Other - min(Other))/diff(range(Other))*127+1
plot(sh, col=Other_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Other.Visible.Minority), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Other.Visible.Minority), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


Multiple <- sh4@data$X...Multiple.Visible.Minority
Multiple_cols <- (Multiple - min(Multiple))/diff(range(Multiple))*127+1
plot(sh, col=Multiple_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Multiple.Visible.Minority), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Multiple.Visible.Minority), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))


Not_Visible <- sh4@data$X...Not.a.Visible.Minority
Not_Visible_cols <- (Not_Visible - min(Not_Visible))/diff(range(Not_Visible))*127+1
plot(sh, col=Not_Visible_cols)

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Not.a.Visible.Minority), data=points4, color="black") +
  scale_fill_gradient(low="white", high="red")

toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=X...Not.a.Visible.Minority), data=points4, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

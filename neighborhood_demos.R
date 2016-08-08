library(rgeos)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(ggmap)


shpfile <- "NEIGHBORHOODS_WGS84_2.shp"
sh <- readShapePoly(shpfile)
plot(sh)

# add demo data
#neighborhood id is a string, change to int
sh@data$AREA_S_CD <- as.numeric(sh@data$AREA_S_CD)

#read in demo data and merge on neighborhood id
demo <- read.csv(file="WB-Demographics.csv", header=T)
sh2 <- merge(sh, demo, by.x="AREA_S_CD", by.y="Neighbourhood.Id")

#set palette
p <- colorRampPalette(c("white", "red"))(128)
palette(p)

#scale pop to palette
pop <- sh2@data$Total.Population
cols <- (pop - min(pop))/diff(range(pop))*127+1
plot(sh, col=cols)

#spectral
p <- colorRampPalette(brewer.pal(11, "Spectral"))(128)
palette(rev(p))
plot(sh2, col=cols)

#ggmap
points <- fortify(sh, region="AREA_S_CD")

#plot neighborhoods
toronto <- qmap("Toronto, Ontario", zoom=10)
toronto + geom_polygon(aes(x=long, y=lat, group=group, alpha=0.25), data=points, fill="white") +
  geom_polygon(aes(x=long, y=lat, group=group), data=points, color="black", fill=NA)

#merge shapefile data with social housing data using neighborhood ID
points2 <- merge(points, demo, by.x="id", by.y="Neighbourhood.Id", all.x=TRUE)

#plot 
toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Total.Population), data=points2, color="black") +
  scale_fill_gradient(low="white", high="red")

#Spectral plot
toronto + geom_polygon(aes(x=long, y=lat, group=group, fill=Total.Population), data=points2, color="black") +
  scale_fill_distiller(palette="Spectral") + scale_alpha(range=c(0.5, 0.5))

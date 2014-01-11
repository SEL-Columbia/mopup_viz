require(RColorBrewer)
require(maptools)
require(ggplot2)
require(plyr)
require(stringr)
require(rgeos)
require(ggmap)
require(RgoogleMaps)


# load shapefile and facility list
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
nga_shp <- readShapeSpatial("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/nga_states/nga_states.shp")

# polygon manipulations
akwa_shp <- subset(nga_shp, Name == "Akwa Ibom")
akwa_shp_fortify <- fortify(akwa_shp, region="Name")




# facility data processing
facilities <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Education_774_NMIS_Facility.rds")

facilities$lat <- as.numeric(lapply(str_split(facilities$gps, " "), function(x) x[1]))
facilities$long <- as.numeric(lapply(str_split(facilities$gps, " "), function(x) x[2]))

facilities <- subset(facilities, state == "Akwa Ibom")


# bbox <- c(akwa_shp@bbox['x', 'min'], akwa_shp@bbox['y', 'min'],
#           akwa_shp@bbox['x', 'max'], akwa_shp@bbox['y', 'max'])

# 
# my_map <- get_map(location=bbox, maptype = 'satellite')
# ggmap(my_map) + 
#   geom_polygon(aes(x=long, y=lat, group=group), data=akwa_shp_fortify,
#               color = 'red', fill='black', alpha=0.2) +
#   coord_equal() 





# Plo

ggplot(akwa_shp_fortify, aes(x=long, y=lat)) +
  geom_polygon(alpha=0.2) +
  geom_path(color="red") +
  geom_point(data=facilities, aes(x=long, y=lat, color='green'), size=1) + 
  geom_vline(xintercept = seq(akwa_shp@bbox['x', 'min'], 
                              akwa_shp@bbox['x', 'max'], length.out=4)) + 
  geom_hline(yintercept = seq(akwa_shp@bbox['y', 'min'],
                              akwa_shp@bbox['y', 'max'], length.out=4)) +

  coord_equal() 
  



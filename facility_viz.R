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



grid_x <- seq(akwa_shp@bbox['x', 'min'], 
         akwa_shp@bbox['x', 'max'], length.out=4)
grid_y <- seq(akwa_shp@bbox['y', 'min'],
         akwa_shp@bbox['y', 'max'], length.out=4)

# x_center <- get_grid_mean(grid_x)
# y_center <- get_grid_mean(grid_y)

# text_center <- expand.grid(x=x_center, y=y_center)
# text_center$word <- 1:nrow(text_center)
# 
# get_grid_mean <- function(x){
#   apply(cbind(x[1:length(x)-1], x[2:length(x)]), 1, mean)
# }

get_grid_zoomin_bbox <- function(x, y){
  x_coor <- data.frame(x_min = x[1:length(x)-1], 
                       x_max = x[2:length(x)])
  y_coor <- data.frame(y_min = y[1:length(y)-1],
                       y_max = y[2:length(y)])
  bbox_df <- merge(x_coor, y_coor, by=NULL)
  bbox_df$x_center <- rowMeans(bbox_df[, c('x_min', 'x_max')])
  bbox_df$y_center <- rowMeans(bbox_df[, c('y_min', 'y_max')])
  bbox_df$word <- 1:nrow(bbox_df)
  return(bbox_df)
}

bbox_data <- get_grid_zoomin_bbox(grid_x, grid_y)


lga_name <- "Akwa Ibom"
# Plotting
ggplot(akwa_shp_fortify, aes(x=long, y=lat)) +
  geom_polygon(alpha=0.2) +
  geom_path(color="red") +
  geom_point(data=facilities, aes(x=long, y=lat), color='black', size=1) + 
  geom_vline(xintercept = grid_x) + 
  geom_hline(yintercept = grid_y) +
  geom_text(data=bbox_df, aes(x=x_center, y=y_center, label=word), size=10, color='blue') + 
  coord_equal() + 
  theme(panel.grid=element_blank()) + 
  labs(title = paste('Map of', lga_name,sep=' '))


current_bbox_df <- bbox_data[1,]
# zoom-in view
ggplot(akwa_shp_fortify, aes(x=long, y=lat)) +
  geom_polygon(alpha=0.2) +
  geom_path(color="red") +
  geom_point(data=facilities, aes(x=long, y=lat), color='black', size=1) + 
  geom_vline(xintercept = grid_x) + 
  geom_hline(yintercept = grid_y) +
  coord_equal() +
  theme(panel.grid=element_blank()) +
  scale_x_continuous(limits=c(current_bbox_df$x_min, 
                              current_bbox_df$x_max)) + 
  scale_y_continuous(limits=c(current_bbox_df$y_min, 
                              current_bbox_df$y_max)) + 
  labs(title = paste('Map of Area', current_bbox_df$word, sep=' '))




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
nga_shp <- readShapeSpatial("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/nga_lgas/nga_lgas_with_corrected_id.shp")

# polygon manipulations
current_shp <- subset(nga_shp, lga_id == "2")
current_shp_fortify <- fortify(current_shp, region="lga_id")




# facility data processing
facilities <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Education_774_NMIS_Facility.rds")

facilities$lat <- as.numeric(lapply(str_split(facilities$gps, " "), function(x) x[1]))
facilities$long <- as.numeric(lapply(str_split(facilities$gps, " "), function(x) x[2]))

current_facilities <- subset(facilities, lga_id == "2")

#### getting separating grids
grid_x <- seq(current_shp@bbox['x', 'min'], 
         current_shp@bbox['x', 'max'], length.out=4)
grid_y <- seq(current_shp@bbox['y', 'min'],
         current_shp@bbox['y', 'max'], length.out=4)


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


lga_name <- current_shp_fortify$id[1]

# Plotting lga level
ggplot(current_shp_fortify, aes(x=long, y=lat)) +
  geom_polygon(alpha=0.2) +
  geom_path(color="red") +
  geom_point(data=current_facilities, aes(x=long, y=lat), color='black', size=1) + 
  geom_vline(xintercept = grid_x) + 
  geom_hline(yintercept = grid_y) +
  geom_text(data=bbox_data, aes(x=x_center, y=y_center, label=word), size=10, color='blue') + 
  coord_equal() + 
  theme(panel.grid=element_blank()) + 
  labs(title = paste('Map of lga_id ==', lga_name,sep=' '))

# Plotting area zoom in level
getting_zoomin_graph <- function(current_bbox_df){
  plot <- ggplot(current_shp_fortify, aes(x=long, y=lat)) +
    geom_polygon(alpha=0.2) +
    geom_path(color="red") +
    geom_point(data=current_facilities, aes(x=long, y=lat), color='black', size=1) + 
    geom_vline(xintercept = grid_x) + 
    geom_hline(yintercept = grid_y) +
    coord_equal() +
    theme(panel.grid=element_blank(),
          panel.background = element_blank()) +
    coord_map(xlim=c(current_bbox_df$x_min, current_bbox_df$x_max),
              ylim=c(current_bbox_df$y_min, current_bbox_df$y_max)) +   
    labs(title = paste('Map of Area', current_bbox_df$word, sep=' '))
  print(plot)
}

d_ply(bbox_data, .(word), function(df) getting_zoomin_graph(df))


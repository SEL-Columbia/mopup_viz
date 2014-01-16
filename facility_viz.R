require(RColorBrewer)
require(maptools)
require(ggplot2)
require(plyr)
require(stringr)
require(rgeos)
require(ggmap)
require(RgoogleMaps)
require(OpenStreetMap)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
nga_shp <- readShapeSpatial("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/nga_lgas/nga_lgas_with_corrected_id.shp", proj4string=wgs84)

# facility data processing
facilities <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Normalized/Education_774_NMIS_Facility.rds")
facilities$lat <- as.numeric(lapply(str_split(facilities$gps, " "), function(x) x[1]))
facilities$long <- as.numeric(lapply(str_split(facilities$gps, " "), function(x) x[2]))
# facilities <- facilities[1:60,]

# # current lga data subsetting
current_shp <- subset(nga_shp, lga_id == "2")
# current_shp_fortify <- fortify(current_shp, region="Name")
current_facilities <- subset(facilities, lga_id == "2")



### getting grid line coordinates
get_grids <- function(current_shp, nrow=3, ncol=3){
  
  ncol <- ncol + 1
  nrow <- nrow + 1
  
  grid_x <- seq(current_shp@bbox['x', 'min'], 
                current_shp@bbox['x', 'max'], length.out=ncol)
  grid_y <- seq(current_shp@bbox['y', 'min'],
                current_shp@bbox['y', 'max'], length.out=nrow)
  
  grid_df <- data.frame(x=grid_x, y=grid_y)
  return(grid_df)
}



#### define function for grid line df 
get_grid_zoomin_bbox <- function(grid_df){
  
  x <- grid_df$x
  y <- grid_df$y
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

# download map img from osm
# get_osm_map <- function(current_shp){
#   
#     map <- openmap(upperLeft=c(lat = current_shp@bbox["y","max"], 
#                              lon=current_shp@bbox["x","min"]), 
#                  lowerRight=c(lat = current_shp@bbox["y","min"], 
#                               lon=current_shp@bbox["x","max"]),
#                  type="osm", minNumTiles=5)
#     map <- openproj(map) 
#     return(map)
# }                                     

# download ZOOM IN map img from osm
get_osm_map <- function(current_bbox_df){
    
    n <- dim(current_bbox_df)[1]
    map <- openmap(upperLeft=c(lat = current_bbox_df$y_max[n], 
                               lon=current_bbox_df$x_min[1]), 
                   lowerRight=c(lat = current_bbox_df$y_min[1], 
                                lon=current_bbox_df$x_max[n]),
                   type="osm", minNumTiles=5)
    map <- openproj(map) 
    return(map)
}                                     

# Plotting lga level
getting_lga_graph <- function(current_shp_fortify, current_facilities, 
                              bbox_data, grid_lines){
    
    lga_name <- current_shp_fortify$id[1]
    osm_map <- get_osm_map(bbox_data)
        
    plot <- autoplot(osm_map, expand=F) + 
        geom_point(data=current_facilities, 
                   aes(x=long, y=lat), 
                   color=I('red'), size=5, shape='+') + 
        geom_polygon(data=current_shp_fortify, 
                     aes(x=long, y=lat, group=group), 
                     fill='black', color='black', alpha=0.05) + 
        geom_vline(xintercept = grid_lines$x) + 
        geom_hline(yintercept = grid_lines$y) +
        geom_text(data=bbox_data, 
                  aes(x=x_center, y=y_center, label=word),
                  size=11, color='blue', alpha=0.4) + 
        coord_equal() + 
        theme(panel.grid=element_blank(),
              panel.background = element_blank()) + 
        labs(title = paste('Map of', lga_name,sep=' ')) + 
        xlab("Longitude") + ylab("Latitude")
    print(plot)
}

# Plotting area zoom in level
getting_zoomin_graph <- function(current_bbox_df, current_shp_fortify, 
                                 current_facilities, grid_lines){
    
    osm_map <- get_osm_map(current_bbox_df)

    plot <- autoplot(osm_map, expand=F) + 
        geom_polygon(data=current_shp_fortify, 
                     aes(x=long, y=lat, group=group), 
                     fill='black', color='black', alpha=0.05) + 
        geom_point(data=current_facilities, 
                   aes(x=long, y=lat), 
                   color=I('red'), size=5, shape='+') + 
        geom_vline(xintercept = grid_lines$x) + 
        geom_hline(yintercept = grid_lines$y) +
        coord_equal() +
        theme(panel.grid=element_blank(),
              panel.background = element_blank()) +
        coord_cartesian(xlim=c(current_bbox_df$x_min, 
                               current_bbox_df$x_max),
                        ylim=c(current_bbox_df$y_min, 
                               current_bbox_df$y_max)) +   
        labs(title = paste('Map of Area', 
                           current_bbox_df$word, sep=' ')) +
        xlab("Longitude") + ylab("Latitude")
    print(plot)
}

# Creating master function to plot lga overview + zoomin level all at once
lga_viz <- function(current_shp, current_facilities){
    
    current_shp_fortify <- fortify(current_shp, region="Name")
    grid_lines <- get_grids(current_shp, 3, 3)
    bbox_data <- get_grid_zoomin_bbox(grid_lines)
    
    getting_lga_graph(current_shp_fortify, current_facilities,
                      bbox_data, grid_lines)
    
    d_ply(bbox_data, .(word), function(df) getting_zoomin_graph(df, current_shp_fortify,
                                                                current_facilities, grid_lines))
    
}


# Below chunk is for testing only
# grid_lines <- get_grids(current_shp, 3, 3)
# bbox_data <- get_grid_zoomin_bbox(grid_lines)
# lga_name <- current_shp_fortify$id[1]
# 
# current_bbox_df <- subset(bbox_data, word == "1")
# 
# getting_zoomin_graph(current_bbox_df, current_shp_fortify,
#                      current_facilities, grid_lines)
# 
# getting_lga_graph(current_shp_fortify, current_facilities, bbox_data, grid_lines)
# # single lga level 

pdf("./lga1.pdf")
lga_viz(current_shp, current_facilities)
dev.off()




# # Plotting lga level
# getting_lga_graph <- function(current_shp_fortify, current_facilities, 
#                               osm_map, bbox_data, grid_lines){
#     
#     lga_name <- current_shp_fortify$id[1]
#     osm_map <- get_osm_map(current_shp)
#     
# 
#     plot <- autoplot(osm_map, expand=F) + 
#           geom_point(data=current_facilities, 
#                    aes(x=long, y=lat), 
#                      color=I('red'), size=5, shape='+') + 
#           geom_polygon(data=current_shp_fortify, 
#                        aes(x=long, y=lat, group=group), 
#                        fill='black', color='black', alpha=0.05) + 
#           geom_vline(xintercept = grid_lines$x) + 
#           geom_hline(yintercept = grid_lines$y) +
#           geom_text(data=bbox_data, 
#                     aes(x=x_center, y=y_center, label=word),
#                     size=11, color='blue', alpha=0.4) + 
#           coord_equal() + 
#           theme(panel.grid=element_blank(),
#                 panel.background = element_blank()) + 
#           labs(title = paste('Map of', lga_name,sep=' ')) + 
#           xlab("Longitude") + ylab("Latitude")
#   print(plot)
# }
# 
# # Plotting area zoom in level
# getting_zoomin_graph <- function(current_bbox_df, current_shp_fortify, 
#                                  current_facilities, osm_map, grid_lines){
#   
#   plot <-   autoplot(osm_map, expand=F) + 
#             geom_polygon(data=current_shp_fortify, 
#                         aes(x=long, y=lat, group=group), 
#                         fill='black', color='black', alpha=0.05) + 
#             geom_point(data=current_facilities, 
#                        aes(x=long, y=lat), 
#                        color=I('red'), size=5, shape='+') + 
#             geom_vline(xintercept = grid_lines$x) + 
#             geom_hline(yintercept = grid_lines$y) +
#             coord_equal() +
#             theme(panel.grid=element_blank(),
#                   panel.background = element_blank()) +
#             coord_cartesian(xlim=c(current_bbox_df$x_min, 
#                                  current_bbox_df$x_max),
#                             ylim=c(current_bbox_df$y_min, 
#                                  current_bbox_df$y_max)) +   
#             labs(title = paste('Map of Area', 
#                                current_bbox_df$word, sep=' ')) +
#             xlab("Longitude") + ylab("Latitude")
#   print(plot)
# }
# 
# # Creating master function to plot lga overview + zoomin level all at once
# lga_viz <- function(current_shp, current_facilities){
#   
#   current_shp_fortify <- fortify(current_shp, region="Name")
#   osm_map <- get_osm_map(current_shp)
#   grid_lines <- get_grids(current_shp, 3, 3)
#   bbox_data <- get_grid_zoomin_bbox(grid_lines)
#   
#   getting_lga_graph(current_shp_fortify, current_facilities,
#                     osm_map, bbox_data, grid_lines)
#   
#   d_ply(bbox_data, .(word), function(df) getting_zoomin_graph(df, current_shp_fortify,
#                                           current_facilities, osm_map, grid_lines))
#   
# }

# lga_viz <- function(current_shp, current_facilities){
#     
#     current_shp_fortify <- fortify(current_shp, region="Name")
#     osm_map <- get_osm_map(current_shp)
#     grid_lines <- get_grids(current_shp, 3, 3)
#     bbox_data <- get_grid_zoomin_bbox(grid_lines)
#     
#     getting_lga_graph(current_shp_fortify, current_facilities,
#                       osm_map, bbox_data, grid_lines)
#     
#     d_ply(bbox_data, .(word), function(df) getting_zoomin_graph(df, current_shp_fortify,
#                                                                 current_facilities, osm_map, grid_lines))
#     
# }




### The BIG Loop
# pdf("./all_lgas.pdf")
# 
# d_ply(facilities, .(lga_id), function(df){
#   current_shp <- subset(nga_shp, lga_id == df$lga_id[1])
#   lga_viz(current_shp, df)
# })
# 
# dev.off()

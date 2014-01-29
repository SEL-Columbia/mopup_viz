require(RColorBrewer)
require(maptools)
require(ggplot2)
require(plyr)
require(stringr)
require(rgeos)
require(ggmap)
require(RgoogleMaps)
require(OpenStreetMap)
require(xtable)
require(gridExtra)
require(RSAGA)

wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
nga_shp <- readShapeSpatial("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/nga_lgas/nga_lgas_with_corrected_id.shp", proj4string=wgs84)

# missing facility list data processing
missing_edu <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result/facility_missing_list_edu.csv")
missing_health <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result/facility_missing_list_health.csv")

missing_edu <- subset(missing_edu, 
                     select=c("short_id", "facility_name", "ward",
                              "community", "facility_type", "lga_id"))
missing_health <- subset(missing_health, 
                      select=c("short_id", "facility_name", "ward",
                               "community", "facility_type", "lga_id"))

# nmis data processing
nmis_edu <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/Education_774_NMIS_Facility.csv")
nmis_edu$lat <- as.numeric(lapply(str_split(nmis_edu$gps, " "), function(x) x[1]))
nmis_edu$long <- as.numeric(lapply(str_split(nmis_edu$gps, " "), function(x) x[2]))
nmis_edu <- subset(nmis_edu, select=c("facility_name", "community", "ward", "lat", "long",
                                    "facility_ID", "facility_type", "lga_id"))

# # current lga data subsetting
current_shp <- subset(nga_shp, lga_id == "2")
current_shp_fortify <- fortify(current_shp, region="Name")
current_facilities <- subset(nmis_edu, lga_id == "2")
current_missing <- subset(missing_edu, lga_id == "2")



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
get_osm_map <- function(current_bbox_df, tile_level = 5){
    
    n <- dim(current_bbox_df)[1]
    map <- openmap(upperLeft=c(lat = current_bbox_df$y_max[n], 
                               lon = current_bbox_df$x_min[1]), 
                   lowerRight=c(lat = current_bbox_df$y_min[1], 
                                lon = current_bbox_df$x_max[n]),
                   type="osm", minNumTiles = tile_level)
    map <- openproj(map) 
    return(map)
}                                     

facility_subset_griddf <- function(current_bbox_df, current_facilities_seriel_added){
    x_min <- current_bbox_df$x_min
    x_max <- current_bbox_df$x_max
    y_min <- current_bbox_df$y_min
    y_max <- current_bbox_df$y_max
    map_num <- current_bbox_df$word
    
    current_facilities_seriel_added <- subset(current_facilities_seriel_added, ( 
                                    long >= x_min & long <= x_max &
                                    lat >= y_min & lat <= y_max),
                                    select = c("seriel_ID", "facility_name", 
                                               "community", "ward", "facility_type",
                                               "facility_ID"))
    current_facilities_seriel_added$map <- rep(map_num, nrow(current_facilities_seriel_added))
    current_facilities_seriel_added <- current_facilities_seriel_added[,c("map", "seriel_ID",
                                                                          "facility_name", "ward", 
                                                                          "community", "facility_type",
                                                                          "facility_ID")]
    current_facilities_seriel_added <- arrange(current_facilities_seriel_added, seriel_ID)
    current_facilities_seriel_added <- rename(current_facilities_seriel_added, 
                                              replace=c("map" = "MAP",
                                                        "seriel_ID" = "ID#",
                                                        "facility_name" = "NAME",
                                                        "ward" = "WARD",
                                                        "community" = "COMMUNITY",
                                                        "facility_type" = "TYPE",
                                                        "facility_ID" = "FACILITY_ID"))
    
    if (nrow(current_facilities_seriel_added) > 0){
        title_name <- paste("Facilities that already surveyed in Area -", map_num, sep=' ')
        grid_table_assemble(current_facilities_seriel_added, title_name)
    }
    
}

#### turn to-go-list into functions



grid_table_assemble <- function(df, title_name){
    table <- tableGrob(df, show.rownames = FALSE, row.just = "left",
                       col.just = "center", 
                       gpar.corefill = gpar(fill = "white", col = "black"))
    grid.newpage()
    h <- grobHeight(table)
    w <- grobWidth(table)
    title <- textGrob(title_name, y=unit(0.5,"npc") + 0.5*h, 
                      vjust=0, gp=gpar(fontsize=20, fontface = "bold"))
    gt <- gTree(children=gList(table, title))
    grid.draw(gt)
}



# Plotting lga level
getting_lga_graph <- function(current_shp_fortify, current_facilities, 
                              bbox_data, grid_lines){
    
    lga_name <- current_shp_fortify$id[1]
    osm_map <- get_osm_map(bbox_data)
        
    plot <- autoplot(osm_map, expand=F) + 
        geom_point(data=current_facilities, 
                   aes(x=long, y=lat), 
                   color=I('black'), size=2) + 
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
    print(plot, width = 7, height = 7)
}

# Plotting area zoom in level
getting_zoomin_graph <- function(current_bbox_df, current_shp_fortify, 
                                 current_facilities, grid_lines){
    
    osm_map <- get_osm_map(current_bbox_df)
    text_df <- subset(current_facilities, 
                      !duplicated(seriel_ID),
                      select = c("long", "lat", "seriel_ID"))

    plot <- autoplot(osm_map, expand=F) + 
        geom_polygon(data=current_shp_fortify, 
                     aes(x=long, y=lat, group=group), 
                     fill='black', color='black', alpha=0.05) + 
        geom_point(data=current_facilities, 
                   aes(x=long, y=lat), 
                   color=I('red'), size=5) + 
        geom_vline(xintercept = grid_lines$x) + 
        geom_hline(yintercept = grid_lines$y) +
        geom_text(data=text_df, 
                  aes(x=long, y=lat, label=seriel_ID),
                  color='black', size=3, vjust=0) + 
        theme(panel.grid=element_blank(),
              panel.background = element_blank()) +
        coord_fixed(ratio=1, xlim=c(current_bbox_df$x_min, 
                               current_bbox_df$x_max),
                        ylim=c(current_bbox_df$y_min, 
                               current_bbox_df$y_max)) +   
        labs(title = paste('Map of Area', 
                           current_bbox_df$word, sep=' ')) +
        xlab("Longitude") + ylab("Latitude")
    print(plot)
}

### helper function to create proper serial_ID for solving near points issue
facility_get_serial_ID <- function(current_facilities){
    getting_closest_point <- function(current_facilities){
        
        facility_copy <- current_facilities
        facility_copy$facility_ID.y <- facility_copy$facility_ID
        facility_copy$lat.y <- facility_copy$lat
        facility_copy$long.y <- facility_copy$long
        facility_copy$seriel_ID.y <- facility_copy$seriel_ID
        
        
        facility_list <- ddply(current_facilities, .(facility_ID), function(df){
            candidate <- facility_copy
            candidate <- subset(candidate, facility_ID.y != df$facility_ID[1])
            closest_df <- pick.from.points(data=df, src=candidate, 
                                           X.name="lat", Y.name="long", 
                                           radius=10,
                                           pick=c('facility_ID.y', 'lat.y', 'long.y', 'seriel_ID.y'),
                                           set.na=TRUE)
            
            closest_df$dist <- sqrt((closest_df$lat - closest_df$lat.y)^2 + (closest_df$long - closest_df$long.y)^2)
            return(closest_df)
        })
        facility_list <- arrange(facility_list, seriel_ID)
        return(facility_list)
    }
    
    
    re_assign_serial_ID <- function(facility_list){
        idx <- which(facility_list$dist < 0.0012)
        for (i in idx){
            
            id <- which(facility_list$seriel_ID == facility_list$seriel_ID.y[i])
            
            facility_list$seriel_ID[id] <- facility_list$seriel_ID[i]
            facility_list$seriel_ID.y[idx][which(facility_list$seriel_ID.y[idx] == facility_list$seriel_ID.y[i])] <- facility_list$seriel_ID[i]
        }
        return(facility_list)    
    }
    
    # calling internal function to get result
    current_facilities$seriel_ID <- 1:nrow(current_facilities)
    facility_list <- re_assign_serial_ID(getting_closest_point(current_facilities))
    facility_list <- subset(facility_list, select= -c(facility_ID.y, lat.y, long.y, seriel_ID.y))
    
    return(facility_list)
}

# Creating master function to plot lga overview + zoomin level all at once
lga_viz <- function(current_shp, current_facilities, current_missing){
    
    current_shp_fortify <- fortify(current_shp, region="Name")
    grid_lines <- get_grids(current_shp, 3, 3)
    bbox_data <- get_grid_zoomin_bbox(grid_lines)
    current_facilities <- facility_get_serial_ID(current_facilities)
    
    # print lga level map of current lga
    getting_lga_graph(current_shp_fortify, current_facilities,
                      bbox_data, grid_lines)
    # print missing facility list of current lga
    if (nrow(current_missing) > 0){
        lga_name <- current_shp_fortify$id[1]
        title_name <- paste("Facilities that need to be surveyed -", lga_name, sep=' ')
        grid_table_assemble(current_missing, title_name)
    }
    
    
    # zoomin starts here
    d_ply(bbox_data, .(word), function(df) {
        # print zoomin small map
        getting_zoomin_graph(df, current_shp_fortify,
                             current_facilities, grid_lines)
        # print NOT TO GO list of the small map
        facility_subset_griddf(df, current_facilities)
    })
}




# Below chunk is for testing only
grid_lines <- get_grids(current_shp, 3, 3)
bbox_data <- get_grid_zoomin_bbox(grid_lines)
lga_name <- current_shp_fortify$id[1]

current_bbox_df <- subset(bbox_data, word == "1")

getting_lga_graph(current_shp_fortify, current_facilities, bbox_data, grid_lines)

getting_zoomin_graph(current_bbox_df, current_shp_fortify,
                     current_facilities, grid_lines)


# single lga level 

pdf("./lga1.pdf", width = 12, height = 8)
    lga_viz(current_shp, current_facilities, current_missing)
dev.off()

pdf("./test.pdf", width = 20, height = 8)
grid.newpage()
grid.table(missing_edu[1:20,], show.rownames = FALSE, row.just = "left",
           col.just = "center", gpar.corefill = gpar(fill = "white", col = "black")) 
dev.off()




table <- tableGrob(missing_edu[1:20,], show.rownames = FALSE, row.just = "left",
                   col.just = "center", 
                   gpar.corefill = gpar(fill = "white", col = "black"))

grid.newpage()
h <- grobHeight(table)
w <- grobWidth(table)
lga_nm <- "Abaji"


######

### The BIG Loop
# pdf("./all_lgas.pdf")
# 
# d_ply(facilities, .(lga_id), function(df){
#   current_shp <- subset(nga_shp, lga_id == df$lga_id[1])
#   lga_viz(current_shp, df)
# })
# 
# dev.off()



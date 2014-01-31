require(RColorBrewer)
require(ggplot2)
require(plyr)
require(stringr)
require(rgeos)
require(ggmap)
require(RgoogleMaps)
require(OpenStreetMap)
require(xtable)
require(gridExtra)
require(RANN)

# RUN MakeData.R if you haven't already; these files are generated there
missing_edu <- readRDS("data/MissingEducationFacilities.rds")
missing_health <- readRDS("data/MissingHealthFacilities.rds")
nmis_edu <- readRDS("data/NMISEducationFacilities.rds")
nmis_health <- readRDS("data/NMISHealthFacilities.rds")
nga_shp <- readRDS("data/NGALGAS_shp.rds")
nga_shp_fortified <- readRDS("data/NGALGAS_fortified.rds")

# # current lga data subsetting
get_data_for_current_lga = function(LGA_ID) {
    shp = subset(nga_shp, lga_id==LGA_ID)
    list(
        missing_edu = subset(missing_edu, lga_id==LGA_ID, select=-c(lga_id)),
        missing_health = subset(missing_health, lga_id==LGA_ID, select=-c(lga_id)),
        nmis_edu = subset(nmis_edu, lga_id==LGA_ID),
        nmis_health = subset(nmis_health, lga_id==LGA_ID),
        shp = shp,
        shp_fortified = subset(nga_shp_fortified, lga_id==LGA_ID),
        name = shp$Name
    )
}

### Given shapefile, break it up into a grid.
# Grid size will be approx: xwidth X ywidth (defaulted to 0.2)
get_grids <- function(current_shp, xwidth=0.2, ywidth=0.2){
    
    y <- current_shp@bbox["y","max"] - current_shp@bbox["y","min"]
    x <- current_shp@bbox["x","max"] - current_shp@bbox["x","min"]
        
    nrow <- round(y / xwidth) + 1
    ncol <- round(x / ywidth) + 1
    
    grid_x <- seq(current_shp@bbox['x', 'min'], 
                    current_shp@bbox['x', 'max'], length.out=ncol)
    grid_y <- seq(current_shp@bbox['y', 'min'],
                    current_shp@bbox['y', 'max'], length.out=nrow)
  
    grid_df <- list(x=grid_x, y=grid_y)
    return(grid_df)
}

### Starting with grid_df a list, x=x-intercept vector, y=y-intercept vector
## generates x_min x_max y_min y_max x_center y_center word
get_grid_zoomin_bbox <- function(grid_df) {
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
                   

# download ZOOM IN map img from osm
get_osm_map <- function(current_bbox_df, tile_level = 5){
    
    n <- dim(current_bbox_df)[1]
    
    x_margin <- (current_bbox_df$x_max[n] - current_bbox_df$x_min[1]) * 0.025
    y_margin <- (current_bbox_df$y_max[n] - current_bbox_df$y_min[1]) * 0.025
    
    map <- openmap(upperLeft=c(lat = current_bbox_df$y_max[n] + y_margin, 
                               lon = current_bbox_df$x_min[1] - x_margin), 
                   lowerRight=c(lat = current_bbox_df$y_min[1] - y_margin, 
                                lon = current_bbox_df$x_max[n] + x_margin),
                   type="osm", minNumTiles = tile_level)
    map <- openproj(map) 
    return(map)
}                                     

facility_subset_griddf <- function(current_bbox_df, current_facilities_serial_added, title) {
    x_min <- current_bbox_df$x_min
    x_max <- current_bbox_df$x_max
    y_min <- current_bbox_df$y_min
    y_max <- current_bbox_df$y_max
    map_num <- current_bbox_df$word
    current_facilities_serial_added <- subset(current_facilities_serial_added, ( 
                                    long >= x_min & long <= x_max &
                                    lat >= y_min & lat <= y_max),
                                    select = c("serial_ID", "facility_name",
                                               "community", "ward", "facility_type",
                                               "facility_ID"))
    current_facilities_serial_added$map <- rep(map_num, nrow(current_facilities_serial_added))
    current_facilities_serial_added <- current_facilities_serial_added[,c("map", "serial_ID",
                                                                          "facility_name", "ward", 
                                                                          "community", "facility_type",
                                                                          "facility_ID")]
    current_facilities_serial_added <- arrange(current_facilities_serial_added, serial_ID)
    current_facilities_serial_added <- rename(current_facilities_serial_added, 
                                              replace=c("map" = "MAP",
                                                        "serial_ID" = "ID#",
                                                        "facility_name" = "NAME",
                                                        "ward" = "WARD",
                                                        "community" = "COMMUNITY",
                                                        "facility_type" = "TYPE",
                                                        "facility_ID" = "FACILITY_ID"))
    if (nrow(current_facilities_serial_added) > 0) {
        break_data_grid_print(current_facilities_serial_added, title)
    }
}

#### turn to-go-list into functions
grid_table_assemble <- function(df, title_name){
    table <- tableGrob(df, show.rownames = FALSE, row.just = "left",
                       col.just = "left", core.just="left", 
                       gpar.corefill = gpar(fill = "white", col = "grey"),
                       gpar.coretext  = gpar(fontsize = 10, col="black"))
    grid.newpage()
    h <- grobHeight(table)
    w <- grobWidth(table)
    title <- textGrob(title_name, y=unit(0.5,"npc") + 0.5*h, 
                      vjust=0, gp=gpar(fontsize=15, fontface = "bold"))
    gt <- gTree(children=gList(table, title))
    grid.draw(gt)
}

break_data_grid_print <- function(df, title_name, page_limit = 25) {
    df$splitter <- cut_interval(1:nrow(df), length=page_limit)
    d_ply(df, .(splitter), function(df) {
        grid_table_assemble(subset(df,select=-c(splitter)), title_name)
    })
}

# Plotting lga level
getting_lga_graph <- function(current_shp_fortify, current_facilities, bbox_data, grid_lines, title) {
    osm_map <- get_osm_map(bbox_data)
        
    plot <- autoplot(osm_map, expand=F) + 
        geom_point(data=current_facilities, 
                   aes(x=long, y=lat), 
                   color=I('red'), size=2, alpha=0.6) + 
        geom_polygon(data=current_shp_fortify, 
                     aes(x=long, y=lat, group=group), 
                     fill='black', color='black', alpha=0.05) + 
        geom_vline(xintercept = grid_lines$x, linetype="dotted", size=1) + 
        geom_hline(yintercept = grid_lines$y, linetype="dotted", size=1) +
        geom_text(data=bbox_data, 
                  aes(x=x_center, y=y_center, label=word),
                  size=11, color='blue', alpha=0.4) + 
        coord_equal() + 
        labs(title=title) +
        theme(panel.grid=element_blank(),
              panel.background = element_blank()) + 
        xlab("Longitude") + ylab("Latitude")
    print(plot)
}



# Plotting area zoom in level
getting_zoomin_graph <- function(current_bbox_df, current_shp_fortify, 
                                 current_facilities, grid_lines) {
    osm_map <- get_osm_map(current_bbox_df)
    x_margin <- (current_bbox_df$x_max - current_bbox_df$x_min)*0.025
    y_margin <- (current_bbox_df$y_max - current_bbox_df$y_min)*0.025
    
    text_df <- ddply(current_facilities, .(serial_ID),
                     summarize, clong=mean(long), clat=mean(lat) )
    
    plot <- autoplot(osm_map, expand=F) + 
        geom_polygon(data=current_shp_fortify, 
                     aes(x=long, y=lat, group=group), 
                     fill='black', color='black', alpha=0.05) + 
        geom_point(data=current_facilities, 
                   aes(x=long, y=lat), 
                   color=I('red'), size=3, alpha=0.6) + 
        geom_vline(xintercept = grid_lines$x, linetype="dotted", size=1) + 
        geom_hline(yintercept = grid_lines$y, linetype="dotted", size=1) +
        geom_text(data=text_df, 
                  aes(x=clong, y=clat + .001, label=serial_ID),
                  # + .001 is a text offset for clarity
                  color='black', size=3, vjust=0) + 
        geom_text(data=current_bbox_df, 
                  aes(x=x_center, y=y_center, label=word),
                  size=11, color='blue', alpha=0.3) + 
        theme(panel.grid=element_blank(),
              panel.background = element_blank()) +
        coord_fixed(ratio=1, xlim=c(current_bbox_df$x_min - x_margin, 
                                    current_bbox_df$x_max + x_margin),
                            ylim=c(current_bbox_df$y_min - y_margin, 
                                    current_bbox_df$y_max + y_margin)) +   
        labs(title = paste('Map of Area', 
                           current_bbox_df$word, sep=' ')) +
        xlab("Longitude") + ylab("Latitude")
    print(plot)
}

### helper function to create proper serial_ID for solving near points issue
facility_get_serial_ID <- function(fac, DIST_THRESHOLD=0.0012) {
    fac$serial_ID <- 1:nrow(fac)
        
    #### FIND THE CLOSEST FACILITY
    nn <- nn2(fac[c("lat", "long")], k=2)
    fac$serial_ID.y <- nn$nn.idx[,2] # first neighbor is self
    fac$dist <- nn$nn.dist[,2]

    #### COMBINE IDS for CLOSE FACILITIES
    idx <- which(fac$dist < DIST_THRESHOLD)
    for (i in idx){
        id <- which(fac$serial_ID == fac$serial_ID.y[i])
            
        fac$serial_ID[id] <- fac$serial_ID[i]
        fac$serial_ID.y[idx][which(fac$serial_ID.y[idx] == fac$serial_ID.y[i])] <- fac$serial_ID[i]
    }
    fac[idx,'serial_ID'] <- paste0(fac[idx,'serial_ID'], "*")
    
    # calling internal function to get result
    fac <- subset(fac, select= -c(serial_ID.y))
    
    return(fac)
}

# Creating master function to plot lga overview + zoomin level all at once
lga_viz <- function(lga_data) {
    grid_lines <- get_grids(lga_data$shp)
    bbox_data <- get_grid_zoomin_bbox(grid_lines)
    lga_name <- lga_data$name
    
    to_be_surveyed <- function(missing, sector, num) {
        if (nrow(missing) > 0){        
            break_data_grid_print(missing,
                                  sprintf("%s. %s Facilities that need to be surveyed - %s", 
                                          num, sector, lga_name))
        }    
    }
    
    zoomed_in <- function(nmisdata, sector) {
        current_facilities <- facility_get_serial_ID(nmisdata, .01)
        d_ply(bbox_data, .(word), function(df) {
            # print zoomin small map
            getting_zoomin_graph(df, lga_data$shp_fortified,
                                 current_facilities, grid_lines)
            # print NOT TO GO list of the small map
            facility_subset_griddf(df, current_facilities,
                                   sprintf("Surveyed %s Facilities in %s - Area %s", 
                                           sector, lga_name, df[1,'word']))
        })    
    }
    
    # PAGE 1: Education facilities that need to be surveyed
    to_be_surveyed(lga_data$missing_edu, "Education", 'A')
    
    # PAGE 2: Health facilities that need to be surveyed
    to_be_surveyed(lga_data$missing_health, "Health", 'B')
    
    # PAGE 3: OVERVIEW Education
    getting_lga_graph(lga_data$shp_fortified, lga_data$nmis_edu, bbox_data, grid_lines,
                      sprintf("C. Surveyed Education Facilities - %s", lga_name))
    
    # ZOOMED IN MAPS AND TABLES -- EDUCATION
    zoomed_in(lga_data$nmis_edu, "Education")
    
    # PAGE 4: OVERVIEW Health
    # print lga level map of current lga
    getting_lga_graph(lga_data$shp_fortified, lga_data$nmis_health, bbox_data, grid_lines,
                      sprintf("D. Surveyed Health Facilities - %s", lga_name))
    
    # ZOOMED IN MAPS AND TABLES -- HEALTH
    zoomed_in(lga_data$nmis_health, "Health")
}

to_pdf <- function(LGA_ID) {
    lga_data <- get_data_for_current_lga(LGA_ID)
    pdf(paste0('pdfs/', lga_data$name, '.pdf'), width = 11, height = 8.5)
    lga_viz(lga_data)
    dev.off()    
}

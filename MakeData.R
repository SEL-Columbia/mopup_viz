require(maptools)
require(ggplot2)
require(plyr)
require(stringr)
require(formhub)

# Creating data folder if it doesn't exist
DATA_DIR <- "data"
BASE_DIR<- getwd()
if (!file.exists(file.path(BASE_DIR, DATA_DIR))){
    
    dir.create(file.path(BASE_DIR, DATA_DIR))
}

# load data
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
nga_shp <- readShapeSpatial("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/raw_data/nga_lgas/nga_lgas_with_corrected_id.shp", proj4string=wgs84)

# :( fortify sucks )
nga_shp_fortify <- fortify(nga_shp, region=="Name")
nga_shp_fortify$lga_id <- nga_shp@data[nga_shp_fortify$id,'lga_id']
saveRDS(nga_shp, "data/NGALGAS_shp.rds")
saveRDS(nga_shp_fortify, "data/NGALGAS_fortified.rds")



#### Helper function for truncate excessive long strings in columns
length_fix <- function(df){
    df$NAME <- ifelse( str_length(df$NAME) <= 50,
                       df$NAME,
                       str_sub(df$NAME, 1, 50))
    
    df$WARD <- ifelse( str_length(df$WARD) <= 20,
                       df$WARD,
                       str_sub(df$WARD, 1, 20))
    
    df$COMMUNITY <- ifelse( str_length(df$COMMUNITY) <= 20,
                            df$COMMUNITY,
                            substr(df$COMMUNITY, 1, 20))
    return(df)    
}

#### REVALUE
edu_type_revalue <- c("adult_ed"="Adult", "adult_lit"="Adult", 
                      "adult_vocational"="Adult", "js"="JS", "js_ss"="JS + SS", 
                      "junior_and_senior_sec"="JS + SS", "primary_js_ss" = "Primary + JS + SS",
                      "junior_sec_only" = "JS Only", "juniors_sec_only" = "JS Only", 
                      "preprimary_and_primary"="Preprim + Primary", "preprimary_only"="Preprimary",
                      "preprimary_primary"="Preprim + Primary", "primary" = "Primary Only",
                      "primary_and_junior_sec"="Primary + JS", "primary_js" = "Primary + JS",
                      "primary_junior_and_senior_sec" = "Primary + JS + SS", "preprimary" = "Preprimary", 
                      "primary_only" = "Primary Only", "science_technical" = "Other", 
                      "senior_sec_only" = "SS Only", "ss" = "SS Only", "vocational" = "Voc",
                      "vocational_post_primary" = "Voc", "vocational_post_secondary" = "Voc")



# MISSING FACILITY LISTS
missing_edu <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result/facility_missing_list_edu.csv", stringsAsFactors=FALSE)
missing_health <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result/facility_missing_list_health.csv", stringsAsFactors=FALSE)
missing_edu$facility_type <- revalue(missing_edu$facility_type, edu_type_revalue)
renamelist_missing <- c("short_id" = "UID", "facility_name" = "NAME",
                        "facility_type" = "TYPE",
                        "ward" = "WARD", "community" = "COMMUNITY")
missing_edu <- subset(missing_edu,
                      select=c(names(renamelist_missing), "lga_id"))
missing_health <- subset(missing_health,
                      select=c(names(renamelist_missing), "lga_id"))
missing_edu <- rename(missing_edu, renamelist_missing)
missing_health <- rename(missing_health, renamelist_missing)

missing_edu$UID <- toupper(missing_edu$UID)
missing_health$UID <- toupper(missing_health$UID)
missing_edu <- length_fix(missing_edu)
missing_health <- length_fix(missing_health)

missing_edu$UID <- substr(missing_edu$UID, start=3,7)
missing_health$UID <- substr(missing_health$UID, start=3,7)

### Load finished mopup facility and remove those from missing list
edu_finished <- formhubDownload(formName="mopup_questionnaire_education_final",
                                uname="ossap", pass="777lgas", 
                                na.strings=c(999,"n/a","dn"), keepGroupNames=FALSE)
edu_finished <- unique(edu_finished$facility_ID)
edu_finished <- toupper(edu_finished)

health_finished <- formhubDownload(formName="mopup_questionnaire_health_final",
                                uname="ossap", pass="777lgas", 
                                na.strings=c(999,"n/a","dn"), keepGroupNames=FALSE)
health_finished <- unique(health_finished$facility_ID)
health_finished <- toupper(health_finished)

# remove surveyed facility 
missing_edu <- subset(missing_edu, ! missing_edu$UID %in% edu_finished)
missing_health <- subset(missing_health, ! missing_health$UID %in% health_finished)

# adding date of survey column to missing list
missing_edu$Date_of_Survey <- "    "
missing_health$Date_of_Survey <- "    "

saveRDS(missing_edu, "data/MissingEducationFacilities.rds")
saveRDS(missing_health, "data/MissingHealthFacilities.rds")


#Given a column that contains space-separated gps values, returns a data frame with lats and longs
gps_explode <- function(gpscol) {
    gps_exploded <- ldply(str_extract_all(gpscol, "[0-9.]+"), function(x) {
        if(length(x) == 0) {c(NA,NA,NA,NA)} else { x }
    })
    data.frame("lat"=as.numeric(gps_exploded$V1), "long"=as.numeric(gps_exploded$V2))
}

# LISTS OF NMIS FACILITIES
renamelist_nmis <- c("facility_ID" = "UID", "facility_name" = "NAME",
                        "facility_type" = "TYPE",
                        "ward" = "WARD", "community" = "COMMUNITY")

nmis_edu <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/final_output/Education_774_NMIS_Facility.csv",
                     stringsAsFactors=FALSE)
nmis_edu <- cbind(nmis_edu, gps_explode(nmis_edu$gps))
nmis_edu <- subset(nmis_edu, select=c(names(renamelist_nmis), c("lga_id", "lat", "long")))
nmis_edu <- rename(nmis_edu, renamelist_nmis)
nmis_edu$TYPE <- revalue(nmis_edu$TYPE, edu_type_revalue)
nmis_edu <- length_fix(nmis_edu)
saveRDS(nmis_edu, "data/NMISEducationFacilities.rds")

nmis_health <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/data_774/final_output/Health_774_NMIS_Facility.csv",
                        stringsAsFactors=FALSE)
nmis_health <- cbind(nmis_health, gps_explode(nmis_health$gps))
nmis_health <- subset(nmis_health, select=c(names(renamelist_nmis), c("lga_id", "lat", "long")))
nmis_health <- rename(nmis_health, renamelist_nmis)
nmis_health <- length_fix(nmis_health)
saveRDS(nmis_health, "data/NMISHealthFacilities.rds")

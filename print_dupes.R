missing_edu <- colwise(toupper)(missing_edu)
missing_health <- colwise(toupper)(missing_health)

dup_e <- missing_edu[which(duplicated(missing_edu[,
                                                  c("NAME", "TYPE", "WARD", "COMMUNITY", "lga_id")]) | 
                               duplicated(missing_edu[,
                                                      c("NAME", "TYPE", "WARD", "COMMUNITY", "lga_id")], fromLast=T)),]


dup_h <- missing_health[which(duplicated(missing_health[,
                                                        c("NAME", "TYPE", "WARD", "COMMUNITY", "lga_id")]) | 
                                  duplicated(missing_health[,
                                                            c("NAME", "TYPE", "WARD", "COMMUNITY", "lga_id")], fromLast=T)),]

dup_e$sector <- "Education"
dup_h$sector <- "Health"

dup_total <- rbind.fill(dup_e, dup_h)
lga <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/lgas.csv", stringsAsFactors=F)
lga <- subset(lga, select=c("lga_id", "lga", "state", "zone"))

final <- merge(dup_total, lga, by="lga_id")
final <- arrange(final, lga_id, sector, NAME, TYPE, WARD, COMMUNITY)

BASEDIR <- "~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_viz_output/"
d_ply(final, .(zone), function(df){
    name <- gsub("-", "", df$zone[1])
    name <- paste(name, ".csv", sep="")
    final_dir <- paste(BASE_DIR, name, sep="/")
    print(final_dir)
    
    df$lga_id <- NULL
    write.csv(df, final_dir, row.names=F)
})

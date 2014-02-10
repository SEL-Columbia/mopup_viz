required_packages <- c("ggplot2", "plyr", "stringr", "OpenStreetMap", "gridExtra", "RANN", 
                       "maptools")
installed_pkg <- installed.packages()
for (pkg in required_packages){
    
    if (! pkg %in% installed_pkg){
        install.packages(pkg, dependencies=TRUE)
    }
    
}
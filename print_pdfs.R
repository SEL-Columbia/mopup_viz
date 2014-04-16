source("MakeData.R")
source("./facility_viz.R")
source("check_n_install_pkg.R")

yo <- read.csv("./data/TA_pilot_LGA_assignments.csv")
yo <- yo[yo$zone != "South-South",]
remix <- yo$lga_id

for (lga_id in remix){
    to_pdf(lga_id)
    print(lga_id)
}


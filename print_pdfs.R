source("MakeData.R")
source("./facility_viz.R")

for (lga_id in 1:774){
    to_pdf(lga_id)
}


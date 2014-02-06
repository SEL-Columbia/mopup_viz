source("MakeData.R")
source("./facility_viz.R")

for (lga_id in 668:774){
    to_pdf(lga_id)
    print(lga_id)
}


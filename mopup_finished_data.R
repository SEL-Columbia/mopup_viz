edu_mopup <- formhubRead("../backcheck/data/mopup_questionnaire_education_final_2014_03_10_16_05_39.csv",
                         #change above csv file when refreshing data!
                         "../backcheck/data/jsons/mopup_questionnaire_education_final.json",
                         keepGroupNames=FALSE)

health_mopup <- formhubRead("../backcheck/data/mopup_questionnaire_health_final_2014_03_10_16_06_09.csv",
                            #change above csv file when refreshing data!
                            "../backcheck/data/jsons/mopup_questionnaire_health_final.json",
                            keepGroupNames=FALSE)


edu_mopup <- edu_mopup[edu_mopup$lga != "NA",]

health_mopup <- health_mopup[health_mopup$lga != "NA",]


#cleaning facilityIDs
edu_mopup$facility_ID <- tolower(edu_mopup$facility_ID)   

health_mopup$facility_ID <- tolower(health_mopup$facility_ID)   

#detecting messed up facilityIDs  
edu_mopup <- subset(edu_mopup, edu_mopup$facility_ID !=  "xxxx" & 
                                 !str_detect(edu_mopup$facility_ID, "[0-9]")) 

health_mopup <- subset(health_mopup, health_mopup$facility_ID !=  "xxxx" &
                                    !str_detect(health_mopup$facility_ID, "[0-9]")) 

saveRDS(edu_mopup, "data/edu_finished_mop.RDS")
saveRDS(health_mopup, "data/health_finished_mop.RDS")

source("R/convert_data.R")
source("R/calb_aa3.R")
SciViews::R
library(chart)
library(econum)



EcoNumData_aa3 <- conversion2_aa3(file_aa3_txt = "Data/raw/181011A.TXT",
                                  file_aa3_xlsx = "Data/raw/181011A.xlsx", 
                                  project = "calibration",remote = FALSE, 
                                  local = TRUE,
                                  local_repos = "~/Documents/protocol_seal_aa3/Data", 
                                  remote_repos = NA) 

calb_aa3(EcoNumData_aa3)


# repos_save

write_rds(EcoNumData_aa3, path = "nutri.rds" )

uncoupled_data_aa3(nutrient = EcoNumData_aa3, remote_repos = NA, local_repos = "~/Documents/protocol_seal_aa3/Data", remote = FALSE, local = TRUE)



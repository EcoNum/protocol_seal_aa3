source("R/convert_data.R")
SciViews::R
library(chart)
library(econum)



conversion_aa3(file_aa3_txt = "Data/raw/181009A.txt",file_aa3_xlsx = "Data/raw/181009A.xlsx", project = "calibration",remote = FALSE, 
               local = TRUE,local_repos = "~/Documents/protocol_seal_aa3/Data", remote_repos = NA)

write_rds(EcoNumData_aa3, path = "nutri.rds" )

uncoupled_data_aa3(nutrient = EcoNumData_aa3, remote_repos = NA, local_repos = "~/Documents/protocol_seal_aa3/Data", remote = FALSE, local = TRUE)



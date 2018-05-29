
source("R/convert_data.R")

conversion_aa3(file_aa3_txt = "Data/raw/180528A.txt",file_aa3_xlsx = "Data/raw/180528A.xlsx", project = "cs_calibration",remote = FALSE, local = TRUE,local_repos = "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data", remote_repos = NA)

uncoupled_data_aa3(nutrient = EcoNumData_aa3, remote_repos = NA, local_repos = "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data", remote = FALSE, local = TRUE)




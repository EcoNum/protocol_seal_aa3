source("R/convert.R")
source("R/calb_aa3.R")
source("R/calc_newlm.R")
source("R/merge_sampdb.R")
SciViews::R
library(chart)
library(econum)

# INORG DATA 
EcoNumData_aa3 <- convert_aa3(file_aa3_txt = "Data/raw/181009A.TXT",
                                  file_aa3_xlsx = "Data/raw/181009A.xlsx", 
                                  project = "calibration",remote = FALSE, 
                                  local = TRUE,
                                  local_repos = "~/Documents/protocol_seal_aa3/Data", 
                                  remote_repos = NA) 
EcoNumData_aa3 -> inorga

calb_aa3(inorga) -> calb_inorga

calc_newlm(EcoNumData = calb_inorga, 
           EcoNumData_aa3 = inorga, 
           filter_list = list(NH4 = c(0.1,0.5))) -> calb_inorga2

# ORG DATA
EcoNumData_aa3 <- convert_aa3(file_aa3_txt = "Data/raw/181011A.TXT",
                                  file_aa3_xlsx = "Data/raw/181011A.xlsx", 
                                  project = "calibration",remote = FALSE, 
                                  local = TRUE,
                                  local_repos = "~/Documents/protocol_seal_aa3/Data", 
                                  remote_repos = NA) 
EcoNumData_aa3 -> orga

calb_aa3(orga) -> calb_orga

calc_newlm(EcoNumData = calb_orga, 
           EcoNumData_aa3 = orga, 
           filter_list = list(Ptot = c(10))) -> calb_orga2

# calb_orga %>.% 
#   filter(., sample_type == "SAMP") -> samp_org
# inorga %>.%
#   filter(., sample_type == "SAMP") -> samp_inorg

# Exemple of nutrients selections 
nutrient = c("Ptot_conc", "Ptot_conc_new", "Ntot_conc", "PO4_conc", "NOx_conc", "NO2_conc", 
           "NO3_conc", "NH4_conc", "NH4_conc_new")

merge_sampdb(sampdb_org = calb_orga2$samp, sampdb_inorg = calb_inorga2$samp, nutrient)

# repos_save

write_rds(EcoNumData_aa3, path = "nutri.rds" )

uncoupled_data_aa3(nutrient = EcoNumData_aa3, remote_repos = NA, local_repos = "~/Documents/protocol_seal_aa3/Data", remote = FALSE, local = TRUE)

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

test <- convert_aa3_2(file_aa3_txt = "Data/raw/181009A.TXT",
                              file_aa3_xlsx = "Data/raw/181009A.xlsx") 

EcoNumData_aa3 -> inorga

calb_aa3(as.data.frame(test)) -> calb_inorga

calc_newlm(EcoNumData = calb_inorga, 
           filter_list = list(PO4 = c(0.1,0.5))) -> samp_inorg

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
           filter_list = list(Ptot = c(10))) -> samp_org

samp_org$sampdb -> sampdb_org
samp_inorg$sampdb -> sampdb_inorg

# calb_orga %>.% 
#   filter(., sample_type == "SAMP") -> samp_org
# inorga %>.%
#   filter(., sample_type == "SAMP") -> samp_inorg

# Exemple of nutrients selections 
merge_sampdb(sampdb_org, sampdb_inorg)

# repos_save

write_rds(EcoNumData_aa3, path = "nutri.rds" )

uncoupled_data_aa3(nutrient = EcoNumData_aa3, remote_repos = NA, local_repos = "~/Documents/protocol_seal_aa3/Data", remote = FALSE, local = TRUE)

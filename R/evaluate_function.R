

# load package ####
library(econum)

# load function ####
source("R/function_analyse_seal_data.R")
source(file = "R/autoplot.R")


# Import function and convert txt file into EcoNumData ####
# First define where are located the local and remote EcoNumData repositories on this computer
set_opt_econum("local_repos", "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data")
set_opt_econum("remote_repos", "/Volumes/Public/EcoNumData")
# Use import_aa3() to convert txt file into AA3 EcoNumData 
# and repos_save() to save the AA3 EcoNumData object.


repos_save(import_aa3(file = "Data/171124AR1.TXT", project = "Nutrient_test"), remote = FALSE)
repos_save(import_aa3(file = "Data/171124B.TXT", project = "Nutrient_test"), remote = FALSE)
repos_save(import_aa3(file = "Data/171208D.TXT", project = "Nutrient_test"), remote = FALSE)
repos_save(import_aa3(file = "Data/171211B.TXT", project = "Nutrient_test"), remote = FALSE)
repos_save(import_aa3(file = "Data/171214A.TXT", project = "Nutrient_test"), remote = FALSE)
repos_save(import_aa3(file = "Data/171215A.TXT", project = "Nutrient_test"), remote = FALSE)
repos_save(import_aa3(file = "Data/180222A.txt", project = "Nutrient_test"), remote = FALSE)


# Data recovering from the repository ####

repos_load(file = "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data/Nutrient_test/AA3/171215A_2017-12-15_11.44.46_5A331080_AA3.RData")

# Calibration function ####
# Use file loaded before 

chart_aa3_calb(EcoNumData_AA3.a)

tab1 <-calb_linear_model(EcoNumData_AA3.a)

calb_aa3(EcoNumData_AA3.a)





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
repos_save(import_aa3(file = "Data/180313D.txt", project = "oceano2018"), remote = FALSE)

# Data recovering from the repository ####

repos_load(file = "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data/Nutrient_test/AA3/171215A_2017-12-15_11.44.46_5A331080_AA3.RData")

# Calibration function ####
# Use file loaded before 

chart_aa3_calb(EcoNumData_AA3.a)

tab1 <-calb_linear_model(EcoNumData_AA3.a)

calb_aa3(EcoNumData_AA3.a)


## Importation and conversion


SciViews::R
library(econum)
# the local repository
set_opt_econum("local_repos", "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data")
# Save in econumdata the calibration information.

source("R/function_analyse_seal_data.R")

file_aa3 <- "Data/180305B.txt" # or file_aa3 <- "Data/180305A.txt"
file_convert <- "Data/nu01_20180305.xlsx"

import_aa3_sample(file_aa3 = file_aa3 , file_convert = file_convert)


## Ecophysio & mesocosm-monitoring


source("R/function_analyse_seal_data.R")

library(econum)
# the local repository
set_opt_econum("local_repos", "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data")

econum::repos_save(object = convert_aa3(file_aa3_txt = "Data/180430AR1.txt" ,file_aa3_xlsx = "Data/180430AR1.xlsx", project = "mesocosm_monitoring"), remote = FALSE)

econum::repos_save(object = convert_aa3(file_aa3_txt = "Data/180503A.txt" ,file_aa3_xlsx = "Data/180503A.xlsx", project = "mesocosm2018"), remote = FALSE)

econum::repos_save(object = convert_aa3(file_aa3_txt = "Data/180427A.txt" ,file_aa3_xlsx = "Data/180424A_B.xlsx", project = "ecophysio2018"), remote = FALSE)

library(econum)
SciViews::R

repos_load(file = "Data/ecophysio2018/aa3/180424A-inorga_2018-04-24_14.35.20_5ADE7380_aa3.RData")
nutri <- EcoNumData_aa3.inorga

repos_load(file = "Data/ecophysio2018/aa3/180424B-inorga_2018-04-24_16.33.58_5ADE7380_aa3.RData")
nutri1 <- EcoNumData_aa3.inorga

nutri <- bind_rows(nutri, nutri1)
rm(EcoNumData_aa3.inorga, nutri1)

nutri <- filter(nutri, sample_type == "SAMP")
nutri <- filter(nutri, project != "NA")

nutri <- separate(nutri, col = sample, into = c("respiro", "type", "cycle"), sep = "-" )

nutri$sample_date <- as.POSIXct(nutri$sample_date)


chart(data = filter(nutri, NO3_conc <= 1), formula = NO3_conc ~ sample_date %col=% respiro) +
  geom_point() +
  geom_line()



chart(data = nutri, formula = PO4_conc ~ sample_date %col=% respiro) +
  geom_point() +
  geom_line()

chart(data = nutri, formula = NH4_conc ~ sample_date %col=% respiro) +
  geom_point() +
  geom_line(mapping = aes(col = respiro))



repos_load(file = "Data/ecophysio2018/aa3/180425A-inorga_2018-04-25_13.48.23_5ADFC500_aa3.RData")

nutri <- EcoNumData_aa3.inorga
nutri <- filter(nutri, sample_type == "SAMP")

chart(data = nutri, formula = NO3_conc ~ sample_date %col=% sample) +
  geom_point() +
  geom_line()

chart(data = nutri, formula = PO4_conc ~ sample_date %col=% sample) +
  geom_point() +
  geom_line()

chart(data = nutri, formula = NH4_conc ~ sample_date %col=% sample) +
  geom_point() +
  geom_line()


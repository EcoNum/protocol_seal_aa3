# Combine seal data obtain with uncoupled_data_aa3()

SciViews::R
library(econum)
# Version 2 avec des organiques et inorganiques 
# direction <- "Data/mesocosm_monitoring/aa3"
# direction <- "Data/coral_salinity002/aa3"
# create a list with all names 
all_data <- dir(direction, full.names = TRUE)
t <- grep(x = all_data, pattern = "inorga")
inorga <- all_data[t]
orga <- all_data[-t]
## Inorga
repos_load(inorga[1])
nutri <- EcoNumData_aa3
nutri$calib_inorga <- attr(nutri, "calibration")
# create a loop to combine all dataset
for(f in inorga[-1]){
  repos_load(f)
  nutri1 <- EcoNumData_aa3
  nutri1$calib_inorga <- attr(nutri1, "calibration")
  nutri <- dplyr::bind_rows(nutri, nutri1)
  remove(nutri1)
}
inorga <- nutri
remove(all_data, direction, t, f, EcoNumData_aa3, nutri)

## Orga
repos_load(orga[1])
nutri <- EcoNumData_aa3
nutri$calib_inorga <- attr(nutri, "calibration")
# create a loop to combine all dataset
for(f in orga[-1]){
  repos_load(f)
  nutri1 <- EcoNumData_aa3
  nutri1$calib_inorga <- attr(nutri1, "calibration")
  nutri <- dplyr::bind_rows(nutri, nutri1)
  remove(nutri1)
}
orga <- nutri
remove(EcoNumData_aa3, f, nutri)

nutri <- left_join(inorga, orga, by = c("sample_id", "sample_type", "project", "sample", "sample_date", "authors" ), suffix = c("_inorga", "_orga"))
nutri <- select(nutri, - PO4_std,  - NO3_std, - NH4_std, - Ptot_std, - Ntot_std, - NO2_std)
nutri <- separate(nutri, sample, c("position", "cycle", " our"))

rm(inorga, orga)

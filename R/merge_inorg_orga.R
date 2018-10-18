# Import data and filter  by project 
# merge inor and orga
library(seal.econum)

samp_inorga <- readRDS("data/samp_inorga.RDS")
samp_orga <- readRDS("data/samp_orga.RDS")

inorga <- dplyr::filter(samp_inorga, project == "mesocosm_monitoring")
orga <- dplyr::filter(samp_orga, project == "mesocosm_monitoring")


test <- merge_sampdb_aa3(sampdb_org = orga, sampdb_inorg = inorga)



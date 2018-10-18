# Import, Combine and Analyse of seal AA3 data

library(seal.econum)

aa3 <- convert_aa3(file_aa3_txt = "data/raw/181011A.TXT", file_aa3_xlsx = "data/raw/181011A.xlsx")

(calb <- calb_aa3(aa3))

(calb_new <- calb_correction_aa3(calb, filter_list = list(Ptot = c(10))))



## If inorga ----
calb_inorga <- readRDS("data/calibration_inorg.RDS")
samp_inorga <- readRDS("data/samp_inorg.RDS")

calb_inorga <- dplyr::bind_rows(calb_inorga, calb_new$calbdb) # calb$calbdb or calb_new$calbdb
samp_inorga <- dplyr::bind_rows(samp_inorga, calb_new$sampdb) # calb$calbdb or calb_new$calbdb
saveRDS(object = calb_inorga, file = "data/calibration_inorga.RDS")
saveRDS(object = samp_inorga, file = "data/samp_inorga.RDS")


##if orga -----
calb_orga <- readRDS("data/calibration_orga.RDS")
samp_orga <- readRDS("data/samp_orga.RDS")

calb_orga <- dplyr::bind_rows(calb_orga, calb_new$calbdb) # calb$calbdb or calb_new$calbdb
samp_orga <- dplyr::bind_rows(samp_orga, calb_new$sampdb) # calb$calbdb or calb_new$calbdb

saveRDS(object = calb_new$calbdb, file = "data/calibration_orga.RDS")
saveRDS(object = calb_new$sampdb, file = "data/samp_orga.RDS")

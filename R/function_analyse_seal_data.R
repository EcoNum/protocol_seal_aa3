


# function tentative #########
# I would like prepare a function or routine analyses  to replace test version 1, test version 2, test version 3.

# this function has for objectives to import txt file of seal AA3 
# and transform to prepare for function new_econumdata

library(econum)
library(readr)

import_aa3 <- function(file, project = basename(dirname(file))){
  
  # Extract the metadata and create a list that respect the list of EcoNumData
  header <- read_delim(file = file, 
                       delim = ";",
                       n_max = 13,
                       col_names = FALSE,
                       col_types = cols( .default = "c"),
                       locale = readr::locale(encoding = "LATIN1")) # particulary attention with the encoding system
  # Extract nutrients that are analysed
  results <- as.character(header[9, c(10, 13, 16)])
  
  # Prepare names of variables for raw_data
  stds <- paste(results, "std", sep = "_")
  concs <- paste(results, "conc", sep = "_")
  vals <- paste(results, "values", sep = "_")
 
  # Change in data  NPinorganique.ANL in inorganique 
  header$X2[header$X2 == "NPinorganique.ANL" ] <- "a"
  header$X2[header$X2 == "NPorganique.ANL" ] <- "b"
  
  # Method information
  # Method information
  
  channel_1 <- list(method  = as.character(header[9,10]), unit = as.character(header[10,10]), 
                    base = as.character(header[11,10]) , gain = as.character(header[12,10]), 
                    lamp = as.character(header[13,10]))
  
  channel_2 <- list(method  = as.character(header[9,13]), unit = as.character(header[10,13]) , 
                    base = as.character(header[11,13]) , gain = as.character(header[12,13]), 
                    lamp = as.character(header[13,10]))
  
  channel_3 <- list(method  = as.character(header[9,16]), unit = as.character(header[10,16]) , 
                    base = as.character(header[11,16]) , gain = as.character(header[12,16]), 
                    lamp = as.character(header[13,16]))
  
  method <- list(channel_1 = channel_1, channel_2 = channel_2, channel_3 = channel_3) 
  # extract metadata
  meta <- list(project = project, 
               sample = sub("\\.RUN$", "", as.character(header[2, 2])), 
               sample_date = as.POSIXlt(paste(as.character(header[3, 2]),
                 as.character(header[4, 2])),
                 format = "%d/%m/%Y %H:%M:%S"),
               author = as.character(header[5, 2]),
               date = as.POSIXct(as.character(header[3, 2]), format = "%d/%m/%Y"),
               comment = as.character(header[6, 2]),
               topic = as.character(header[1, 2]))
  
  # Extract raw data and remove a metadata 
  raw_data <- read_delim(file = file, delim = ";", skip = 14,
    col_names = c("sample_id", "peak_number", "cup_number", "sample_type", "cup_group", 
                  "x_1", "x_2", "date_time", stds[1], concs[1],
                  vals[1], stds[2], concs[2], vals[2], stds[3],
                  concs[3], vals[3], "x_3", "x_4"), 
    col_types = cols(sample_id = col_character(),
                     peak_number = col_integer(),
                     cup_number = col_skip(),
                     sample_type = col_character(),
                     cup_group = col_skip(),
                     x_1 = col_skip(),
                     x_2 = col_skip(),
                     date_time = col_datetime(format = "%d/%m/%Y %H:%M:%S"),
                     x_3 = col_skip(),
                     x_4 = col_skip(),
                     .default = col_number()))
  
  attr(raw_data, "spec") <- NULL
  attr(raw_data, "method") <- method
  
  new_econum_data(raw_data, metadata = meta, class = "AA3")
} 

# this is the routine analyses
# you must choose the file and the name of project

# test with 2 files one method A  "data/171215A.txt" and one method B "data/171214A.txt"
# First define where are located the local and remote EcoNumData repositories on this computer

library(econum)
set_opt_econum("local_repos", "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data")
set_opt_econum("remote_repos", "/Volumes/Public/EcoNumData")

repos_save(import_aa3(file = "Data/171124AR1.txt", project = "Nutrient_test"))

class(EcoNumData_AA3.a)
# Data recovering from the repository
repos_load(file = "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data/Nutrient_test/AA3/171215A_2017-12-15_11.44.46_5A331080_AA3.RData")


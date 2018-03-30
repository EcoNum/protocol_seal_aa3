SciViews::R
library(econum)

# First define where are located the local and remote EcoNumData repositories on this computer
set_opt_econum("local_repos", "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data")

#set_opt_econum("remote_repos", "/Volumes/Public/EcoNumData")

# First solution ####
# Import function and convert txt file into EcoNumData ####

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
  
  new_econum_data(raw_data, metadata = meta, class = "aa3")
} 


repos_save(import_aa3(file = "Data/180305A.txt", project = "optimisation"), remote = FALSE)

repos_load(file = "Data/optimisation/aa3/180313D_2018-03-13_13.41.32_5AA71480_aa3.RData")

# You can see several problems: 
## The name of the sample is the name of the batch. it is weakly informative
## You can use one name of project but in the batch you can have several batchs
## the sample_date is the date, you can specify the sample_date

## in conclusion, this first version is not the best version. For example, i search the data partaining to MesocosmA. It is impossible to find the data easily.

# Second solution ####

file_aa3 <- "Data/180305B.txt"
file_convert <- "Data/nu01_20180305.xlsx"

# Extract the metadata and create a list that respect the list of EcoNumData
header <- read_delim(file = file_aa3, 
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

# Extract raw data and remove a metadata 
raw_data <- read_delim(file = file_aa3, delim = ";", skip = 14,
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

intro_data <- readxl::read_excel(file_convert)
raw_data <- dplyr::left_join(raw_data, intro_data, by = "sample_id")

attr(raw_data, "spec") <- NULL
attr(raw_data, "method") <- method

raw_data %>.%
  filter(., sample_type == "SAMP") -> samp

for( i in 1:nrow(samp)){
  x <- samp[i, ]
  attr(x, "method") <- method
  
  p <- x[1, 14]
  p <- as.character(p$projet)
  
  s <- x[1, 15]
  s <- as.character(s$sample)
  
  sd <- x[1, 16]
  sd <- as.character(as.POSIXct(sd$sample_Date))
  
  d <- x[1, 4]
  d <- as.character(as.POSIXct(d$date_time))
  
  a <- x[1, 17]
  a <- as.character(a$authors)
  
  meta <- list(project = p, sample = s, sample_date = as.POSIXct(sd),
               author =a, date = as.POSIXct(d),comment = as.character(header[2,2]), topic = as.character(header[1,2]))
  
  test <- new_econum_data(x, metadata = meta, class = "aa3")
  repos_save(object = test ,remote = FALSE)
}

## the name of the file is more informative
## But
## You lose all information in calibration. I have two solutions duplicate the calibrations informations in all file or create a file that contains calibrations informations in each project 

rm(list = ls())

econum::repos_load(file = "Data/MesoA/aa3/A0_2018-03-02_12.00.00_5A9D8AAB_aa3.RData")

# Third solution #####
#I combine the first and second solution.
#the first solution give all data in one dataset. It is important for the calibration information,...
#The second solution give a dataset more informative. 

# Select the data

file_aa3 <- "Data/180305B.txt" # or file_aa3 <- "Data/180305A.txt"
file_convert <- "Data/nu01_20180305.xlsx"

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
  
  new_econum_data(raw_data, metadata = meta, class = "aa3")
} 

# the local repository
set_opt_econum("local_repos", "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data")
# Save in econumdata the calibration information.
repos_save(import_aa3(file = file_aa3, project = "calibration"), remote = FALSE)

# Save the data without calibration point

# Extract the metadata and create a list that respect the list of EcoNumData
header <- read_delim(file = file_aa3, 
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

# Extract raw data and remove a metadata 
raw_data <- read_delim(file = file_aa3, delim = ";", skip = 14,
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

intro_data <- readxl::read_excel(file_convert)
raw_data <- dplyr::left_join(raw_data, intro_data, by = "sample_id")

attr(raw_data, "spec") <- NULL
attr(raw_data, "method") <- method

raw_data %>.%
  filter(., sample_type == "SAMP") -> samp

for( i in 1:nrow(samp)){
  x <- samp[i, ]
  attr(x, "method") <- method
  
  p <- x[1, 14]
  p <- as.character(p$projet)
  
  s <- x[1, 15]
  s <- as.character(s$sample)
  
  sd <- x[1, 16]
  sd <- as.character(as.POSIXct(sd$sample_Date))
  
  d <- x[1, 4]
  d <- as.character(as.POSIXct(d$date_time))
  
  a <- x[1, 17]
  a <- as.character(a$authors)
  
  meta <- list(project = p, sample = s, sample_date = as.POSIXct(sd),
               author =a, date = as.POSIXct(d),comment = as.character(header[2,2]), topic = as.character(header[1,2]))
  
  test <- new_econum_data(x, metadata = meta, class = "aa3")
  repos_save(object = test ,remote = FALSE)
}

rm(list = ls())


import_aa3_sample <- function(file_aa3, file_convert){
  
  # Extract the metadata and create a list that respect the list of EcoNumData
  header <- read_delim(file = file_aa3, 
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
  
  # Extract raw data and remove a metadata 
  raw_data <- read_delim(file = file_aa3, delim = ";", skip = 14,
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
  
  intro_data <- readxl::read_excel(file_convert)
  raw_data <- dplyr::left_join(raw_data, intro_data, by = "sample_id")
  
  attr(raw_data, "spec") <- NULL
  attr(raw_data, "method") <- method
  
  raw_data %>.%
    filter(., sample_type == "SAMP") -> samp
  
  for( i in 1:nrow(samp)){
    x <- samp[i, ]
    attr(x, "method") <- method
    
    p <- x[1, 14]
    p <- as.character(p$projet)
    
    s <- x[1, 15]
    s <- as.character(s$sample)
    
    sd <- x[1, 16]
    sd <- as.character(as.POSIXct(sd$sample_Date))
    
    d <- x[1, 4]
    d <- as.character(as.POSIXct(d$date_time))
    
    a <- x[1, 17]
    a <- as.character(a$authors)
    
    meta <- list(project = p, sample = s, 
                 sample_date = as.POSIXct(sd), author =a, 
                 date = as.POSIXct(d),comment = as.character(header[2,2]), 
                 topic = as.character(header[1,2]))
    test <- new_econum_data(x, metadata = meta, class = "aa3")
    repos_save(object = test ,remote = FALSE)
  }
}
import_aa3_sample(file_aa3 = file_aa3, file_convert = file_convert)

# For analysis the dataset
# First i load de sample data
repos_load(file = "Data/MesoA/aa3/A0_2018-03-02_12.00.00_5A9D8AAB_aa3.RData")
sample <- EcoNumData_aa3.a
# In metadata, you can obtain in comment the name of the batch.
# I calibration folder, you can search the batch 
# In the example, the batch is "180305B"
# 
repos_load(file = "Data/calibration/aa3/180305B_2018-03-05_17.18.23_5A9C8880_aa3.RData")

source(file = "R/autoplot.R")

calb_aa3(EcoNumData_aa3.a)

# Functions's scritp preparation 
# The function 's objective is the transform a text data in object that 
# extract the method : A or B and to apply the good protocol
# extract the point of calibration, prepare graph with calibration point , extract the coefficient of  calibration lineR^2.
# extract sample point


# test version 1 ####
# You give only the txt file produced by seal-AA3
file <- "data/171215A.txt"
import_aa3_test <-function(file){
  
  #Import of metadata
  x <- readr::read_delim(file = file, 
                         delim = ";",
                         n_max = 13,
                         col_names = FALSE,
                         col_types = cols( .default = "c"),
                         locale = readr::locale(encoding = "LATIN1"))
  
    # Create list of important information
  date <- as.character
  
  metadata <- list(method= as.character(x[1,2]), 
                   run = as.character(x[2,2]), 
                   author = as.character(x[5,2]),
                   comment = as.character(x[6,2]),
                   date = as.character(x[3,2]),
                   method_information = data.frame(x = x[c(8:13),c(10,13,16)],
                                                      row.names = c("channel", 
                                                                    "method",
                                                                    "unity",
                                                                    "base_line",
                                                                    "gain",
                                                                    "lamp")))
  # Import raw_data and skip metadata
  raw_data <- readr::read_delim(file = file,
                      delim = ";",
                      skip = 14,
                      col_names = c("sample_id", 
                                    "peak_number",
                                    "cup_number",
                                    "sample_type",
                                    "cup_group", 
                                    "x_1", 
                                    "x_2",
                                    "date_time",
                                    "po4_theo",
                                    "po4_conc",
                                    "po4_val",
                                    "no3_theo",
                                    "no3_conc",
                                    "no3_val",
                                    "nh4_theo",
                                    "nh4_conc",
                                    "nh4_val",
                                    "x_3",
                                    "x_4"), 
                      col_types = cols(sample_id = col_character(),
                                       peak_number = col_skip(),
                                       cup_number = col_skip(),
                                       sample_type = col_character(),
                                       cup_group = col_skip(),
                                       x_1 = col_skip(),
                                       x_2 = col_skip(),
                                       date_time = col_datetime(format = "%d/%m/%Y %H:%M:%S"),
                                       po4_theo = col_number(),
                                       po4_conc = col_number(),
                                       po4_val = col_number(),
                                       no3_theo = col_number(),
                                       no3_conc = col_number(),
                                       no3_val = col_number(),
                                       nh4_theo = col_number(),
                                       nh4_val = col_number(),
                                       x_3 = col_skip(),
                                       x_4 = col_skip()))
  
  data <- raw_data[ raw_data$sample_type == "SAMP", ]
  list(data = data, metadata = metadata)
}

# check function

test <- import_aa3_test(file = "data/171215A.txt")

# it's OK 
# You have a function that give a list with data and metadata
test$metadata

test$data

# This function is OK for method A but must be change for method B 






# test version 2 #### 
# Create a object usable with EcoNumData that is an object containing data and metadata. 
# it create a file-base LIMS (Laboratory Information Management System)


# Script only, it is not a function 

# I give the position of text file, the name of project and the topic 
file  <- "data/171215A.txt"
project <- "Nutrient_test"
topic <- "aa3_a"


  # Extract the metadata and create a list that respect the list of EcoNumData
x <- readr::read_delim(file = file, 
                       delim = ";",
                       n_max = 13,
                       col_names = FALSE,
                       col_types = cols( .default = "c"),
                       locale = locale(encoding = "LATIN1"))


meta <- list(project = project, 
             sample = as.character(x[2,2]), 
             sample_date = as.POSIXlt(x = paste(as.character(x[3,2]),as.character(x[4,2]), sep =" "),
                                      format = "%d/%m/%Y %H:%M:%S"),
             author = as.character(x[5,2]),
             date = as.character(x[3,2]),
             comment = as.character(x[6,2]), 
             topic = topic)



  # Extract raw data and remove a metadata 
raw_data <- readr::read_delim(file = file,
                              delim = ";",
                              skip = 14,
                              col_names = c("sample_id", 
                                            "peak_number",
                                            "cup_number",
                                            "sample_type",
                                            "cup_group", 
                                            "x_1", 
                                            "x_2",
                                            "date_time",
                                            "po4_theo",
                                            "po4_conc",
                                            "po4_val",
                                            "no3_theo",
                                            "no3_conc",
                                            "no3_val",
                                            "nh4_theo",
                                            "nh4_conc",
                                            "nh4_val",
                                            "x_3",
                                            "x_4"), 
                              col_types = cols(sample_id = col_character(),
                                               peak_number = col_skip(),
                                               cup_number = col_skip(),
                                               sample_type = col_character(),
                                               cup_group = col_skip(),
                                               x_1 = col_skip(),
                                               x_2 = col_skip(),
                                               date_time = col_datetime(format = "%d/%m/%Y %H:%M:%S"),
                                               po4_theo = col_number(),
                                               po4_conc = col_number(),
                                               po4_val = col_number(),
                                               no3_theo = col_number(),
                                               no3_conc = col_number(),
                                               no3_val = col_number(),
                                               nh4_theo = col_number(),
                                               nh4_val = col_number(),
                                               x_3 = col_skip(),
                                               x_4 = col_skip()))

# Now, i have a list of metadata and raw_data 

# the problems is that the importation of data and metadata is only applicable on method A 

test1 <- new_econum_data(x = raw_data, metadata = meta)



# test version 3 #### 
# Create a object usable with EcoNumData that is an object containing data and metadata. 
# it create a file-base LIMS (Laboratory Information Management System)

# I would like a scripts that applicable for method A and B 


file  <- "data/171214A.txt" # or check with method B "data/171214A.txt"
project <- "Nutrient_test" # I must be the project of analysis 


# Extract the metadata and create a list that respect the list of EcoNumData
x <- readr::read_delim(file = file, 
                       delim = ";",
                       n_max = 13,
                       col_names = FALSE,
                       col_types = cols( .default = "c"),
                       locale = readr::locale(encoding = "LATIN1")) # particulary attention with the encoding system



# extract nutrient thatis  analysed

result_1 <- as.character(x[9,10])
result_2 <- as.character(x[9,13])
result_3 <- as.character(x[9,16])

# Prepare names of variables for raw_data

std_1 <- paste(result_1, "std", sep = "_")
conc_1 <- paste(result_1, "conc", sep = "_")
val_1 <-paste(result_1, "values", sep = "_")


std_2 <- paste(result_2, "std", sep = "_")
conc_2 <- paste(result_2, "conc", sep = "_")
val_2 <-paste(result_2, "values", sep = "_")


std_3 <- paste(result_3, "std", sep = "_")
conc_3 <- paste(result_3, "conc", sep = "_")
val_3 <-paste(result_3, "values", sep = "_")

# extract metadata

meta <- list(project = project, 
             sample = as.character(x[2,2]), 
             sample_date = as.POSIXlt(x = paste(as.character(x[3,2]),as.character(x[4,2]), sep =" "),
                                      format = "%d/%m/%Y %H:%M:%S"),
             author = as.character(x[5,2]),
             date = as.character(x[3,2]),
             comment = as.character(x[6,2]),
             method = as.character(x[1,2]))


# Extract raw data and remove a metadata 
raw_data <- readr::read_delim(file = file,
                              delim = ";",
                              skip = 14,
                              col_names = c("sample_id", 
                                            "peak_number",
                                            "cup_number",
                                            "sample_type",
                                            "cup_group", 
                                            "x_1", 
                                            "x_2",
                                            "date_time",
                                            std_1,
                                            conc_1,
                                            val_1,
                                            std_2,
                                            conc_2,
                                            val_2,
                                            std_3,
                                            conc_3,
                                            val_3,
                                            "x_3",
                                            "x_4"), 
                              col_types = cols(sample_id = col_character(),
                                               peak_number = col_integer(),
                                               cup_number = col_skip(),
                                               sample_type = col_character(),
                                               cup_group = col_skip(),
                                               x_1 = col_skip(),
                                               x_2 = col_skip(),
                                               date_time = col_datetime(format = "%d/%m/%Y %H:%M:%S"),
                                               x_3 = col_skip(),
                                               x_4 = col_skip(), .default = col_number()))


# Now, i have a list of metadata and raw_data 
# Applicable for method A and B 

# Now transform raw_data and a list of metadata turn into a data frame with EcoNumData Class : function is new_econum_data
# After that, save the EcoNumData with Repos_save



# function tentative 
# I would like prepare a function or routine analyses  to replace test version 1, test version 2, test version 3.




# function tentative #########
# I would like prepare a function or routine analyses  to replace test version 1, test version 2, test version 3.

# this function has for objectives to import txt file of seal AA3 
# and transform to prepare for function new_econumdata

library(econum)
library(readr)

file <- "data/171215A.txt"
project  <- "Nutrient_test"

import_aa3_beta_test <- function(file, project = basename(dirname(file))){
  
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
  
  channel_1 <- list(method  = as.character(header[9,10]), unity = as.character(header[10,10]), 
                     base = as.character(header[11,10]) , gain = as.character(header[12,10]), 
                    lamp = as.character(header[13,10]))
  
  channel_2 <- list(method  = as.character(header[9,13]), unity = as.character(header[10,13]) , 
                    base = as.character(header[11,13]) , gain = as.character(header[12,13]), 
                    lamp = as.character(header[13,10]))
  
  channel_3 <- list(method  = as.character(header[9,16]), unity = as.character(header[10,16]) , 
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
  
  # Extract information on calibration 
  
  calib1 <- raw_data[raw_data$sample_type=="CALB", c(5,7)]
  
  y_calib <- names(calib1)[2]
  x_calib <- names(calib1)[1]
  
  calib1 <- na.omit(calib1)
  cal1 <- lm(names(calib1)[1] ~ names(calib1)[2], data = calib1)
  
  
  summary(cal1)
  
  calib1 <- list(method =  results[1])
  
  
  attr(raw_data, "spec") <- NULL
  attr(raw_data, "method") <- method
  attr(raw_data, "calibration") <- calibration 
  
  new_econum_data(raw_data, metadata = meta, class = "AA3")
} 



#########SEE function_analyse_seal_data.R for last version##

##### Function script preparation 
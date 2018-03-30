

library(econum)
library(readr)

# Import function and convert txt file into EcoNumData ####

convert_aa3 <- function(file_aa3_txt, file_aa3_xlsx, project = basename(dirname(file))){
  
  # Extract the metadata and create a list that respect the list of EcoNumData
  header <- readr::read_delim(file = file_aa3_txt, 
                              delim = ";",
                              n_max = 13,
                              col_names = FALSE,
                              col_types = readr::cols( .default = "c"),
                              locale = readr::locale(encoding = "LATIN1")) # particulary attention with the encoding system
  
  # Extract nutrients that are analysed
  results <- as.character(header[9, c(10, 13, 16)])
  
  # Prepare names of variables for raw_data
  stds <- paste(results, "std", sep = "_")
  concs <- paste(results, "conc", sep = "_")
  vals <- paste(results, "values", sep = "_")
  
  # Change in data  NPinorganique.ANL in inorganique 
  header$X2[header$X2 == "NPinorganique.ANL" ] <- "inorga"
  header$X2[header$X2 == "NPorganique.ANL" ] <- "orga"
  
  # Create a several list wit the method's information
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
               sample = paste(sub("\\.RUN$", "", as.character(header[2, 2])), 
                              as.character(header[1,2]),sep = "-"), 
               sample_date = as.POSIXlt(paste(as.character(header[3, 2]),
                                              as.character(header[4, 2])),
                                        format = "%d/%m/%Y %H:%M:%S"),
               author = as.character(header[5, 2]),
               date = as.POSIXct(as.character(header[3, 2]), 
                                 format = "%d/%m/%Y"),
               comment = as.character(header[6, 2]),
               topic = as.character(header[1, 2]))
  
  # Extract raw data
  raw_data <- readr::read_delim(file = file_aa3_txt, delim = ";", skip = 13) 
  
  # recoding type of variable
  raw_data$`Cup Type` <- as.factor(raw_data$`Cup Type`)
  raw_data$`Date Time Stamp` <- as.POSIXct(raw_data$`Date Time Stamp`,
                                           format = "%d/%m/%Y %H:%M:%S")
  raw_data$X9 <- as.numeric(raw_data$X9)
  raw_data$`Results 1` <- as.numeric(raw_data$`Results 1`)
  raw_data$X12 <- as.numeric(raw_data$X12)
  raw_data$`Results 2` <- as.numeric(raw_data$`Results 2`)
  raw_data$X15 <- as.numeric(raw_data$X15)
  raw_data$`Results 3` <- as.numeric(raw_data$`Results 3`)
  
  # Add units' informations
  attr(raw_data$X9, "units") <- method$channel_1$unit
  attr(raw_data$`Results 1`, "units") <- method$channel_1$unit
  attr(raw_data$X12, "untis") <- method$channel_2$unit
  attr(raw_data$`Results 2`, "untis") <- method$channel_2$unit
  attr(raw_data$X15, "untis") <- method$channel_3$unit
  attr(raw_data$`Results 3`, "untis") <- method$channel_3$unit
  
  # Rename the columns in raw_data
  names(raw_data) <- c("sample_id", "peak_number",
                       "cup_number", "sample_type", "cup_group", 
                       "x_1", "x_2", "date_time", stds[1], concs[1],
                       vals[1], stds[2], concs[2], vals[2], stds[3],
                       concs[3], vals[3], "x_3", "x_4")
  
  # Select variables in raw_data
  raw_data <- raw_data[ , -c(3, 5:7, 18, 19)]
  
  
  
  # import xlsx file
  add_data <- readxl::read_xlsx(file_aa3_xlsx, sheet = "data")
  
  raw_data <- dplyr::left_join(raw_data, add_data, by = "sample_id")
  
  # Add method informations
  attr(raw_data, "method") <- method
  
  # Preparation of econum data object
  econum::new_econum_data(raw_data, metadata = meta, class = "aa3")
}


# first test
#test <- convert_aa3(file_aa3_txt = "Data/180305A.txt" ,file_aa3_xlsx = "Data/20180305.xlsx", project = "nutrient_aa3")

#Check with Data/180305B.txt", Data/180305A.txt", 
# Select the local repository
#econum::set_opt_econum("local_repos", "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data")

# Select the remote repository
#set_opt_econum("remote_repos", "/Volumes/Public/EcoNumData")

#econum::repos_save(object = convert_aa3(file_aa3_txt = "Data/180313D.txt" ,file_aa3_xlsx = "Data/180313D.xlsx", project = "nutrient_aa3"), remote = FALSE)


#econum::repos_load(file = "Data/nutrient_aa3/aa3/180313D-inorga_2018-03-13_13.41.32_5AA71480_aa3.RData")




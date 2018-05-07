# Function of conversion txt file in EcoNumData File for aa3 analysis
# The reflexion on the function place in file Report/data_conversion_seal_aa3.Rmd
# By Guyliann Engels 

# 
SciViews::R
library(readxl)
library(econum)

# one example with NA values for the project 
#file_aa3_txt <- "Data/180430AR1.txt" 
#file_aa3_xlsx <- "Data/180430AR1.xlsx"
#project <- "test"
#file_calibration <- "calibration_test"
# one example without NA values for the project
#file_aa3_txt <- "Data/180425B.txt" 
#file_aa3_xlsx <- "Data/180425B.xlsx"

conversion_aa3 <- function(file_aa3_txt, file_aa3_xlsx, project, topic = NULL, remote_repos, local_repos, remote, local){
  
  set_opt_econum(key = "local_repos", value = local_repos)
  set_opt_econum(key = "remote_repos", value = remote_repos)

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
  chl_1 <- list(method  = as.character(header[9,10]), 
                    unit = as.character(header[10,10]), 
                    base = as.character(header[11,10]) , 
                    gain = as.character(header[12,10]), 
                    lamp = as.character(header[13,10]))
  
  chl_2 <- list(method  = as.character(header[9,13]), 
                    unit = as.character(header[10,13]), 
                    base = as.character(header[11,13]) , 
                    gain = as.character(header[12,13]), 
                    lamp = as.character(header[13,10]))

  chl_3 <- list(method  = as.character(header[9,16]), 
                    unit = as.character(header[10,16]), 
                    base = as.character(header[11,16]),
                    gain = as.character(header[12,16]), 
                    lamp = as.character(header[13,16]))

  method <- list(channel_1 = chl_1, channel_2 = chl_2, channel_3 = chl_3) 

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
               topic = topic)
  
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
  
  samp <- raw_data[raw_data$sample_type == "SAMP", ] # samp contient des valeurs manquantes
  i <- 1
  for(i in 1:nrow(samp)){
    is.na(samp$project[i]) -> x
    if(x == TRUE){
      stop("Attention : Présence de valeurs manquantes dans la colonnes `project`, le fichier xlsx et txt ne correspondent pas entièrement.")} else{
        i+1
      }
  }
  attr(raw_data, "method") <- method
  # Preparation of econum data object
  nutrient <- new_econum_data(raw_data, metadata = meta, class = "aa3")
  repos_save(object = nutrient, remote = remote, local = local)
}


#nutrient <- conversion_aa3(file_aa3_txt = "Data/180425B.txt",file_aa3_xlsx = "Data/180425B.xlsx", project = "test",remote = FALSE, local = TRUE,local_repos = "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data", remote_repos = NA)

# save calibration data


uncoupled_data_aa3 <- function(nutrient, remote_repos, local_repos, remote, local){
  
  set_opt_econum(key = "local_repos", value = local_repos)
  set_opt_econum(key = "remote_repos", value = remote_repos)
  
  nutrient[nutrient$sample_type == "SAMP", ] -> samp
  
  i <- 1
  for( i in 1:nrow(samp)){
    x <- samp[i,]
    tp <- attr(nutrient, "metadata")$sample
    topic <- attr(nutrient, "metadata")$topic
    attr(x = x , which = "calibration") <- attr(nutrient, "metadata")$sample
    
    metadata <-  list(project = x$project[1], 
                      sample = x$sample[1], 
                      sample_date = as.POSIXct(x$sample_date[1]),
                      author = x$authors[1], 
                      date = as.POSIXct(x$date_time[1]),
                      comment = x$comment[1], 
                      topic = topic)
    
    test <- new_econum_data(x, metadata = metadata, class = "aa3")
    repos_save(object = test,remote = remote, local = local )
    i+1 
  }
}

#uncoupled_data_aa3(nutrient = nutrient, remote_repos = NA, local_repos = "~/Documents/these_engels_guyliann/protocol_seal_aa3/Data", remote = FALSE, local = TRUE)





# function tentative #########
# I would like prepare a function or routine analyses  to replace test version 1, test version 2, test version 3.

# this function has for objectives to import txt file of seal AA3 
# and transform to prepare for function new_econumdata

import_aa3 <-function(file, project){
  
  # Extract the metadata and create a list that respect the list of EcoNumData
  x <- readr::read_delim(file = file, 
                         delim = ";",
                         n_max = 13,
                         col_names = FALSE,
                         col_types = cols( .default = "c"),
                         locale = readr::locale(encoding = "LATIN1")) # particulary attention with the encoding system
  # extract nutrient that is  analysed
  
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
  
  # Change in data  NPinorganique.ANL in inorganique 
  x$X2[x$X2 == "NPinorganique.ANL" ] <- "a"
  x$X2[x$X2 == "NPorganique.ANL" ] <- "b"
  
  
  # extract metadata
  
  meta <- list(project = project, 
               sample = as.character(x[2,2]), 
               sample_date = as.POSIXlt(x = paste(as.character(x[3,2]),as.character(x[4,2]), sep =" "),
                                        format = "%d/%m/%Y %H:%M:%S"),
               author = as.character(x[5,2]),
               date = as.POSIXct(x = as.character(x[3,2]), format = "%d/%m/%Y"),
               comment = as.character(x[6,2]),
               topic = as.character(x[1,2]))
  
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
  
  list( data = raw_data, metadata = meta)
} 

# this is the routine analyses
# you must choose the file and the name of project

# test with 2 files one method A  "data/171215A.txt" and one method B "data/171214A.txt"

. <- import_aa3(file = "data/171215A.txt", project = "Nutrient_test") 

. <-new_econum_data(x = .$data, metadata = .$metadata,class = "aa3")

repos_save(object = .,local = TRUE, remote = FALSE)

SciViews::R

aa3_txt <- function(file_aa3_txt, project, topic = NULL){
  # Import metadata and extract informaation
  header_read <- readr::read_lines(file_aa3_txt, n_max = 13,
                                   locale = readr::locale(encoding = "LATIN1"))
  header <- stringr::str_extract_all(header_read,
                                     "(-?\u03BC?\\w+:?\\.?-?/?\\w*:?/?\\d*)")

  # Rename list elements
  sapply(header, `[[`, 1) -> names(header)

  # paste Comment
  if (lengths(header["COMM"]) > 2) {
    header$COMM[2] <- paste(header$COMM[-1], collapse = " ")
    header$COMM <- header$COMM[1:2]
  }

  # Check_1
  header_control <- c("ANAL", "RUN", "DATE", "TIME", "OPER", "COMM", "TYPE",
                      "CHAN", "METH", "UNIT", "Base", "Gain", "Lamp")
  length_list_element <- c(2,2,2,2,2,2,4,4,4,4,4,4,4)

  if (sum(names(header) != header_control) != 0 |
      sum(lengths(header) != length_list_element) != 0) {
    stop("attention : erreur durant l importation ou l extraction des metadonnees")
  }

  # Change NO3 in NOx
  stringr::str_replace_all(header$METH, "NO3", "NOx") -> header$METH

  # Extract nutrients that are analysed
  results <- header$METH[-1]

  # Prepare names of variables for raw_data
  stds <- paste(results, "std", sep = "_")
  concs <- paste(results, "conc", sep = "_")
  vals <- paste(results, "values", sep = "_")

  # Change in data NPinorganique.ANL in inorganique
  header$ANAL[2][header$ANAL[2]  == "NPinorganique.ANL" ] <- "inorga"
  header$ANAL[2][header$ANAL[2]  == "NPorganique.ANL" ] <- "orga"

  # Create a several list with the method information
  method <- list()
  for (i in 2:4) {
    method[[i - 1]] <- list(method = header$METH[i],
                            unit = header$UNIT[i],
                            base = header$Base[i],
                            gain = header$Gain[i],
                            lamp = header$Lamp[i])
  }

  dplyr::bind_rows(method) -> .
  as.data.frame(.) -> method
  row.names(method) <- c("channel_1", "channel_2", "channel_3")

  # Create a list with the meta information
  meta <- list(sample = paste(sub("\\.RUN$", "", header$RUN[2]),
                              header$ANAL[2],sep = "-"),
               project = project,
               sample_date = lubridate::dmy_hms(paste(header$DATE[2],
                                                      header$TIME[2])),
               author = header$OPER[2],
               date = lubridate::dmy(header$DATE[2]),
               comment = header$COMM[2], topic = topic)

  # Extract raw data
  raw_data <- readr::read_delim(file = file_aa3_txt, delim = ";", skip = 13)

  # Select variables in raw_data
  raw_data <- raw_data[ , -c(3, 5:7, 18, 19)]

  # Rename the columns in raw_data
  names(raw_data) <- c("sample_id", "peak_number", "sample_type",
                       "date_time", stds[1], concs[1], vals[1], stds[2],
                       concs[2], vals[2], stds[3], concs[3], vals[3])

  # recoding type of variable
  raw_data$sample_type <- as.factor(raw_data$sample_type)
  raw_data$date_time <- lubridate::dmy_hms(raw_data$date_time)

  # Add units informations
  for (i in 1:3) {
    attr(raw_data[[stds[i]]], "units") <- method$unit[[i]]
    attr(raw_data[[concs[i]]], "units") <- method$unit[[i]]
  }

  lm_list <- list()
  for( i in 1:3){
    # nutrient name
    nutri_name <-  stringr::str_split(vals[i], pattern = "_")[[1]][1]

    lm_mod <- stats::lm(raw_data,
                        formula = stats::as.formula(paste(vals[i],"~",stds[i])))

    # compute n for the data.frame
    dplyr::filter(raw_data, sample_type == "CALB") -> .
    dplyr::select(., stds[i]) ->.
    sum(!is.na(.)) -> n

    data.frame(std_name = paste(nutri_name),
               intercept = lm_mod$coefficients[[1]],
               values = lm_mod$coefficients[[2]],
               r_squared = round(summary(lm_mod)$r.squared,digits = 3),
               n = n
    ) ->  lm_list[[i]]

    names(lm_list)[i] <- paste(nutri_name)
  }
  lm_df <- dplyr::bind_rows(lm_list)

  attr(raw_data, "method") <- method
  attr(raw_data, "calb_lm") <- lm_df
  attr(raw_data, "metadata") <- meta
  class(raw_data) <- c("tbl_df", "tbl", "data.frame", "aa3")
  raw_data
}

# t <- aa3_txt(file_aa3_txt = "data/raw/190205A.TXT", project = "test", topic = NULL)


# function more simple to import aa3 file
aa3_txt_raw <- function(file_aa3_txt, project, topic = NULL){
  # Import metadata and extract informaation
  header_read <- readr::read_lines(file_aa3_txt, n_max = 13,
                                   locale = readr::locale(encoding = "LATIN1"))
  header <- stringr::str_extract_all(header_read,
                                     "(-?\u03BC?\\w+:?\\.?-?/?\\w*:?/?\\d*)")

  # Rename list elements
  sapply(header, `[[`, 1) -> names(header)

  # paste Comment
  if (lengths(header["COMM"]) > 2) {
    header$COMM[2] <- paste(header$COMM[-1], collapse = " ")
    header$COMM <- header$COMM[1:2]
  }

  # Check_1
  header_control <- c("ANAL", "RUN", "DATE", "TIME", "OPER", "COMM", "TYPE",
                      "CHAN", "METH", "UNIT", "Base", "Gain", "Lamp")
  length_list_element <- c(2,2,2,2,2,2,4,4,4,4,4,4,4)

  if (sum(names(header) != header_control) != 0 |
      sum(lengths(header) != length_list_element) != 0) {
    stop("attention : erreur durant l importation ou l extraction des metadonnees")
  }

  # Change NO3 in NOx
  stringr::str_replace_all(header$METH, "NO3", "NOx") -> header$METH

  # Extract nutrients that are analysed
  results <- header$METH[-1]

  # Prepare names of variables for raw_data
  stds <- paste(results, "std", sep = "_")
  concs <- paste(results, "conc", sep = "_")
  vals <- paste(results, "values", sep = "_")

  # Change in data NPinorganique.ANL in inorganique
  header$ANAL[2][header$ANAL[2]  == "NPinorganique.ANL" ] <- "inorga"
  header$ANAL[2][header$ANAL[2]  == "NPorganique.ANL" ] <- "orga"

  # Create a several list with the method information
  method <- list()
  for (i in 2:4) {
    method[[i - 1]] <- list(method = header$METH[i],
                            unit = header$UNIT[i],
                            base = header$Base[i],
                            gain = header$Gain[i],
                            lamp = header$Lamp[i])
  }

  dplyr::bind_rows(method) -> .
  as.data.frame(.) -> method
  row.names(method) <- c("channel_1", "channel_2", "channel_3")

  # Create a list with the meta information
  meta <- list(sample = paste(sub("\\.RUN$", "", header$RUN[2]),
                              header$ANAL[2],sep = "-"),
               project = project,
               sample_date = lubridate::dmy_hms(paste(header$DATE[2],
                                                      header$TIME[2])),
               author = header$OPER[2],
               date = lubridate::dmy(header$DATE[2]),
               comment = header$COMM[2], topic = topic)

  # Extract raw data
  raw_data <- readr::read_delim(file = file_aa3_txt, delim = ";", skip = 13)

  # Select variables in raw_data
  raw_data <- raw_data[ , -c(3, 5:7, 18, 19)]

  # Rename the columns in raw_data
  names(raw_data) <- c("sample_id", "peak_number", "sample_type",
                       "date_time", stds[1], concs[1], vals[1], stds[2],
                       concs[2], vals[2], stds[3], concs[3], vals[3])

  # recoding type of variable
  raw_data$sample_type <- as.factor(raw_data$sample_type)
  raw_data$date_time <- lubridate::dmy_hms(raw_data$date_time)

  # Add units informations
  for (i in 1:3) {
    attr(raw_data[[stds[i]]], "units") <- method$unit[[i]]
    attr(raw_data[[concs[i]]], "units") <- method$unit[[i]]
  }

  attr(raw_data, "method") <- method
  attr(raw_data, "metadata") <- meta
  class(raw_data) <- c("tbl_df", "tbl", "data.frame")
  raw_data
}

aa3_plot <- function(nutrient, data){
  ggplot(data, aes_string(x = "date_time", y = nutrient,
                         group = 1, color = "sample_type")) +
    geom_line() +
    geom_point() +
    theme_sciviews()
}

#' Title
#'
#' @param x aa3 data 
#'
#' @return a list containing a data.frame with the parameters of the regression (intercept, values, r squared and n) and a list of graphs for each method of dosage (plot of all data and linear regression plot) 
#' @export
#'
#' @examples
#' # Load some EcoNumData
#' econum::repos_load("Data/calibration/aa3/180531A-orga_2018-05-31_13.52.40_5B0F3B00_aa3.RData")
#' econum::repos_load("Data/calibration/aa3/180613E-inorga_2018-06-13_16.15.10_5B205E80_aa3.RData")
#' # Check data quality and calibration
#' calb_aa3(EcoNumData_aa3) 
#' 
calb_aa3 <- function(x){
  # Load packages
  require(flow)
  require(tidyverse)
  require(dplyr)
  require(ggplot2)
  require(ggpubr)
  require(ggrepel)
  
  # Paramètres
  param <- list(method_1 = list(col = c(5,7)),
                method_2 = list(col = c(8,10)),
                method_3 = list(col = c(11,13)))
  
  # Sample
  attr(x = x, which = "metadata")$sample -> samp_name
  
  # Lists
  graph_list <- list()
  calb_lm_list <- list()
  calb_db_list <- list()
  
  for (i in seq_along(param)) {
    # Samp Data
    x[,c(3:4, param[[i]]$col)] -> samp
    
    # method & sample
    attr(x = x, which = "method")[[i]]$method -> met
    
    # all_values plot
    all_values_plot <- ggplot(samp, aes(x = date_time, y = samp[,4], 
                                        col = sample_type, group = 1)) +
      geom_point() +
      geom_line() + 
      labs(y = "Values") +
      theme_bw() +
      theme(legend.direction = "horizontal", legend.position = "bottom") + 
      guides(col = guide_legend(title = "Sample",title.position = "top"))
    
    # CALB DATA
    samp[samp$sample_type == "CALB",  ]  %>.%
      na.omit(.) -> calb_data
    
    calb_data[ ,3:4] -> calb
    
    # CALB DATABASE
    calb_data %>.%
      mutate(., id_cal = paste(lubridate::date(calb_data$date_time),
                               str_split(colnames(calb_data)[4], 
                                         pattern = "_")[[1]][1],
                               calb_data[[3]], sep = "_"),
                date = lubridate::date(calb_data$date_time),
                time = base::strftime(calb_data$date_time, format = "%H:%M:%S"),
                project_id = attr(x, which = "metadata")$sample) -> calb_data
    
    calb_data %>.%
      tidyr::gather(., key = "std_type", value = "concentration", 3) -> calb_data
    
    mutate(calb_data, std_type = str_split(calb_data$std_type, 
                                           pattern = "_")[[1]][1],
                      units_ = attr(x, which = "method")[[i]]$unit ) -> calb_data
    
    names(calb_data)[3] <- "values"
    
    calb_data %>.% 
      select(., id_cal, project_id, date, time, std_type, units_, 
             concentration, values) -> calb_db_list[[i]]
    
    # CALB DATABASE (R base) 
    # calb_data$id_cal <- paste(lubridate::date(calb_data$date_time),
    #                      str_split(colnames(calb_data)[4], 
    #                      pattern = "_")[[1]][1],
    #                      calb_data[[3]], sep = "_")
    # calb_data$date <- lubridate::date(calb_data$date_time)
    # calb_data$time <- strftime(calb_data$date_time, format = "%H:%M:%S")
    # calb_data$project_id <- attr(x, which = "metadata")$sample
    
    # calb_data %>.%
    #   tidyr::gather(., key = "std_type", 
    #                    value = "concentration", 3) -> calb_data
    # calb_data$std_type <- str_split(calb_data$std_type, pattern = "_")[[1]][1] 
    # names(calb_data)[3] <- "values"
    # calb_data[,c("id_cal", "project_id", "date", "time", "std_type", 
    #           "concentration", "values")] -> calb_db_list[[i]]
    
    names(calb_db_list)[i] <- unique(calb_data$std_type)
    
    # linear model
    lmod <- lm(as.formula(paste0("calb$", names(calb)[2], "~ calb$", names(calb)[1])))
    data.frame(std_name = attr(x = x, which = "method")[[i]]$method,
               intercept = lmod$coefficients[1], values = lmod$coefficients[2], 
               r_squared = round(summary(lmod)$r.squared,digits = 4), 
               n = length(calb[,1])) -> calb_lm_list[[i]]
    names(calb_lm_list)[i] <- met
    
    # Equation 
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(calb_lm_list[[i]]$intercept, digits = 2),
                          b = format(calb_lm_list[[i]]$values, digits = 2),
                          r2 = format(calb_lm_list[[i]]$r_squared, digits = 3)))
    eq <- as.character(as.expression(eq))
    
    # graph
    calb_plot <- ggplot(calb, aes(calb[,1], calb[,2])) +
                  geom_point() +
                  labs( x = "Standard", y = "Values") +
                  geom_text(x = 2*(diff(range(calb[,1])))/5 , y = max(calb[,2]), 
                            label = eq, parse = TRUE) +
                  ggrepel::geom_text_repel(aes(label = calb[,1]), nudge_y = 1.5, 
                                           nudge_x = 1.5, direction = "both",
                                           segment.size = 0.2) +
                  geom_smooth(method = "lm") +
                  theme_bw()
    
    # combine plot
    ggpubr::ggarrange(all_values_plot, calb_plot) -> combine_plot
    ggpubr::annotate_figure(combine_plot, 
                            top = text_grob(met, size =  14, face = "bold"),
                            bottom = text_grob(paste0("Data source: ", samp_name), 
                                               color = "blue", hjust = 1, 
                                               x = 1, face = "italic", 
                                               size = 10)) -> graph_list[[i]]  
    names(graph_list)[i] <- met
  }
  
  # CALIBRATION list
  bind_rows(calb_lm_list)  -> lm_tab
  
  bind_rows(calb_db_list) -> calb_db
  
  x[x$sample_type == "SAMP",] -> samp_df
  
  calibration <- (list(calbdb = calb_db, regression = lm_tab, 
                       graph = graph_list, sampdb = samp_df))
  return(calibration)
}

# if (make_sampdb == TRUE) {
#     meth <- str_split(attr(x, which = "metadata")$sample, pattern = "-")[[1]][2]
# 
#     meth_list <- list(orga = c("Ptot", "Ntot", "NO2"),
#                       inorga = c("PO4", "NOx", "NH4"))
# 
#     values <- paste(meth_list[[meth]], "values", sep = "_")
#     concs <- paste(meth_list[[meth]], "conc", sep = "_")
# 
#     x %>.%
#       filter(., sample_type == "SAMP") %>.%
#       select(., sample_id, date_time, values[1], concs[1], values[2], concs[2],
#                 values[3], concs[3], project, sample_date, authors, comment) -> sample_db
# 
#     samp_list <- list(sample_db = sample_db)
#     calibration <- append(calibration, samp_list)
#     return(calibration)
# 
# } else {
#     return(calibration)
# } 


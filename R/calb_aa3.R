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
  
  for (i in seq_along(param)) {
    # Samp Data
    x[,c(3:4, param[[i]]$col)] -> samp
    
    # method & sample
    attr(x = x, which = "method")[[i]]$method -> met
    
    # all_values plot
    all_values_plot <- ggplot(samp, aes(x = date_time, y = samp[,4], col = sample_type, group = 1)) +
      geom_point() +
      geom_line() + 
      labs(y = "Values") +
      theme_bw() +
      theme(legend.direction = "horizontal", legend.position = "bottom") + 
      guides(col = guide_legend(title = "Sample",title.position = "top"))
    
    # CALB DATA
    samp[samp$sample_type == "CALB",  ]  %>.%
      na.omit(.) %>.% 
      .[ ,3:4] -> calb
    
    # linear model
    lmod <- lm(calb[,2] ~ calb[,1])
    calb_lm_list[[i]] <- data.frame(intercept = lmod$coefficients[1], 
                                    values = lmod$coefficients[2], 
                                    r_squared = round(summary(lmod)$r.squared,digits = 4), 
                                    n = length(calb[,1]),
                                    row.names = attr(x = x, which = "method")[[i]]$method)
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
      geom_text(x = 2*(diff(range(calb[,1])))/5 , y = max(calb[,2]), label = eq, parse = TRUE) +
      ggrepel::geom_text_repel(aes(label = calb[,1]), nudge_y = 1.5, nudge_x = 1.5, direction = "both",
                               segment.size = 0.2) +
      geom_smooth(method = "lm") +
      theme_bw()
    
    # combine plot
    ggpubr::ggarrange(all_values_plot, calb_plot) -> combine_plot
    graph_list[[i]] <- ggpubr::annotate_figure(combine_plot, 
                                               top = text_grob(met,
                                                               size =  14,
                                                               face = "bold"),
                                               bottom = text_grob(paste0("Data source: ", samp_name), 
                                                                  color = "blue",
                                                                  hjust = 1, 
                                                                  x = 1, 
                                                                  face = "italic", 
                                                                  size = 10))
    names(graph_list)[i] <- met
  }
  
  bind_rows(calb_lm_list)  -> lm_tab
  row.names(lm_tab) <- names(calb_lm_list)
  
  calibration <- (list(regression = lm_tab, graph = graph_list))
  return(calibration)
}






#### CALCUL NEW LM


filter_list <- list(Ptot = 10)

calc_newlm <- function(x, filter_list) {
  
  # Check_1 : names of list element
  nutrient_ctrl <- c("Ptot", "NO2", "NOx", "Ntot", "NH4", "PO4")
  if ( is.null(names(filter_list)) |
       !(names(filter_list) %in% nutrient_ctrl) ) {
    stop("Attention : pas de noms pour les éléments de la liste ou pas de correspondance, utiliser un ou plusieurs des noms suivant : 'Ptot', 'NO2', 'NOx', 'Ntot', 'NH4', 'PO4'")
  }
  
  
  nutri_std <- paste0(names(filter_list),"_std")
  nutri_values <- paste0(names(filter_list),"_values") 
  
  # CALB DATA
  x[x$sample_type == "CALB" & !(x[[nutri]] %in% filter_list[["Ptot"]]), ] 
  
  
  
}
  
  
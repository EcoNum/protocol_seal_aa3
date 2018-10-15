# Autoplot
# Create an autoplot for object AA3 EcoNumData 
# that show the graph with calibration curve
# This Rscirpt show the evolution of the function
# The last function is in the "function_analyse_seal_data.R"

# SciViews::R
library(econum)
library(flow)
# library(chart)
library(ggpubr)

## Import file with method a
#repos_load("Data/Nutrient_test/AA3/180222A_2018-02-22_15.08.18_5A8E0800_AA3.RData")

## Import file with method b
#repos_load("Data/Nutrient_test/AA3/171214A_2017-12-14_11.03.41_5A31BF00_AA3.RData")

#x <- EcoNumData_AA3.b
#x <- EcoNumData_AA3.a 

# calibration graph
# # chart_aa3_calb <- function(x){
#   samp <- x[x$sample_type == "CALB", ]
#   # Form that allows  to be adapted for the method A and method B
#   
#   # method_1
#   attr(x = x, which = "method")$channel_1$method -> m_1
#   
#   samp %>.%
#     select(., c(5,7)) %>.%
#     na.omit(.) -> calb1
#   #calb1
#   
#   a <- chart(data = calb1, formula = calb1[ , 2] ~ calb1[ , 1]) +
#     geom_point() +
#     labs( x = "Standard", y = "Values") +
#     geom_smooth(method = "lm")
#   #a
#   
#   # method_2
#   attr(x = x, which = "method")$channel_2$method -> m_2
#   
#   samp %>.%
#     select(., c(8,10)) %>.%
#     na.omit(.) -> calb2
#   #calb2
#   
#   b <- chart(data = calb2, formula = calb2[, 2] ~ calb2[, 1]) +
#     geom_point() +
#     labs( x = "Standard", y = "Values") +
#     geom_smooth(method = "lm")
#   #b
#   #
#   # method_3
#   attr(x = x, which = "method")$channel_3$method -> m_3
#   
#   samp %>.%
#     select(., c(11,13)) %>.%
#     na.omit(.) -> calb3
#   #calb3
#   
#   c <- chart(data = calb3, formula = calb3[ , 2] ~ calb3[ , 1]) +
#     geom_point() +
#     labs( x = "Standard", y = "Values") +
#     geom_smooth(method = "lm")
#   #c
#   
#   # combine plot
#   ggarrange(a,b,c,labels = c(m_1, m_2, m_3), 
#             font.label = list(size = 10, face = "plain"), align = "hv", label.x = 0.5)
# }
# 
# #chart_aa3_calb(x = EcoNumData_AA3.a)
# #chart_aa3_calb(x = EcoNumData_AA3.b)
# 
# 
# #calibration linear model
# # calb_linear_model <-function(x){
#   samp <- x[x$sample_type == "CALB", ]
#   # Form that allows  to be adapted for the method A and method B
#   
#   # method_1
#   attr(x = x, which = "method")$channel_1$method -> m_1
#   
#   samp %>.%
#     select(., c(5,7)) %>.%
#     na.omit(.) -> calb1
# 
#   lm_1 <- lm(data = calb1, formula = calb1[, 2] ~ calb1[, 1])
#   coef_1 <- lm_1$coefficients 
#   r_1 <-round(summary(lm_1)$r.squared,digits = 4)
#   n_1 <- length(calb1[ , 1])
# 
# 
#   # metod_2
#   attr(x = x, which = "method")$channel_2$method -> m_2
#   
#   samp %>.%
#     select(., c(8,10)) %>.%
#     na.omit(.) -> calb2
#   
#   lm_2 <- lm(data = calb2, formula = calb2[, 2] ~ calb2[, 1])
#   coef_2 <- lm_2$coefficients 
#   r_2 <-round(summary(lm_2)$r.squared,digits = 4)
#   n_2 <- length(calb2[ , 1])
#   
#   # metod_3
#   attr(x = x, which = "method")$channel_3$method -> m_3
#   
#   samp %>.%
#     select(., c(11,13)) %>.%
#     na.omit(.) -> calb3
#   
#   lm_3 <- lm(data = calb3, formula = calb3[, 2] ~ calb3[, 1])
#   coef_3 <- lm_3$coefficients 
#   r_3 <- round(summary(lm_3)$r.squared,digits = 4)
#   n_3 <- length(calb3[ , 1])
#   
#   #create data frame with information
#   data_frame(method = c(m_1, m_2, m_3), intercep = c(coef_1[1], coef_2[1], coef_3[1]), values = c(coef_1[2], coef_2[2], coef_3[2]), r_squared = c(r_1, r_2, r_3), n = c(n_1, n_2, n_3))
# }
# 
# #lm_tab <- calb_linear_model(EcoNumData_AA3.a)
# #lm_tab1 <- calb_linear_model(EcoNumData_AA3.b)
# 
# 
# # Combine two functions to create a graph with table 
# # 
# 
# # Last updated 2018-02-28
# # calb_aa3 <- function(x){
#   samp <- x[x$sample_type == "CALB", ]
#   # Form that allows  to be adapted for the method A and method B
#   
#   # method_1
#   attr(x = x, which = "method")$channel_1$method -> m_1
#   
#   samp %>.%
#     select(., c(5,7)) %>.%
#     na.omit(.) -> calb1
#   #calb1
#   
#   a <- chart(data = calb1, formula = calb1[ , 2] ~ calb1[ , 1]) +
#     geom_point() +
#     labs( x = "Standard", y = "Values") +
#     geom_smooth(method = "lm")
#   #a
#   
#   # method_2
#   attr(x = x, which = "method")$channel_2$method -> m_2
#   
#   samp %>.%
#     select(., c(8,10)) %>.%
#     na.omit(.) -> calb2
#   #calb2
#   
#   b <- chart(data = calb2, formula = calb2[, 2] ~ calb2[, 1]) +
#     geom_point() +
#     labs( x = "Standard", y = "Values") +
#     geom_smooth(method = "lm")
#   #b
#   #
#   # method_3
#   attr(x = x, which = "method")$channel_3$method -> m_3
#   
#   samp %>.%
#     select(., c(11,13)) %>.%
#     na.omit(.) -> calb3
#   #calb3
#   
#   c <- chart(data = calb3, formula = calb3[ , 2] ~ calb3[ , 1]) +
#     geom_point() +
#     labs( x = "Standard", y = "Values") +
#     geom_smooth(method = "lm")
#   #c
#   
#   # Summary of data
#   
#   lm_tab <- calb_linear_model(x = x)
#   
#   ## Template for tab 
#   mytheme <- gridExtra::ttheme_default(
#     core = list(fg_params = list(cex = 0.7)),
#     colhead = list(fg_params = list(cex = 0.7)),
#     rowhead = list(fg_params = list(cex = 0.7)))
#   
#   ## Create tab with lm's coefficients 
#   d <- gridExtra::tableGrob(lm_tab, theme = mytheme)
#   
#   # combine plot
#   ggarrange(a,b,c,d, labels = c(m_1, m_2, m_3, "Linear model"), 
#             font.label = list(size = 14, face = "bold"), align = "hv",
#             label.x = c(0.5, 0.5, 0.5, 0.2))
# }
# 
# #calb_aa3(EcoNumData_AA3.a)
# #calb_aa3(EcoNumData_AA3.b)
# 
# ### update 2018-10-08
# reposLoad("Data/calibration/aa3/180531A-orga_2018-05-31_13.52.40_5B0F3B00_aa3.RData")
# # reposLoad("Data/calibration/aa3/180613E-inorga_2018-06-13_16.15.10_5B205E80_aa3.RData")
# library(dplyr)
# library(flow)
# 
# # Fonction calb2_aa3
# # calb2_aa3 <- function(x, channel_all = TRUE){
#   
#   # Fonction calb_linear_model
#   calb_linear_model <- function(x){
#     lm <- lm(x[,2] ~ x[,1])
#     return(data.frame(intercept = lm$coefficients[1], 
#                       values = lm$coefficients[2], 
#                       r_squared = round(summary(lm)$r.squared,digits = 4), 
#                       n = length(x[,1]),
#                       row.names = attr(x = x, which = "method")[[i]]$method))
#   }
#   
#   # Paramètres
#   param <- list(method_1 = list(channel = 1, col = c(5,7)),
#                 method_2 = list(channel = 2, col = c(8,10)),
#                 method_3 = list(channel = 3, col = c(11,13)))
#   
#   # CALB Data
#   x %>.%
#     filter(., sample_type == "CALB") -> samp
#   
#   # Lists
#   graph_list <- list()
#   calb_lm_list <- list()
#   
#   for (i in seq_along(param)){
#     samp %>.%
#       select(., param[[i]]$col) %>.%
#       na.omit(.) -> calb
#     
#     # method
#     attr(x = x, which = "method")[[i]]$method -> m_1
#     
#     # linear model
#     calb_lm_list[[i]] <- calb_linear_model(calb)
#     names(calb_lm_list)[i]<- m_1
#     
#     # Equation 
#     # eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#     #                  list(a = format(calb_lm_list[[i]]$intercept, digits = 2), 
#     #                       b = format(calb_lm_list[[i]]$values, digits = 2), 
#     #                       r2 = format(calb_lm_list[[i]]$r_squared, digits = 3)))
#     # eq <- as.character(as.expression(eq))
#     eq <- paste0("(y = ", format(calb_lm_list[[i]]$intercept, digits = 2), " + ", format(calb_lm_list[[i]]$values, digits = 2), " * x ; R squared = ", 
#                  format(calb_lm_list[[i]]$r_squared, digits = 3), " ; n = ", calb_lm_list[[i]]$n, ")" )
#      
#     # graph
#     graph_list[[i]] <- ggplot(calb, aes(calb[,1], calb[,2])) +
#       geom_point() +
#       labs( x = "Standard", y = "Values") +
#       ggtitle(paste0(m_1," - ", eq)) +
#       # geom_text(x = 2.5, y = 60000, label = eq, parse = TRUE) +
#       geom_smooth(method = "lm")
# 
#     names(graph_list)[i]<- m_1
#     
#   }
#   
#   bind_rows(calb_lm_list) %>.% 
#      mutate(.,method = names(calb_lm_list)) %>.%
#      select(.,method, intercept, values, r_squared, n) -> lm_tab
#    
#   ## Template for tab 
#   mytheme <- gridExtra::ttheme_default(
#     core = list(fg_params = list(cex = 0.7)),
#     colhead = list(fg_params = list(cex = 0.7)),
#     rowhead = list(fg_params = list(cex = 0.7)))
#   
#   ## Create tab with lm's coefficients 
#   d <- gridExtra::tableGrob(lm_tab, theme = mytheme)
#   
#   # combine plot
#   ggarrange(graph_list[[1]], graph_list[[2]], graph_list[[3]], d, labels = c("","","","Linear model"), 
#             font.label = list(size = 14, face = "bold"), align = "hv",
#             label.x = c(0.5, 0.5, 0.5, 0.2))
#   
# } # FIN DE LA FONCTION calb2_aa3

### update 2018-10-09
repos_load("Data/calibration/aa3/180531A-orga_2018-05-31_13.52.40_5B0F3B00_aa3.RData")
# reposLoad("Data/calibration/aa3/180613E-inorga_2018-06-13_16.15.10_5B205E80_aa3.RData")

# library(dplyr)
library(flow)
library(tidyverse)

# Fonction calb2_aa3
calb_aa3 <- function(x){
  # Load packages
  require(flow)
  require(tidyverse)
  require(dplyr)
  require(ggplot2)
  require(ggpubr)
  
  # Paramètres
  param <- list(method_1 = list(channel = 1, col = c(5,7)),
                method_2 = list(channel = 2, col = c(8,10)),
                method_3 = list(channel = 3, col = c(11,13)))
  
  # Sample
  attr(x = x, which = "metadata")$sample -> samp_name
  
  # Lists
  graph_list <- list()
  calb_lm_list <- list()
  
  for (i in seq_along(param)) {
    # Samp Data
    x %>.%
      select(., date_time, sample_type, param[[i]]$col) -> samp
    
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
      select(., 3:4) -> calb
    
    # linear model
    lmod <- lm(calb[,2] ~ calb[,1])
    calb_lm_list[[i]] <- data.frame(intercept = lmod$coefficients[1], 
                              values = lmod$coefficients[2], 
                              r_squared = round(summary(lmod)$r.squared,digits = 4), 
                              n = length(x[,1]),
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
    # %>.% 
    # mutate(.,method = names(calb_lm_list)) %>.%
    # select(.,method, intercept, values, r_squared, n) -> lm_tab
    
  calibration <- (list(regression = lm_tab, graph = graph_list))
  return(calibration)
  
} # FIN DE LA FONCTION calb2_aa3
 


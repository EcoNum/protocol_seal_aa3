# Autoplot
# Create an autoplot for object AA3 EcoNumData 
# that show the graph with calibration curve
# This Rscirpt show the evolution of the function
# The last function is in the "function_analyse_seal_data.R"

SciViews::R
library(econum)
library(flow)
library(chart)
library(ggpubr)

## Import file with method a
#repos_load("Data/Nutrient_test/AA3/180222A_2018-02-22_15.08.18_5A8E0800_AA3.RData")

## Import file with method b
#repos_load("Data/Nutrient_test/AA3/171214A_2017-12-14_11.03.41_5A31BF00_AA3.RData")

#x <- EcoNumData_AA3.b
#x <- EcoNumData_AA3.a 

# calibration graph
chart_aa3_calb <- function(x){
  samp <- x[x$sample_type == "CALB", ]
  # Form that allows  to be adapted for the method A and method B
  
  # method_1
  attr(x = x, which = "method")$channel_1$method -> m_1
  
  samp %>.%
    select(., c(5,7)) %>.%
    na.omit(.) -> calb1
  #calb1
  
  a <- chart(data = calb1, formula = calb1[ , 2] ~ calb1[ , 1]) +
    geom_point() +
    labs( x = "Standard", y = "Values") +
    geom_smooth(method = "lm")
  #a
  
  # method_2
  attr(x = x, which = "method")$channel_2$method -> m_2
  
  samp %>.%
    select(., c(8,10)) %>.%
    na.omit(.) -> calb2
  #calb2
  
  b <- chart(data = calb2, formula = calb2[, 2] ~ calb2[, 1]) +
    geom_point() +
    labs( x = "Standard", y = "Values") +
    geom_smooth(method = "lm")
  #b
  #
  # method_3
  attr(x = x, which = "method")$channel_3$method -> m_3
  
  samp %>.%
    select(., c(11,13)) %>.%
    na.omit(.) -> calb3
  #calb3
  
  c <- chart(data = calb3, formula = calb3[ , 2] ~ calb3[ , 1]) +
    geom_point() +
    labs( x = "Standard", y = "Values") +
    geom_smooth(method = "lm")
  #c
  
  # combine plot
  ggarrange(a,b,c,labels = c(m_1, m_2, m_3), 
            font.label = list(size = 10, face = "plain"), align = "hv", label.x = 0.5)
}

#chart_aa3_calb(x = EcoNumData_AA3.a)
#chart_aa3_calb(x = EcoNumData_AA3.b)


#calibration linear model
calb_linear_model <-function(x){
  samp <- x[x$sample_type == "CALB", ]
  # Form that allows  to be adapted for the method A and method B
  
  # method_1
  attr(x = x, which = "method")$channel_1$method -> m_1
  
  samp %>.%
    select(., c(5,7)) %>.%
    na.omit(.) -> calb1

  lm_1 <- lm(data = calb1, formula = calb1[, 2] ~ calb1[, 1])
  coef_1 <- lm_1$coefficients 
  r_1 <-round(summary(lm_1)$r.squared,digits = 4)
  n_1 <- length(calb1[ , 1])


  # metod_2
  attr(x = x, which = "method")$channel_2$method -> m_2
  
  samp %>.%
    select(., c(8,10)) %>.%
    na.omit(.) -> calb2
  
  lm_2 <- lm(data = calb2, formula = calb2[, 2] ~ calb2[, 1])
  coef_2 <- lm_2$coefficients 
  r_2 <-round(summary(lm_2)$r.squared,digits = 4)
  n_2 <- length(calb2[ , 1])
  
  # metod_3
  attr(x = x, which = "method")$channel_3$method -> m_3
  
  samp %>.%
    select(., c(11,13)) %>.%
    na.omit(.) -> calb3
  
  lm_3 <- lm(data = calb3, formula = calb3[, 2] ~ calb3[, 1])
  coef_3 <- lm_3$coefficients 
  r_3 <- round(summary(lm_3)$r.squared,digits = 4)
  n_3 <- length(calb3[ , 1])
  
  #create data frame with information
  data_frame(method = c(m_1, m_2, m_3), intercep = c(coef_1[1], coef_2[1], coef_3[1]), values = c(coef_1[2], coef_2[2], coef_3[2]), r_squared = c(r_1, r_2, r_3), n = c(n_1, n_2, n_3))
}

#lm_tab <- calb_linear_model(EcoNumData_AA3.a)
#lm_tab1 <- calb_linear_model(EcoNumData_AA3.b)


# Combine two functions to create a graph with table 
# 

# Last updated 2018-02-28
calb_aa3 <- function(x){
  samp <- x[x$sample_type == "CALB", ]
  # Form that allows  to be adapted for the method A and method B
  
  # method_1
  attr(x = x, which = "method")$channel_1$method -> m_1
  
  samp %>.%
    select(., c(5,7)) %>.%
    na.omit(.) -> calb1
  #calb1
  
  a <- chart(data = calb1, formula = calb1[ , 2] ~ calb1[ , 1]) +
    geom_point() +
    labs( x = "Standard", y = "Values") +
    geom_smooth(method = "lm")
  #a
  
  # method_2
  attr(x = x, which = "method")$channel_2$method -> m_2
  
  samp %>.%
    select(., c(8,10)) %>.%
    na.omit(.) -> calb2
  #calb2
  
  b <- chart(data = calb2, formula = calb2[, 2] ~ calb2[, 1]) +
    geom_point() +
    labs( x = "Standard", y = "Values") +
    geom_smooth(method = "lm")
  #b
  #
  # method_3
  attr(x = x, which = "method")$channel_3$method -> m_3
  
  samp %>.%
    select(., c(11,13)) %>.%
    na.omit(.) -> calb3
  #calb3
  
  c <- chart(data = calb3, formula = calb3[ , 2] ~ calb3[ , 1]) +
    geom_point() +
    labs( x = "Standard", y = "Values") +
    geom_smooth(method = "lm")
  #c
  
  # Summary of data
  
  lm_tab <- calb_linear_model(x = x)
  
  ## Template for tab 
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params = list(cex = 0.7)),
    colhead = list(fg_params = list(cex = 0.7)),
    rowhead = list(fg_params = list(cex = 0.7)))
  
  ## Create tab with lm's coefficients 
  d <- gridExtra::tableGrob(lm_tab, theme = mytheme)
  
  # combine plot
  ggarrange(a,b,c,d, labels = c(m_1, m_2, m_3, "Linear model"), 
            font.label = list(size = 14, face = "bold"), align = "hv",
            label.x = c(0.5, 0.5, 0.5, 0.2))
}

#calb_aa3(EcoNumData_AA3.a)
#calb_aa3(EcoNumData_AA3.b)



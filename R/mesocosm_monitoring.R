
SciViews::R
library(econum)
direction <- "Data/mesocosm_monitoring/aa3"
source("R/combine_aa3_data.R")

chart(filter(nutri, sample != "AR1" & sample != "AR2" & sample != "BR1" & sample != "BR2"), formula = PO4_conc~sample_date)+
  geom_line() +
  geom_point() +
  geom_rect(mapping = aes(xmin = as.POSIXct("2018-05-11 12:00:00"), xmax = as.POSIXct("2018-05-18 12:00:00"), ymin = 0, ymax = 2), fill = "lightblue",alpha = 0.03) +
  geom_text(mapping = aes(x = as.POSIXct("2018-05-14 12:00:00"), y = 1.8, label = "CS002", size = 8)) +
  geom_vline(xintercept = as.POSIXct("2018-05-03 12:00:00")) +
  facet_wrap(~ sample, nrow = 2) 

chart(filter(nutri, sample != "AR1" & sample != "AR2" & sample != "BR1" & sample != "BR2"), formula = NO3_conc~sample_date)+
  geom_line() +
  geom_point() +
  geom_rect(mapping = aes(xmin = as.POSIXct("2018-05-11 12:00:00"), xmax = as.POSIXct("2018-05-18 12:00:00"), ymin = 0, ymax = 2.3), fill = "lightblue",alpha = 0.03) +
  geom_text(mapping = aes(x = as.POSIXct("2018-05-14 12:00:00"), y = 1.8, label = "CS002", size = 8)) +
  geom_vline(xintercept = as.POSIXct("2018-05-03 12:00:00")) +
  facet_wrap(~ sample, nrow = 2) 


chart(filter(nutri, sample != "AR1" & sample != "AR2" & sample != "BR1" & sample != "BR2"), formula = NH4_conc~sample_date)+
  geom_line() +
  geom_point() +
  geom_rect(mapping = aes(xmin = as.POSIXct("2018-05-11 12:00:00"), xmax = as.POSIXct("2018-05-18 12:00:00"), ymin = 0, ymax = 4), fill = "lightblue",alpha = 0.03) +
  geom_text(mapping = aes(x = as.POSIXct("2018-05-14 12:00:00"), y = 1.8, label = "CS002", size = 8)) +
  geom_vline(xintercept = as.POSIXct("2018-05-03 12:00:00")) +
  facet_wrap(~ sample, nrow = 2) 

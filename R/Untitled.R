
library(econum)
library(chart)
library(flow)

SciViews::R

repos_load(file = "Data/Nutrient_test/AA3/180222A_2018-02-22_15.08.18_5A8E0800_AA3.RData")

samp <- EcoNumData_AA3.a[EcoNumData_AA3.a$sample_type == "SAMP", ]
samp$sample_id <- as.factor(samp$sample_id)
class(samp)

chart(samp,PO4_conc ~ sample_id, type = "auto")

samp %>.%
  group_by(., sample_id) %>.%
  summarise(., mean = mean(PO4_conc))

chart(samp, NO3_conc ~ sample_id, type = "auto")

samp %>.%
  group_by(., sample_id)%>.%
  summarise(., mean = mean(NO3_conc))

chart(samp,NH4_conc ~ sample_id, type = "auto")

chart(samp,NH4_conc ~ sample_id) +
  geom_point()

samp %>.%
  group_by(., sample_id)%>.%
  summarise(., mean = mean(NH4_conc))

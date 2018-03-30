

# Use : devtools::install_github("SciViews/SciViews")
# Use : devtools::install_github("SciViews/flow", build_vignettes = TRUE)
# Use : devtools::install_github("SciViews/chart", build_vignettes = TRUE)

library(econum)
repos_load(file = "Data/oceano2018/AA3/180313D_2018-03-13_13.41.32_5AA71480_AA3.RData")

SciViews::R

EcoNumData_AA3.a %>.%
  filter(., sample_type == "SAMP") %>.%
  separate(., col = sample_id, into = c("sample", "author"), sep = " " ) -> samp


chart(samp, formula = PO4_conc ~ sample) +
  geom_boxplot()

chart(samp, formula = NO3_conc ~ sample) +
  geom_boxplot()

chart(samp, formula = NH4_conc ~ sample) +
  geom_boxplot()

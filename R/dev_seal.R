

econum::repos_load("Data/calibration/aa3/180612A-orga_2018-06-12_15.18.22_5B1F0D00_aa3.RData")

SciViews::R

# optimised graph

EcoNumData_aa3 -> nutri
nutri <- filter(nutri, sample_type %in% c("CALB", "SAMP"))

nutri$sample_rec <- c(rep("calb_orga", times = "6"), 
                      rep("calb_NO2", times = "6"),
                      rep("calb_inorga", times = "3"),
                      rep("KNO3", times = "3"),
                      rep("POD", times = "3"),
                      rep("NO3_PO4", times = "3"),
                      rep("KH2PO4", times = "3"),
                      rep("calb_orga", times = "3"),
                      rep("calb_NO2", times = "3"),
                      rep("aqua", times = "2")
                      )

nutri$Ptot_predict <- c(10, 5, 2, 1, 0.5, 0.1,
                        0, 0, 0, 0, 0, 0,
                        10, 5, 1,
                        0, 0, 0, 
                        10, 5, 1,
                        10, 5, 1,
                        10, 5, 1,
                        10, 5, 1,
                        0, 0, 0,
                        NA, NA)

nutri$Ptot_predict_2 <- c(rep(NA, times = 12),
                          7.68, 3.84, 0.768,
                          rep(NA, times = 6),
                          7.68, 3.84, 0.768,
                          rep(NA, times = 11))



nutri$Ntot_predict <- c(20, 10, 4, 2, 1, 0.2,
                        10, 5, 2, 1, 0.5, 0.1,
                        20, 10, 2,
                        10, 5, 1, 
                        10, 5, 1,
                        10, 5, 1,
                        0, 0, 0,
                        20, 10, 2,
                        10, 5, 1,
                        NA, NA)


nutri <- separate(nutri, sample, c("samp", "method", "spec"), remove = FALSE)
nutri$Ntot_conc_rec <- nutri$Ntot_conc*2

a <- chart(nutri, formula = Ptot_conc ~ sample_rec) +
  geom_point()+
  geom_point(f_aes(Ptot_predict ~ sample_rec), color = "red") +
  geom_point(f_aes(Ptot_predict_2 ~ sample_rec),  col = "green") +
  labs(y = "Concentration en phosphore total dissous [µmol/L]", x = " ", captions = "Variation des concentration en phosphore total dissous attendues (en rouge), obtenues (en noir). \n Les points en vert sur le graphique représente une prédiction avec le dihydrogénophosphate de sodium non seché")

b <- chart(nutri, formula = Ntot_conc_rec ~ sample_rec) +
  geom_point()+
  geom_point(f_aes(Ntot_predict ~ sample_rec), col = "red") +
  labs( y = "Concentration en azote total dissous [µmol/L]", x = " ")


econum::repos_load("Data/calibration/aa3/180613E-inorga_2018-06-13_16.15.10_5B205E80_aa3.RData")
nutri1 <- EcoNumData_aa3
nutri1 <- filter(nutri1, sample_type %in% c("CALB", "SAMP"))
nutri1$sample_rec <- c(rep("calb_inorga", times = "6"),
                      rep("calb_inorga", times = "3"),
                      rep("KNO3", times = "3"),
                      rep("POD", times = "3"),
                      rep("NO3_PO4", times = "3"),
                      rep("KH2PO4", times = "3"),
                      rep("calb_orga", times = "3"),
                      rep("calb_NO2", times = "3"),
                      rep("aqua", times = "2")
)

nutri1$PO4_predict <- c(10, 5, 2, 1, 0.5, 0.1,
                        10, 5, 1,
                        0, 0, 0,
                        0, 0, 0,
                        10, 5, 1,
                        10, 5, 1,
                        0, 0, 0,
                        10, 5, 1,
                        NA, NA)


nutri1$NO3_predict <- c(10, 5, 2, 1, 0.5, 0.1,
                        10, 5, 1,
                        10, 5, 1,
                        0, 0, 0,
                        10, 5, 1,
                        0, 0, 0,
                        10, 5, 1,
                        10, 5, 1,
                        NA, NA)


# First graph
c <- chart(nutri1, formula = PO4_conc ~ sample_rec) +
  geom_point() +
  labs( y = "Concentration en phosphate total dissous [µmol/L]", x = " ") +
  geom_point(f_aes(PO4_predict ~ sample_rec), color = "red")


d <- chart(nutri1, formula = NO3_conc ~ sample_rec) +
  geom_point() +
  labs( y = "Concentration en nitrate total dissous [µmol/L]", x = " ") +
  geom_point(f_aes(NO3_predict ~ sample_rec), color = "red") +
  scale_y_continuous(limits = c(-1, 11))
library(ggpubr)

ggarrange(a,c)

ggarrange(b,d)

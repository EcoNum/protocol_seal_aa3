# MERGE SAMP function 
merge_sampdb <- function(sampdb_org, sampdb_inorg) {
  
  nutrient = c("Ptot_conc", "Ntot_conc", "PO4_conc", "NOx_conc", 
               "NO2_conc", "NO3_conc", "NH4_conc")
  nutrient_new = paste(nutrient, "new", sep = "_")
  
  for (i in nutrient_new) {
    if (i %in% c(names(sampdb_inorg), names(sampdb_org))) {
      nutrient <- c(nutrient, i)
    }
  }
  
  sort(nutrient) -> nutrient
  
  # JOIN sampdb_org and sampdb_inorg
  sampdb_org %>.%
    left_join(., sampdb_inorg, by = "sample_id") %>.%
    # Calcul NO3_conc
    mutate(., NO3_conc = NOx_conc - NO2_conc,
           calb_orga_date = lubridate::date(date_time.x),
           calb_inorga_date = lubridate::date(date_time.y)) %>.%
    select(., sample.x, sample_date.x, nutrient, sample_id, project.x, 
           calb_orga_date, orga_filename, calb_inorga_date,
           inorga_filename, authors.x, comment.x) -> samp_org_inorg
  
  # Rename df colname 
  names(samp_org_inorg) <- stringr::str_replace(names(samp_org_inorg), 
                                                pattern = "\\.x", 
                                                replacement = "")
  
  return(samp_org_inorg)
}

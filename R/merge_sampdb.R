# MERGE SAMP function 
merge_sampdb <- function(sampdb_org, sampdb_inorg, 
                         nutrient = c("Ptot_conc", "Ntot_conc", "PO4_conc", "NOx_conc", 
                                      "NO2_conc", "NO3_conc", "NH4_conc") ) {
  
  # JOIN sampdb_org and sampdb_inorg
  sampdb_org %>.%
    left_join(., sampdb_inorg, by = "sample_id") %>.%
    # Calcul NO3_conc
    mutate(., NO3_conc = NOx_conc - NO2_conc,
           calb_orga_date = lubridate::date(date_time.x),
           calb_inorga_date = lubridate::date(date_time.y)) %>.%
    select(., sample_id, sample.x, nutrient, project.x, 
           sample_date.x, calb_orga_date, calb_inorga_date,
           authors.x, comment.x) -> samp_org_inorg
  
  # Rename df colname 
  names(samp_org_inorg) <- stringr::str_replace(names(samp_org_inorg), 
                                                pattern = "\\.x", 
                                                replacement = "")
  
  return(samp_org_inorg)
}
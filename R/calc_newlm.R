#### CALCUL NEW LM

# filter_list <- list(Ptot = 5, NO2 = 10)

calc_newlm <- function(EcoNumData, filter_list) {
  
  calb_df <- EcoNumData$calbdb
  # samp_df <- EcoNumData$sample_db
  
  # Check_1 : names of list element
  nutrient_ctrl <- c("Ptot", "NO2", "NOx", "Ntot", "NH4", "PO4")
  if ( is.null(names(filter_list)) ||
       !(names(filter_list) %in% nutrient_ctrl) ) {
    stop("Attention : pas de noms pour les éléments de la liste ou pas de 
         correspondance, utiliser un ou plusieurs des noms suivants : 
         'Ptot', 'NO2', 'NOx', 'Ntot', 'NH4', 'PO4'")
  }
  
  # graph lists status
  graph_list <- list()
  calb_lm_list <- list()
  
  for (i in 1:length(names(filter_list))) {
    type <- names(filter_list[i])
    conc <- filter_list[[i]]
    
    # CALB DATA select
    calb_df[calb_df$std_type ==  type & !(calb_df$concentration %in% conc),] -> calb
    
    # new linear model 
    lm(data = calb, formula = values ~ concentration ) -> lm_mod
    
    calb_lm_list[[i]] <- data.frame(std_name = paste(type, "new", sep = "_"),
                                    intercept = lm_mod$coefficients[1], 
                                    values = lm_mod$coefficients[2], 
                                    r_squared = round(summary(lm_mod)$r.squared,
                                                      digits = 4), 
                                    n = length(calb[,1]))
    names(calb_lm_list)[i] <- paste(type, "new", sep = "_")
    
    # Equation 
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(calb_lm_list[[i]]$intercept, digits = 2),
                          b = format(calb_lm_list[[i]]$values, digits = 2),
                          r2 = format(calb_lm_list[[i]]$r_squared, digits = 3)))
    eq <- as.character(as.expression(eq))
    
    # graph
    graph_list[[i]] <- ggplot(calb, aes(calb$concentration, calb$values)) +
      geom_point() +
      labs( x = "Standard", y = "Values") +
      geom_text(x = 2*(diff(range(calb$concentration)))/5 , y = max(calb$values), 
                label = eq, parse = TRUE) +
      ggrepel::geom_text_repel(aes(label = calb$concentration), nudge_y = 1.5, 
                               nudge_x = 1.5, direction = "both", 
                               segment.size = 0.2) +
      geom_smooth(method = "lm") +
      ggtitle(paste(type, "new", sep = "_")) +
      theme_bw()
    
    names(graph_list)[i] <- paste(type, "new", sep = "_")
    
    # add new nutrient values
    cname <- paste(type, "conc", sep = "_")
    cnum <- which(names(aa3$sampdb) == cname)
    names(EcoNumData$sampdb)[cnum] <- paste(type, "conc_old", sep = "_")
    EcoNumData$sampdb %>.%
      dplyr::mutate(., new = round((EcoNumData$sampdb[,paste(type, "values", sep = "_")] -
                                      lm_mod$coefficients[[1]]) /
                                     lm_mod$coefficients[[2]],3)) -> EcoNumData$sampdb
    
    names(EcoNumData$sampdb)[length(EcoNumData$sampdb)] <- paste(type,"conc", sep = "_")
  }
  
  # add new graph, regression parameter and nutrient values in EcoNumData
  # samp_list <- list(samp = samp_df)
  # EcoNumData <- append(EcoNumData, samp_list)
  EcoNumData$graph <- append(EcoNumData$graph, graph_list)
  EcoNumData$regression <- bind_rows(EcoNumData$regression, calb_lm_list)
  
  return(EcoNumData)
}


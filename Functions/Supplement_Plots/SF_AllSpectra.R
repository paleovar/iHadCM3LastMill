#################################################
## Plot all Spectra

## PLOT

filter <- list(
  ISOT = c(2.0, 16.0, 8.0),
  ITPC = c(3.0, 9.0, 3.0)
)

COLZ <- c("#1A2254", "#0A4296", "#278BCE","#91002B", "#BD6B73", "black")
cex_text = 1.7
site_data <- read.csv("Data/SISALv2/site.csv")

for(var in c("ISOT", "ITPC")){
  pdf(file = paste0("Sup_Plots/SF_AllSpectra_1_",var,".pdf"), width = 21, height = 29.7)
  par(mfrow=c(7,6),oma = c(3,4,0,0) + 0.1,mar = c(1,0,1,1) + 0.1)
  for(entity_number in 1:42){
    entity = SPECTRA$entities_spec_rec[entity_number]
    LPlot(SPECTRA$SIM_full$a[[var]][[paste0("ENTITY",entity)]], col = adjustcolor(COLZ[1], alpha = 0.5), 
          ylim = c(0.00005,1000), xlim = c(1/300, 0.5),
          ylab = "",
          xaxt = 'n',
          yaxt = "n",
          panel.first = grid(equilogs = F, ny = NA),
          xlab = "", lwd = 2)
    abline(h = 0.1, lty = 3, col = "lightgrey")
    abline(h = 0.001, lty = 3, col = "lightgrey")
    abline(h = 10, lty = 3, col = "lightgrey")
    abline(h = 1000, lty = 3, col = "lightgrey")
    LLines(LogSmooth(SPECTRA$SIM_full$a[[var]][[paste0("ENTITY",entity)]]), col = COLZ[1], lw = 3)#,
    #main = TeX("Mean Spectra from cave locations (res>8)"))
    if(entity_number %in% c(37,38,39,40,41,42)){
      mtext("Period (y)", side = 1, line= 2.5, cex = cex_text/1.3)  
      axis(side = 1, at = c(0.002, 0.005, 0.01, 0.02, 0.05, 0.2, 0.5), 
           labels = c(1/0.002, 1/0.005, 1/0.01, 1/0.02, 1/0.05, 1/0.2, 1/0.5), cex.axis = cex_text)
    }
    if(entity_number %in% c(1,7,13,19,25,31,37)){
      mtext("Power spectral sensity", side = 2, line= 2.5, cex = cex_text/1.3)  
      axis(side = 2, at = c(1e-3, 1e-1, 1e1, 1e3), 
           labels = c(1e-3, 0.1, 10, 1000), cex.axis = cex_text)
    }
    
    
    LLines(SPECTRA$SIM_filter_full_down[[var]]$a[[paste0("ENTITY",entity)]], col = COLZ[2], lty = 3, lw = 2)
    LLines(LogSmooth(SPECTRA$SIM_filter_full_down[[var]]$a[[paste0("ENTITY",entity)]]), col = COLZ[2], lw = 2)
    LLines(SPECTRA$SIM_filter_full_rec[[var]]$a[[paste0("ENTITY",entity)]], col = COLZ[3], lty = 3, lw = 2)
    LLines(LogSmooth(SPECTRA$SIM_filter_full_rec[[var]]$a[[paste0("ENTITY",entity)]]), col = COLZ[3], lw = 2)
    #text(0.2, 8e2, "HadCM3 yearly res.", col = "#074893")
    #text(0.3, 3e2, "5y filter", col = "#074893")
    #text(0.3, 1e2, "50y filter", col = "#074893")
    
    
    
    LLines(SPECTRA$SIM_ds$a[[var]][[paste0("ENTITY",entity)]], col = adjustcolor(COLZ[4], alpha = 0.5), lw = 2)
    LLines(LogSmooth(SPECTRA$SIM_ds$a[[var]][[paste0("ENTITY",entity)]]), col = COLZ[4], lw = 2)
    LLines(SPECTRA$SIM_filter_down_rec[[var]]$a[[paste0("ENTITY",entity)]], col = COLZ[5], lty = 3, lw = 2)
    LLines(LogSmooth(SPECTRA$SIM_filter_down_rec[[var]]$a[[paste0("ENTITY",entity)]]), col = COLZ[5], lw = 2)
    #text(0.02, 0.01, "HadCM3 down-sampled", col = COLZ[4])
    #text(0.02, 0.001, "10y filter", col = COLZ[4])
    
    LLines(SPECTRA$RECORDS[[paste0("ENTITY",entity)]], col = adjustcolor(COLZ[6], alpha = 0.5), lw = 2)
    LLines(LogSmooth(SPECTRA$RECORDS[[paste0("ENTITY",entity)]]), col = COLZ[6], lw = 3)
    #text(0.02, 0.0005, "Records", col = "black")
    legend(1/550,0.025, legend = c("HadCM3 (full)", paste0(" ...  ",filter[[var]][1],"y filter"), paste0(" ...  ",filter[[var]][2],"y filter"), "HadCM3 (down-sampled)", paste0(" ...  ",filter[[var]][3],"y filter"), "Record"), 
           text.col = c(COLZ[1],COLZ[2],COLZ[3],COLZ[4],COLZ[5],COLZ[6]), bty = "n", cex = cex_text)
    
    site_name = as.character(site_data$site_name[site_data$site_id == DATA_past1000$CAVES$entity_info$site_id[DATA_past1000$CAVES$entity_info$entity_id == entity]])
    text(0.5,700, paste0("eID ", entity), adj = 1, cex = cex_text*1.2)
    text(0.5, 250, paste0("(site: ",site_name,")"), adj = 1, cex = cex_text)
    if(var == "ISOT"){
      text(0.5, 100, TeX("HadCM3: $\\delta^{18}O$ in prec"), adj = 1, cex = cex_text)
    }
    if(var == "ITPC"){
      text(0.5, 100, TeX("HadCM3: prec-weighted $\\delta^{18}O$"), adj = 1, cex = cex_text)
    }
    
    if(entity_number<27){
      LETTER <- letters[entity_number]  
    }else if(entity_number < 53){
      LETTER <- paste0("a", letters[entity_number-26])
    }else if(entity_number < 79){
      LETTER <- paste0("b", letters[entity_number-52])
    }else if(entity_number < 105){
      LETTER <- paste0("c", letters[entity_number-78])
    }else{
      LETTER <- paste0("d", letters[entity_number-104])
    }
    
    mtext(side = 3, at = 1/250, paste0(LETTER,")"), line = -2, cex = cex_text)
  }
  
  
  
  dev.off()
  
  pdf(file = paste0("Sup_Plots/SF_AllSpectra_2_",var,".pdf"), width = 21, height = 29.7)
  par(mfrow=c(7,6),oma = c(3,4,0,0) + 0.1,mar = c(1,0,1,1) + 0.1)
  for(entity_number in 43:84){
    entity = SPECTRA$entities_spec_rec[entity_number]
    if(entity %in% c(351, 390)){next}
    LPlot(SPECTRA$SIM_full$a[[var]][[paste0("ENTITY",entity)]], col = adjustcolor(COLZ[1], alpha = 0.5), 
          ylim = c(0.00005,1000), xlim = c(1/300, 0.5),
          ylab = "",
          xaxt = 'n',
          yaxt = "n",
          panel.first = grid(equilogs = F, ny = NA),
          xlab = "", lwd = 2)
    abline(h = 0.1, lty = 3, col = "lightgrey")
    abline(h = 0.001, lty = 3, col = "lightgrey")
    abline(h = 10, lty = 3, col = "lightgrey")
    abline(h = 1000, lty = 3, col = "lightgrey")
    LLines(LogSmooth(SPECTRA$SIM_full$a[[var]][[paste0("ENTITY",entity)]]), col = COLZ[1], lw = 3)#,
    #main = TeX("Mean Spectra from cave locations (res>8)"))
    
    if(entity_number %in% c(79,80,81,82,83,84)){
      mtext("Period (y)", side = 1, line= 2.5, cex = cex_text/1.3)  
      axis(side = 1, at = c(0.002, 0.005, 0.01, 0.02, 0.05, 0.2, 0.5), 
           labels = c(1/0.002, 1/0.005, 1/0.01, 1/0.02, 1/0.05, 1/0.2, 1/0.5), cex.axis = cex_text)
      
    }
    if(entity_number %in% c(43,50,56,62,68,74)){
      mtext("Power spectral sensity", side = 2, line= 2.5, cex = cex_text/1.3)  
      axis(side = 2, at = c(1e-3, 1e-1, 1e1, 1e3), 
           labels = c(1e-3, 0.1, 10, 1000), cex.axis = cex_text)
    }
    
    LLines(SPECTRA$SIM_filter_full_down[[var]]$a[[paste0("ENTITY",entity)]], col = COLZ[2], lty = 3, lw = 2)
    LLines(LogSmooth(SPECTRA$SIM_filter_full_down[[var]]$a[[paste0("ENTITY",entity)]]), col = COLZ[2], lw = 2)
    LLines(SPECTRA$SIM_filter_full_rec[[var]]$a[[paste0("ENTITY",entity)]], col = COLZ[3], lty = 3, lw = 2)
    LLines(LogSmooth(SPECTRA$SIM_filter_full_rec[[var]]$a[[paste0("ENTITY",entity)]]), col = COLZ[3], lw = 2)
    LLines(SPECTRA$SIM_ds$a[[var]][[paste0("ENTITY",entity)]], col = adjustcolor(COLZ[4], alpha = 0.5), lw = 2)
    LLines(LogSmooth(SPECTRA$SIM_ds$a[[var]][[paste0("ENTITY",entity)]]), col = COLZ[4], lw = 2)
    LLines(SPECTRA$SIM_filter_down_rec[[var]]$a[[paste0("ENTITY",entity)]], col = COLZ[5], lty = 3, lw = 2)
    LLines(LogSmooth(SPECTRA$SIM_filter_down_rec[[var]]$a[[paste0("ENTITY",entity)]]), col = COLZ[5], lw = 2)
    #text(0.02, 0.01, "HadCM3 down-sampled", col = COLZ[4])
    #text(0.02, 0.001, "10y filter", col = COLZ[4])
    
    LLines(SPECTRA$RECORDS[[paste0("ENTITY",entity)]], col = adjustcolor(COLZ[6], alpha = 0.5), lw = 2)
    LLines(LogSmooth(SPECTRA$RECORDS[[paste0("ENTITY",entity)]]), col = COLZ[6], lw = 3)
    #text(0.02, 0.0005, "Records", col = "black")
    legend(1/550,0.025, legend = c("HadCM3 (full)", paste0(" ...  ",filter[[var]][1],"y filter"), paste0(" ...  ",filter[[var]][2],"y filter"), "HadCM3 (down-sampled)", paste0(" ...  ",filter[[var]][3],"y filter"), "Record"), 
           text.col = c(COLZ[1],COLZ[2],COLZ[3],COLZ[4],COLZ[5],COLZ[6]), bty = "n", cex = cex_text)
    
    site_name = as.character(site_data$site_name[site_data$site_id == DATA_past1000$CAVES$entity_info$site_id[DATA_past1000$CAVES$entity_info$entity_id == entity]])
    text(0.5,700, paste0("eID ", entity), adj = 1, cex = cex_text*1.2)
    text(0.5, 250, paste0("(site: ",site_name,")"), adj = 1, cex = cex_text)
    if(var == "ISOT"){
      text(0.5, 100, TeX("HadCM3: $\\delta^{18}O$ in prec"), adj = 1, cex = cex_text)
    }
    if(var == "ITPC"){
      text(0.5, 100, TeX("HadCM3: prec-weighted $\\delta^{18}O$"), adj = 1, cex = cex_text)
    }
    
    if(entity_number<27){
      LETTER <- letters[entity_number]  
    }else if(entity_number < 53){
      LETTER <- paste0("a", letters[entity_number-26])
    }else if(entity_number < 79){
      LETTER <- paste0("b", letters[entity_number-52])
    }else if(entity_number < 105){
      LETTER <- paste0("c", letters[entity_number-78])
    }else{
      LETTER <- paste0("d", letters[entity_number-104])
    }
    
    mtext(side = 3, at = 1/250, paste0(LETTER,")"), line = -1.7, cex = cex_text)
  }
  
  
  
  dev.off()
  
}

rm(cex_text, COLZ, entity, var)

#################################################
## Paper Figure 8 ###############################
#################################################

## Network 

#################################################

library(plyr)
library(dplyr)
library(tidyverse)
library(zoo)
library(nest)
library(PaleoSpec)

lats = DATA_past1000$CAVES$entity_info$latitude[mask_spec]
longs = DATA_past1000$CAVES$entity_info$longitude[mask_spec]

dist<-fossil::earth.dist(cbind(lats,longs),dist=TRUE)
dist_matrix <- as.matrix(dist)

#################################################

source("Functions/networkmap_simple3.R")

plot_dist = dist_matrix
plot_dist[lower.tri(dist_matrix)] = NA
plot_dist_e = dist_matrix_e
plot_dist_e[lower.tri(dist_matrix_e)] = NA

link_density = 0.05

#FULL
C_SIM_full = NETWORK[[paste0("sim_full_", run)]]
o_sim_full = order(abs(C_SIM_full), na.last = F)
C_SIM_full[o_sim_full[1:floor((length(o_sim_full)-link_density*length(o_sim_full)))]] =  NA
C_SIM_full[NETWORK[[paste0("sim_full_", run, "_p")]]>0.1] = NA

plot_c_sim_full = NETWORK[[paste0("sim_full_", run)]]
plot_c_sim_full[lower.tri(NETWORK[[paste0("sim_full_", run)]], diag = FALSE)] = NA

#DOWN
C_SIM_down = NETWORK[[paste0("sim_down_", run)]]
o_sim_down = order(abs(C_SIM_down), na.last = F)
C_SIM_down[o_sim_full[1:floor((length(o_sim_down)-link_density*length(o_sim_down)))]] =  NA
C_SIM_down[NETWORK[[paste0("sim_down_", run, "_p")]]>0.1] = NA

plot_c_sim_down = NETWORK[[paste0("sim_down_", run)]]
plot_c_sim_down[lower.tri(NETWORK[[paste0("sim_down_", run)]], diag = FALSE)] = NA

#REC
C_REC = NETWORK$record
o_rec = order(abs(C_REC), na.last = F)
C_REC[o_rec[1:floor((length(o_rec)-link_density*length(o_rec)))]] = NA
C_REC[NETWORK$record_p>0.1] = NA

plot_c_rec = NETWORK$record
plot_c_rec[lower.tri(NETWORK$record)] = NA

plot_c_rec_max_e <- c_ensemble
plot_c_rec_max_e[lower.tri(c_ensemble)] = NA

boxes_sim_full <- list()
boxes_sim_down <- list()
boxes_rec <- list()

for(ii in 1:20){
  boxes_sim_full[[paste0(ii*1000)]] <- na.omit(as.numeric(plot_c_sim_full[plot_dist<ii*1000 & plot_dist>(ii-1)*1000]))
  boxes_sim_down[[paste0(ii*1000)]] <- na.omit(as.numeric(plot_c_sim_down[plot_dist<ii*1000 & plot_dist>(ii-1)*1000]))
  boxes_rec[[paste0(ii*1000)]] <- na.omit(as.numeric(plot_c_rec[plot_dist<ii*1000 & plot_dist>(ii-1)*1000]))
}

all_scale = 1.4
scaling = 1.5*all_scale
spacing = 0.7*all_scale
namcex = 1*all_scale
colorbar_length = 0.7

rbPal <- colorRampPalette(c("#2166ac", "grey", "#b2182b"))
COLZ <- array(rbPal(9))
#col_lines<-c("#B2182B","#0068C4")
col_lines<-c("black","#006400")
col_lines<-c("black","#d56f00")


#col_lines<-c("black","#762a83")

pdf(file = paste0("Paper_Plots/Fig8_Network_revised_", run,".pdf"), height= PLOTTING_VARIABLES$HEIGHT/2*3, width = PLOTTING_VARIABLES$WIDTH)
#png(file = paste0("Paper_Plots/Fig8_Network_revised_", run,".png"), height= 100*PLOTTING_VARIABLES$HEIGHT/2*3, width = 100*PLOTTING_VARIABLES$WIDTH)
par(mfrow=c(3,2), mai = c(rep(spacing, 4)), mar = c(1.5,2.5,0,0.5), oma = c(2,0.5,0.5,0.5))
#SIM MAP FULL
networkmap_simple3(CMAT = C_SIM_full, 
                   lat = lats, 
                   lon = longs, 
                   title = "",
                   thresh = 0.1)
fields::colorbar.plot(x = 190,y = 10, col = rev(COLZ), 
                      strip = c(1,0.75, 0.5, 0.25,0,-0.25, -0.5, -0.75,-1), horizontal = F, strip.length = colorbar_length, strip.width = 0.04)
axis(4,at=seq(80,-60,by=-2*140/8),labels=FALSE)
mtext("HadCM3@SISALv2 full", side = 1, cex = namcex, line = -2, font = 2)
mtext("(a)", side = 3, adj = 0, cex = namcex, line = -1.5, at = -185)
#SIM Cor-Dist
plot(plot_dist[seq(1,length(plot_dist), by = 2)], plot_c_sim_full[seq(1,length(plot_c_sim_full), by = 2)], 
     ylim = c(-1,1),xlim = c(0,20000),
     ylab = "", xlab = "", 
     cex = 1, lwd = 0.5, pch = 23,
     panel.first = grid(), col = adjustcolor("grey", alpha.f = 0.3), xaxt = "n")
limits = c(1, 0.875, 0.625, 0.375, 0.125, -0.125, -0.375, -0.625, -0.875)
for(ii in 1:(length(limits)-1)){
  points(dist_matrix[C_SIM_full>limits[ii+1] & C_SIM_full<limits[ii]], C_SIM_full[C_SIM_full>limits[ii+1] & C_SIM_full<limits[ii]], pch = 23, 
         col = adjustcolor(rev(COLZ)[ii], alpha.f = 0.6))  
}
points(dist_matrix[C_SIM_full<limits[9]], C_SIM_full[C_SIM_full<limits[9]], pch = 23, col = adjustcolor(rev(COLZ)[9], alpha.f = 0.7))
abline(h=0)
for(ii in 1:20){
  boxplot(boxes_sim_full[[paste0(ii*1000)]], add = TRUE, at = c(ii*1000-500),boxwex = 1000, names = "n", axes = F, outline = F)  
}


lo <- loess(plot_c_sim_full[order(plot_dist)] ~ plot_dist[order(plot_dist)], span = 0.2)

lines(lo$x, lo$fitted, lwd = 4, col = col_lines[1])

#lines(lowess(lowess_dist_sorted,lowess_c_sim_sorted, f=0.1), lwd = 4, col = "#B2182B")
mtext("(b)", side = 3, adj = 0, cex = namcex, line = -1.5, at = 1000)
mtext("HadCM3@SISALv2 full", side = 1, cex = namcex, line = -2, font = 2)

#SIM MAP DOWN
networkmap_simple3(CMAT = C_SIM_down, 
                   lat = lats, 
                   lon = longs, 
                   title = "",
                   thresh = 0.1)
fields::colorbar.plot(x = 190,y = 10, col = rev(COLZ), 
                      strip = c(1,0.75, 0.5, 0.25,0,-0.25, -0.5, -0.75,-1), horizontal = F, strip.length = colorbar_length, strip.width = 0.04)
axis(4,at=seq(80,-60,by=-2*140/8),labels=FALSE)
mtext("HadCM3@SISALv2 down", side = 1, cex = namcex, line = -2, font = 2)
mtext("(c)", side = 3, adj = 0, cex = namcex, line = -1.5, at = -185)
#SIM Cor-Dist
plot(plot_dist[seq(1,length(plot_dist), by = 2)], plot_c_sim_down[seq(1,length(plot_c_sim_down), by = 2)], 
     ylim = c(-1,1),xlim = c(0,20000),
     ylab = "", xlab = "", 
     cex = 1, lwd = 0.5, pch = 23,
     panel.first = grid(), col = adjustcolor("grey", alpha.f = 0.3), xaxt = "n")
limits = c(1, 0.875, 0.625, 0.375, 0.125, -0.125, -0.375, -0.625, -0.875)
for(ii in 1:(length(limits)-1)){
  points(dist_matrix[C_SIM_down>limits[ii+1] & C_SIM_down<limits[ii]], C_SIM_down[C_SIM_down>limits[ii+1] & C_SIM_down<limits[ii]], pch = 23, 
         col = adjustcolor(rev(COLZ)[ii], alpha.f = 0.6))  
}
points(dist_matrix[C_SIM_down<limits[9]], C_SIM_down[C_SIM_down<limits[9]], pch = 23, col = adjustcolor(rev(COLZ)[9], alpha.f = 0.7))
abline(h=0)
for(ii in 1:20){
  boxplot(boxes_sim_down[[paste0(ii*1000)]], add = TRUE, at = c(ii*1000-500),boxwex = 1000, names = "n", axes = F, outline = F)  
}


lo <- loess(plot_c_sim_down[order(plot_dist)] ~ plot_dist[order(plot_dist)], span = 0.2)

lines(lo$x, lo$fitted, lwd = 4, col = col_lines[1])

#lines(lowess(lowess_dist_sorted,lowess_c_sim_sorted, f=0.1), lwd = 4, col = "#B2182B")
mtext("(d)", side = 3, adj = 0, cex = namcex, line = -1.5, at = 1000)
mtext("HadCM3@SISALv2 down", side = 1, cex = namcex, line = -2, font = 2)

#SISAL MAP
networkmap_simple3(CMAT = C_REC, 
                   lat = lats, 
                   lon = longs,
                   title = "",
                   thresh = 0.1)
fields::colorbar.plot(x = 190,y = 10, col = rev(COLZ), 
                      strip = c(1,0.75, 0.5, 0.25,0,-0.25, -0.5, -0.75,-1), horizontal = F, strip.length = colorbar_length, strip.width = 0.04)
axis(4,at=seq(80,-60,by=-2*140/8),labels=FALSE)
mtext("SISALv2", side = 1, cex = namcex, line = -2, font = 2)
mtext("(e)", side = 3, adj = 0, cex = namcex, line = -1.5, at = -185)

#SISAL Cor-Dist
plot(plot_dist[seq(1,length(plot_dist), by = 2)], plot_c_rec[seq(1,length(plot_c_rec), by = 2)], 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "",
     cex.axis = all_scale, 
     lwd = 1,  pch = 24,
     panel.first = grid(), col = adjustcolor("grey", alpha.f = 0.3))
limits = c(1, 0.875, 0.625, 0.375, 0.125, -0.125, -0.375, -0.625, -0.875)
for(ii in 1:(length(limits)-1)){
  points(dist_matrix[C_REC>limits[ii+1] & C_REC<=limits[ii]], C_REC[C_REC>limits[ii+1] & C_REC<=limits[ii]], pch = 24, 
         col = adjustcolor(rev(COLZ)[ii], alpha.f = 0.6))  
}
points(dist_matrix[C_REC<limits[9]], C_REC[C_REC<limits[9]], pch = 24, col = adjustcolor(rev(COLZ)[9], alpha.f = 0.7))
abline(h=0)
for(ii in 1:20){
  boxplot(boxes_rec[[paste0(ii*1000)]], add = TRUE, at = c(ii*1000-500),boxwex = 1000, names = "n", axes = F, outline = F)  
}

lo <- loess(plot_c_rec[order(plot_dist)] ~ plot_dist[order(plot_dist)], span = 0.2)
lines(lo$x, lo$fitted, lwd = 4, col = col_lines[1])
lo <- loess(plot_c_rec_max_e[order(plot_dist_e)] ~ plot_dist_e[order(plot_dist_e)], span = 0.2)
lines(lo$x, lo$fitted, lwd = 4, col = col_lines[2])
mtext("SISALv2", side = 1, cex = namcex, line = -2, font = 2)
mtext("Distance between pairs (km)", side= 1, line = 2)
mtext("(f)", side = 3, adj = 0, cex = namcex, line = -1.5, at = 0)
text(20000, 0.9, "original chron.", col = col_lines[1], adj = 1, cex = all_scale)
text(20000, 0.77, "sisal ensemble", col = col_lines[2], adj = 1, cex = all_scale)

dev.off()






rm(boxes_rec, boxes_sim_full, boxes_sim_down, COLZ, lo, plot_c_rec, plot_dist, ii, lats, link_density,longs, namcex, o_rec, o_sim_full, o_sim_down, scaling, spacing,
   plot_c_sim_full, plot_c_sim_down, C_REC, C_SIM_full, C_SIM_down, plot_c_rec_max_e, plot_dist_e, colorbar_length, limits, all_scale, dist)

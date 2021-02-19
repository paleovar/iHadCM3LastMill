#################################################
# CORRELATION MAP
#################################################
library(plyr)
library(dplyr)
library(tidyverse)
library(latex2exp)
library(zoo)
library(nest)
library(PaleoSpec)
#source("Functions/projection_ptlyr.R")
#source("Functions/STACYmap_PMIL.R")
source("Functions/STACYmap_PMIL_NAgrid.R")

# this is correlation with downsampled temp and prec

#################################################
## ANALYSIS

load("Data/ClimModes_xnapa.RData")
txt_size = 10


#################################################
## PLOTS ########################################
#################################################

#ENSO
Plot_lyr  = ClimModes$ENSO$corr
Plot_lyr_p =ClimModes$ENSO$p
Plot_lyr[Plot_lyr_p > 0.1] = NA
Plot_lyr[1,1] = 1

Plot_lyr <- rbind(Plot_lyr[49:96,1:73],Plot_lyr[1:48,1:73])

#Plot

NA_plot_lyr = Plot_lyr
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1


plot_ENSO <- STACYmap_NA(gridlyr = Plot_lyr, centercolor = 0, graticules = T,
                         NA_gridlyr = NA_plot_lyr, NA_color = "grey", legend_names = list(grid = TeX("$\\rho (ENSO, \\delta^{18}O)$"))) +
  theme(panel.border = element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = txt_size),
        legend.title = element_text(size = txt_size))

plot_ENSO

#NAO
Plot_lyr  = ClimModes$NAO$corr
Plot_lyr_p =ClimModes$NAO$p
Plot_lyr[Plot_lyr_p > 0.1] = NA
Plot_lyr <- rbind(Plot_lyr[49:96,1:73],Plot_lyr[1:48,1:73])
NA_plot_lyr = Plot_lyr
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1
Plot_lyr[1,1] = 1

plot_NAO <- STACYmap_NA(gridlyr = Plot_lyr, centercolor = 0, graticules = T,
                        NA_gridlyr = NA_plot_lyr, NA_color = "grey", legend_names = list(grid = TeX("$\\rho (NAO, \\delta^{18}O)$"))) +
  theme(panel.border = element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = txt_size),
        legend.title = element_text(size = txt_size))
plot_NAO

library(ggpubr)
plot <- ggarrange(plot_ENSO, plot_NAO,
                  labels = c("(a)", "(b)"),
                  ncol = 2, nrow = 1)

plot  %>% ggsave(filename = paste0('SF_ClimMods_xnapa.pdf'), plot = ., path = 'Sup_Plots', 
                 width = 2*12, height = 12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "pdf")
plot  %>% ggsave(filename = paste0('SF_ClimMods_xnapa.png'), plot = ., path = 'Sup_Plots', 
                 width = 2*12, height = 12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "png")


remove(COR, double_time, plot, Plot_lyr_prec, Plot_lyr_prec_p, Plot_lyr_temp, Plot_lyr_temp_p, plot_prec, plot_temp, Point_Lyr_prec, Point_Lyr_temp)
remove(entity, ii, record, run, sim, var, Point_Lyr_prec_not, Point_Lyr_prec_not_p, Point_Lyr_prec_p, Point_Lyr_temp_not, Point_Lyr_temp_not_p, Point_Lyr_temp_p)
remove(CORR, COR_CAVE, CORR_FIELD, data_rec, NA_plot_lyr,lon,lat)

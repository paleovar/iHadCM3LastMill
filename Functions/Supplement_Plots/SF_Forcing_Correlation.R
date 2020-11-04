#################################################
## SF Forcing Correlation
#################################################

library(plyr)
library(dplyr)
library(tidyverse)
library(latex2exp)

load("Data/CorMap_Forcing.RData")
source("Functions/STACYmap_PMIL_NAgrid.R")

## Correlation using ensemble mean:

#TEMP
Plot_lyr_sol = CorrMap_Forcing$solar_corr_ens.mean$TEMP
Plot_lyr_sol[CorrMap_Forcing$solar_p_ens.mean$TEMP > 0.1] = NA
Plot_lyr_vol = CorrMap_Forcing$volcanic_corr_ens.mean$TEMP
Plot_lyr_vol[CorrMap_Forcing$volcanic_p_ens.mean$TEMP > 0.1] = NA

Plot_lyr_sol <- rbind(Plot_lyr_sol[49:96,1:73],Plot_lyr_sol[1:48,1:73])
Plot_lyr_vol <- rbind(Plot_lyr_vol[49:96,1:73],Plot_lyr_vol[1:48,1:73])

#Plot

GLOBAL_STACY_OPTIONS$GLOBAL_POINT_SIZE <- 4

NA_plot_lyr = Plot_lyr_sol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1


plot_sol_temp <- STACYmap_NA(gridlyr = Plot_lyr_sol, centercolor = 0, graticules = T,
                             NA_gridlyr = NA_plot_lyr, NA_color = "grey", legend_names = list(grid = TeX("$\\rho$ (solar forcing, $\\delta^{18}O)$")),
                             allmax = 1) +
  theme(panel.border = element_blank(),legend.background = element_blank(), axis.text = element_blank(), text = element_text(size = 12), legend.title = element_text(size = 12))

#plot_sol

NA_plot_lyr = Plot_lyr_vol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1

plot_vol_temp <- STACYmap_NA(gridlyr = Plot_lyr_vol, centercolor = 0, graticules = T,
                             NA_gridlyr = NA_plot_lyr, NA_color = "grey",
                             legend_names = list(grid = TeX("$\\rho$ (volcanic forcing, $\\delta^{18}O)$")),
                             allmax = 1) + 
  theme(panel.border = element_blank(), legend.background = element_blank(), axis.text = element_blank(), text = element_text(size = 12), legend.title = element_text(size = 12))

#PREC
Plot_lyr_sol = CorrMap_Forcing$solar_corr_ens.mean$PREC
Plot_lyr_sol[CorrMap_Forcing$solar_p_ens.mean$PREC > 0.1] = NA
Plot_lyr_vol = CorrMap_Forcing$volcanic_corr_ens.mean$PREC
Plot_lyr_vol[CorrMap_Forcing$volcanic_p_ens.mean$PREC > 0.1] = NA

Plot_lyr_sol <- rbind(Plot_lyr_sol[49:96,1:73],Plot_lyr_sol[1:48,1:73])
Plot_lyr_vol <- rbind(Plot_lyr_vol[49:96,1:73],Plot_lyr_vol[1:48,1:73])

#Plot

GLOBAL_STACY_OPTIONS$GLOBAL_POINT_SIZE <- 4

NA_plot_lyr = Plot_lyr_sol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1


plot_sol_prec <- STACYmap_NA(gridlyr = Plot_lyr_sol, centercolor = 0, graticules = T,
                             NA_gridlyr = NA_plot_lyr, NA_color = "grey", legend_names = list(grid = TeX("$\\rho$ (solar forcing, $\\delta^{18}O)$")),
                             allmax = 1) +
  theme(panel.border = element_blank(),legend.background = element_blank(), axis.text = element_blank(), text = element_text(size = 12), legend.title = element_text(size = 12))

#plot_sol

NA_plot_lyr = Plot_lyr_vol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1

plot_vol_prec <- STACYmap_NA(gridlyr = Plot_lyr_vol, centercolor = 0, graticules = T,
                             NA_gridlyr = NA_plot_lyr, NA_color = "grey",
                             legend_names = list(grid = TeX("$\\rho$ (volcanic forcing, $\\delta^{18}O)$")),
                             allmax = 1) + 
  theme(panel.border = element_blank(), legend.background = element_blank(), axis.text = element_blank(), text = element_text(size = 12), legend.title = element_text(size = 12))

#ISOT

Plot_lyr_sol = CorrMap_Forcing$solar_corr_ens.mean$ISOT
Plot_lyr_sol[CorrMap_Forcing$solar_p_ens.mean$ISOT > 0.1] = NA
Plot_lyr_vol = CorrMap_Forcing$volcanic_corr_ens.mean$ISOT
Plot_lyr_vol[CorrMap_Forcing$volcanic_p_ens.mean$ISOT > 0.1] = NA

Plot_lyr_sol <- rbind(Plot_lyr_sol[49:96,1:73],Plot_lyr_sol[1:48,1:73])
Plot_lyr_vol <- rbind(Plot_lyr_vol[49:96,1:73],Plot_lyr_vol[1:48,1:73])

#Plot

GLOBAL_STACY_OPTIONS$GLOBAL_POINT_SIZE <- 4

NA_plot_lyr = Plot_lyr_sol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1


plot_sol_isot <- STACYmap_NA(gridlyr = Plot_lyr_sol, centercolor = 0, graticules = T,
                        NA_gridlyr = NA_plot_lyr, NA_color = "grey", legend_names = list(grid = TeX("$\\rho$ (solar forcing, $\\delta^{18}O)$")),
                        allmax = 1) +
  theme(panel.border = element_blank(),legend.background = element_blank(), axis.text = element_blank(), text = element_text(size = 12), legend.title = element_text(size = 12))

#plot_sol

NA_plot_lyr = Plot_lyr_vol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1

plot_vol_isot <- STACYmap_NA(gridlyr = Plot_lyr_vol, centercolor = 0, graticules = T,
                        NA_gridlyr = NA_plot_lyr, NA_color = "grey",
                        legend_names = list(grid = TeX("$\\rho$ (volcanic forcing, $\\delta^{18}O)$")),
                        allmax = 1) + 
  theme(panel.border = element_blank(), legend.background = element_blank(), axis.text = element_blank(), text = element_text(size = 12), legend.title = element_text(size = 12))

#plot_vol

library(ggpubr)
plot <- ggarrange(plot_vol_temp, plot_sol_temp,plot_vol_prec, plot_sol_prec,plot_vol_isot, plot_sol_isot,
                  labels = c("(a)", "(b)","(c)", "(d)","(e)", "(f)"),
                  ncol = 3, nrow = 2)

plot  %>% ggsave(filename = paste0('SF_Forcing_Correlation_ensemble-mean.pdf'), plot = ., path = 'Sup_Plots', 
                 width = 3*12, height = 2*12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "pdf")
plot  %>% ggsave(filename = paste0('SF_Forcing_Correlation_ensemble-mean.png'), plot = ., path = 'Sup_Plots', 
                 width = 3*12, height = 2*12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "png")


rm(plot, plot_vol, NA_plot_lyr, plot_sol, Plot_lyr_sol, Plot_lyr_vol, CorrMap_Forcing)

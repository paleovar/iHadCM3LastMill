#################################################
## SF Forcing Correlation
#################################################

library(plyr)
library(dplyr)
library(tidyverse)
library(latex2exp)

load("Data/CorMap_Forcing.RData")
source("Functions/STACYmap_PMIL_NAgrid.R")

Plot_lyr_sol = CorrMap_Forcing$solar_corr_LM1
Plot_lyr_sol[CorrMap_Forcing$solar_p_LM1 > 0.1] = NA
Plot_lyr_vol = CorrMap_Forcing$volcanic_corr_LM1
Plot_lyr_vol[CorrMap_Forcing$volcanic_p_LM1 > 0.1] = NA

Plot_lyr_sol <- rbind(Plot_lyr_sol[49:96,1:73],Plot_lyr_sol[1:48,1:73])
Plot_lyr_vol <- rbind(Plot_lyr_vol[49:96,1:73],Plot_lyr_vol[1:48,1:73])

#Plot

GLOBAL_STACY_OPTIONS$GLOBAL_POINT_SIZE <- 4

NA_plot_lyr = Plot_lyr_sol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1


plot_sol <- STACYmap_NA(gridlyr = Plot_lyr_sol, centercolor = 0, graticules = T,
                         NA_gridlyr = NA_plot_lyr, NA_color = "grey", legend_names = list(grid = TeX("$\\rho$ (solar forcing, $\\delta^{18}O)$")),
                        allmax = 1) +
  theme(panel.border = element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12),
        legend.title = element_text(size = 12))

#plot_sol

NA_plot_lyr = Plot_lyr_vol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1

plot_vol <- STACYmap_NA(gridlyr = Plot_lyr_vol, centercolor = 0, graticules = T,
                         NA_gridlyr = NA_plot_lyr, NA_color = "grey",
                         legend_names = list(grid = TeX("$\\rho$ (volcanic forcing, $\\delta^{18}O)$")),
                        allmax = 1) + 
  theme(panel.border = element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12),
        legend.title = element_text(size = 12))

#plot_vol

library(ggpubr)
plot <- ggarrange(plot_sol, plot_vol,
                  labels = c("(a)", "(b)"),
                  ncol = 2, nrow = 1)

plot  %>% ggsave(filename = paste0('SF_Forcing_Correlation_LM1.pdf'), plot = ., path = 'Sup_Plots', 
                 width = 2*12, height = 12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "pdf")
plot  %>% ggsave(filename = paste0('SF_Forcing_Correlation_LM1.png'), plot = ., path = 'Sup_Plots', 
                 width = 2*12, height = 12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "png")

## Correlation using ensemble mean:

Plot_lyr_sol = CorrMap_Forcing$solar_corr_ens.mean
Plot_lyr_sol[CorrMap_Forcing$solar_p_ens.mean > 0.1] = NA
Plot_lyr_vol = CorrMap_Forcing$volcanic_corr_ens.mean
Plot_lyr_vol[CorrMap_Forcing$volcanic_p_ens.mean > 0.1] = NA

Plot_lyr_sol <- rbind(Plot_lyr_sol[49:96,1:73],Plot_lyr_sol[1:48,1:73])
Plot_lyr_vol <- rbind(Plot_lyr_vol[49:96,1:73],Plot_lyr_vol[1:48,1:73])

#Plot

GLOBAL_STACY_OPTIONS$GLOBAL_POINT_SIZE <- 4

NA_plot_lyr = Plot_lyr_sol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1


plot_sol <- STACYmap_NA(gridlyr = Plot_lyr_sol, centercolor = 0, graticules = T,
                        NA_gridlyr = NA_plot_lyr, NA_color = "grey", legend_names = list(grid = TeX("$\\rho$ (solar forcing, $\\delta^{18}O)$")),
                        allmax = 1) +
  theme(panel.border = element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12),
        legend.title = element_text(size = 12))

#plot_sol

NA_plot_lyr = Plot_lyr_vol
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1

plot_vol <- STACYmap_NA(gridlyr = Plot_lyr_vol, centercolor = 0, graticules = T,
                        NA_gridlyr = NA_plot_lyr, NA_color = "grey",
                        legend_names = list(grid = TeX("$\\rho$ (volcanic forcing, $\\delta^{18}O)$")),
                        allmax = 1) + 
  theme(panel.border = element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12),
        legend.title = element_text(size = 12))

#plot_vol

library(ggpubr)
plot <- ggarrange(plot_sol, plot_vol,
                  labels = c("(a)", "(b)"),
                  ncol = 2, nrow = 1)

plot  %>% ggsave(filename = paste0('SF_Forcing_Correlation_ensemble-mean.pdf'), plot = ., path = 'Sup_Plots', 
                 width = 2*12, height = 12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "pdf")
plot  %>% ggsave(filename = paste0('SF_Forcing_Correlation_ensemble-mean.png'), plot = ., path = 'Sup_Plots', 
                 width = 2*12, height = 12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "png")


rm(plot, plot_vol, NA_plot_lyr, plot_sol, Plot_lyr_sol, Plot_lyr_vol, CorrMap_Forcing)

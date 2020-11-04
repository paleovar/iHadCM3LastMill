## Correlation between the simulation

library(plyr)
library(dplyr)
library(tidyverse)
library(latex2exp)

load("Data/CorMap_IsotopesLM1-3.RData")

source("Functions/STACYmap_PMIL.R")
source("Functions/aw_mean.R")

Plot_lyr = CORR_ISOT$corr_ab
Plot_lyr[CORR_ISOT$corr_ab_p > 0.1] = NA
Plot_lyr <- rbind(Plot_lyr[49:96,1:73],Plot_lyr[1:48,1:73])

NA_plot_lyr = Plot_lyr_ab
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1

plot_ab <- STACYmap_NA(gridlyr = Plot_lyr, centercolor = 0, graticules = T,
                             NA_gridlyr = NA_plot_lyr, NA_color = "grey",
                             legend_names = list(grid = TeX("$\\rho$ ($\\delta^{18}O_{LM1}$, $\\delta^{18}O_{LM2})$")),
                             allmax = 1) + 
  theme(panel.border = element_blank(), legend.background = element_blank(), axis.text = element_blank(), text = element_text(size = 12), legend.title = element_text(size = 12))

#plot_ab

Plot_lyr = CORR_ISOT$corr_ac
Plot_lyr[CORR_ISOT$corr_ac_p > 0.1] = NA
Plot_lyr <- rbind(Plot_lyr[49:96,1:73],Plot_lyr[1:48,1:73])

NA_plot_lyr = Plot_lyr
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1

plot_ac <- STACYmap_NA(gridlyr = Plot_lyr, centercolor = 0, graticules = T,
                       NA_gridlyr = NA_plot_lyr, NA_color = "grey",
                       legend_names = list(grid = TeX("$\\rho$ ($\\delta^{18}O_{LM1}$, $\\delta^{18}O_{LM3})$")),
                       allmax = 1) + 
  theme(panel.border = element_blank(), legend.background = element_blank(), axis.text = element_blank(), text = element_text(size = 12), legend.title = element_text(size = 12))


Plot_lyr = CORR_ISOT$corr_bc
Plot_lyr[CORR_ISOT$corr_bc_p > 0.1] = NA
Plot_lyr <- rbind(Plot_lyr[49:96,1:73],Plot_lyr[1:48,1:73])

NA_plot_lyr = Plot_lyr_ab
NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
NA_plot_lyr[is.na(NA_plot_lyr)] = 1

plot_bc <- STACYmap_NA(gridlyr = Plot_lyr, centercolor = 0, graticules = T,
                       NA_gridlyr = NA_plot_lyr, NA_color = "grey",
                       legend_names = list(grid = TeX("$\\rho$ ($\\delta^{18}O_{LM2}$, $\\delta^{18}O_{LM3})$")),
                       allmax = 1) + 
  theme(panel.border = element_blank(), legend.background = element_blank(), axis.text = element_blank(), text = element_text(size = 12), legend.title = element_text(size = 12))

plot_ab

library(ggpubr)
plot <- ggarrange(plot_ab, plot_ac, plot_bc,
                  labels = c("(a)", "(b)","(c)"),
                  ncol = 3, nrow = 1)

plot  %>% ggsave(filename = paste0('SF_Isotope_Correlation_LM1-3.pdf'), plot = ., path = 'Sup_Plots', 
                 width = 3*12, height = 12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "pdf")
plot  %>% ggsave(filename = paste0('SF_Isotope_Correlation_LM1-3.png'), plot = ., path = 'Sup_Plots', 
                 width = 3*12, height = 12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "png")

No.digits = 3
#Temp Volc
corr <- CORR_ISOT$corr_ab
bstrap <- c()
for (i in 1:1000){
  zonmean = numeric(73)
  lats = seq(from = -90,to = 90,length.out = 73)
  for(lat in 1:73){
    zonmean[lat] = mean(sample(corr[,lat], 96, replace = T), na.rm = T)
  }
  bstrap = c(bstrap, sum(cos(lats*pi/180)/sum(cos(lats*pi/180)))*zonmean,  na.rm = T)
}
print(paste0("AW mean isot-correlation LM1-LM2 c = ", round(simpleawmean(corr), digits = No.digits),
             " (",round(quantile(bstrap,0.05, na.rm = T), digits = No.digits),", ",round(quantile(bstrap,0.95, na.rm = T), digits = No.digits), ")"))

corr <- CORR_ISOT$corr_ac
bstrap <- c()
for (i in 1:1000){
  zonmean = numeric(73)
  lats = seq(from = -90,to = 90,length.out = 73)
  for(lat in 1:73){
    zonmean[lat] = mean(sample(corr[,lat], 96, replace = T), na.rm = T)
  }
  bstrap = c(bstrap, sum(cos(lats*pi/180)/sum(cos(lats*pi/180)))*zonmean,  na.rm = T)
}
print(paste0("AW mean isot-correlation LM1-LM3 c = ", round(simpleawmean(corr), digits = No.digits),
             " (",round(quantile(bstrap,0.05, na.rm = T), digits = No.digits),", ",round(quantile(bstrap,0.95, na.rm = T), digits = No.digits), ")"))

corr <- CORR_ISOT$corr_bc
bstrap <- c()
for (i in 1:1000){
  zonmean = numeric(73)
  lats = seq(from = -90,to = 90,length.out = 73)
  for(lat in 1:73){
    zonmean[lat] = mean(sample(corr[,lat], 96, replace = T), na.rm = T)
  }
  bstrap = c(bstrap, sum(cos(lats*pi/180)/sum(cos(lats*pi/180)))*zonmean,  na.rm = T)
}
print(paste0("AW mean isot-correlation LM2-LM3 c = ", round(simpleawmean(corr), digits = No.digits),
             " (",round(quantile(bstrap,0.05, na.rm = T), digits = No.digits),", ",round(quantile(bstrap,0.95, na.rm = T), digits = No.digits), ")"))



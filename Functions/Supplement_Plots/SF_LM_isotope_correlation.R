## Correlation between the simulation

library(plyr)
library(dplyr)
library(tidyverse)
library(latex2exp)

load("Data/CorMap_ensemblesLM1-3.RData")

source("Functions/STACYmap_PMIL.R")
source("Functions/aw_mean.R")

leg_name <- list(TEMP = list(ab = TeX("$\\rho$ ($T_{LM1}$, $T_{LM2})$"), 
                             ac = TeX("$\\rho$ ($T_{LM1}$, $T_{LM3})$"), 
                             bc = TeX("$\\rho$ ($T_{LM2}$, $T_{LM3})$")),
                 PREC = list(ab = TeX("$\\rho$ ($P_{LM1}$, $P_{LM2})$"), 
                             ac = TeX("$\\rho$ ($P_{LM1}$, $P_{LM3})$"), 
                             bc = TeX("$\\rho$ ($P_{LM2}$, $P_{LM3})$")),
                 ISOT = list(ab = TeX("$\\rho$ ($\\delta^{18}O_{LM1}$, $\\delta^{18}O_{LM2})$"), 
                             ac = TeX("$\\rho$ ($\\delta^{18}O_{LM1}$, $\\delta^{18}O_{LM2})$"),
                             bc = TeX("$\\rho$ ($\\delta^{18}O_{LM1}$, $\\delta^{18}O_{LM2})$")))

for(var in c("TEMP", "PREC", "ISOT")){
  for(comb in c("ab", "ac", "bc")){
    Plot_lyr = CORR_ensemble[[var]][[paste0("corr_", comb)]]
    Plot_lyr[CORR_ensemble[[var]][[paste0("corr_", comb, "_p")]] > 0.1] = NA
    Plot_lyr <- rbind(Plot_lyr[49:96,1:73],Plot_lyr[1:48,1:73])
    
    NA_plot_lyr = Plot_lyr
    NA_plot_lyr[!is.na(NA_plot_lyr)] = 0
    NA_plot_lyr[is.na(NA_plot_lyr)] = 1
    
    name = paste0("plot_", var, "_", comb)
    
    plot <- STACYmap_NA(gridlyr = Plot_lyr, centercolor = 0, graticules = T,
                        NA_gridlyr = NA_plot_lyr, NA_color = "grey",
                        legend_names = list(grid = leg_name[[var]][[comb]]),
                        allmax = 1) +
      theme(panel.border = element_blank(), 
            legend.background = element_blank(), 
            axis.text = element_blank(), 
            text = element_text(size = 12), 
            legend.title = element_text(size = 12))

    assign(name, plot)
  }
}


library(ggpubr)
plot <- ggarrange(plot_TEMP_ab, plot_TEMP_ac, plot_TEMP_bc,
                  plot_PREC_ab, plot_PREC_ac, plot_PREC_bc,
                  plot_ISOT_ab, plot_ISOT_ac, plot_ISOT_bc,
                  labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(h)","(i)","(j)"),
                  ncol = 3, nrow = 3)

plot  %>% ggsave(filename = paste0('SF_Correlation_ensembles_LM1-3.pdf'), plot = ., path = 'Sup_Plots', 
                 width = 3*12, height = 3*12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "pdf")
plot  %>% ggsave(filename = paste0('SF_Correlation_ensembles_LM1-3.png'), plot = ., path = 'Sup_Plots', 
                 width = 3*12, height = 3*12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "png")

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



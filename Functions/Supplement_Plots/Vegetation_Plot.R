library(ncdf4)
library(fields)
source("Functions/STACYmap_PMIL.R")
 
# #HadCM3 Triffid PFTs:
# # 1) Broadleaf tree [Tropical], 2) Needleleaf tree, 3) C3 grass, 4) C4 grass, 5) Shrub, 6) Urban, 7) Inland water, 8) Bare soil, 9) Ice
col_scores = c("#668822", "#117733","#558877", "#88BBAA", "#946846", "#B2B2B2", "#7BAFDE", "#FFDD44","white")

load("Data/LM_HadCM3_JuneVegetation.RData")

plot <- STACYmap(gridlyr = rbind(DATA_VEGETATION[49:96,1:73],DATA_VEGETATION[1:48,1:73]),
                 graticules = TRUE,colorscheme = col_scores,
                 legend_cb = F, legend_num_breaks = 9, legend_names = list(grid = "PFT")) +
  theme(panel.border = element_blank(), legend.background = element_blank(), axis.text = element_blank(), legend.text = element_text(size = 12)) 

plot %>% ggsave(filename = paste0('SF_Vegetation_June.pdf'), plot = ., path = 'Sup_Plots', 
                width = 2*12, height = 2*12/8.3*PLOTTING_VARIABLES$HEIGHT, units = 'cm', dpi = 'print', device = "pdf")

rm(DATA_VEGETATION, col_scores)

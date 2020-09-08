# iHadCM3LastMill

This github repository provides the code to reproduce plots from Bühler, J., Roesch, C., Kirschner, M., Sime, L., Holloway, M. and Rehfeld, K.: Comparison of the oxygen isotope signatures in speleothem records and iHadCM3 model simulations for the last millennium, submitted to Clim. Past. Discuss. (2020).

Authors:
Kira Rehfeld, Janica C. Bühler, Louise Sime, Max D. Holloway

Contact:  Janica C. Bühler, jbuehler@iup.uni-heidelberg.de, 
          Kira Rehfeld, krehfeld@iup.uni-heidelberg.de

Description:
Here we provide code and time series of three isotope-enabled model simulations of the iHadCM3 Model in supplement to Bühler et al. (2020, submitted to Clim. Past. Discuss.) to reproduce all plots (including supplement plots). Simulation output was extracted at SISAL v.2. sites for the last millennium (810-1850CE). 

Comment:
Details on the model simulation and data are provided at PANGAEA (www.pangaea.de). We provide the output of three ensemble members ('xnapa', 'xnapb' and 'xnapc') extracted for cave locations within the SISAL v.2. database (https://researchdata.reading.ac.uk/256/, Comas-Bru et al. (2020)). We include output for sites that pass the resolution and dating screening, meaning that have at least 2 radiometric dates (or are lamina-counted) and provide ten oxygen isotope ratio measurements within the last millennium. See the README.txt files for more details. 


References:
Bühler, J., Roesch, C., Kirschner, M., Sime, L., Holloway, M. and Rehfeld, K.: Comparison of the oxygen isotope signatures in speleothem records and iHadCM3 model simulations for the last millennium, submitted to Clim. Past. Discuss. (2020).

Comas-Bru, L., Rehfeld, K., Roesch, C., Amirnezhad-Mozhdehi, S., Harrison, S. P., Atsawawaranunt, K., Ahmad, S. M., Ait Brahim, Y., Baker, A., Bosomworth, M., Breitenbach, S. F. M., Burstyn, Y., Columbu, A., Deininger, M., Demény, A., Dixon, B., Fohlmeister, J., Hatvani, I. G., Hu, J., Kaushal, N., Kern, Z., Labuhn, I., Lechleitner, F. A., Lorrey, A., Martrat, B., Novello, V. F., Oster, J., Pérez-Mejías, C., Scholz, D., Scroxton, N., Sinha, N., Ward, B. M., Warken, S., Zhang, H., and the SISAL members: SISALv2: A comprehensive speleothem isotope database with multiple age-depth models, Earth Syst. Sci. Data Discuss., https://doi.org/10.5194/essd-2020-39, in review, 2020. 

Tindall, J. C., P. J. Valdes, and Louise C. Sime: Stable water isotopes in HadCM3: Isotopic signature of El Niño–Southern Oscillation and the tropical amount effect. J. Geophys. Res.: Atm. 114.D4 (2009).

# How to:

0:  all needed data is provided in the "Data" folder, all functions used are provided in the "Functions" folder, all libraries needed to run scripts are stated.
    
1: execute 1_read_dataset.R to read in all data that is required to plot.

2: execute line 1-70 of 2_paper_plots.R to set the basis for all analysis. After that the plots are numbered from 1-9 and can be performed separately. The analysis for the plot is provided in the respective section and needs to be run before plotting. 

3: For supplement plots, run the respective section in 3_supplement_plots.R (be sure to have executed all analyses from 2_paper_plots.R first). 

# iHadCM3LastMill

Code to reproduce all plots from Bühler, J., Roesch, C., Kirschner, M., Sime, L., Holloway, M. and Rehfeld, K.: Comparison of the oxygen isotope signatures in speleothem records and iHadCM3 model simulations for the last millennium, submitted to Clim. Past. Discuss. (2020).

Authors:
Kira Rehfeld, Janica C. Bühler, Louise Sime, Max D. Holloway

Contact:  Janica C. Bühler, jbuehler@iup.uni-heidelberg.de, 
          Kira Rehfeld, krehfeld@iup.uni-heidelberg.de

Description:
Here we provide code and time series of three isotope-enabled model simulations of the iHadCM3 Model in supplement to Bühler et al. (2020, submitted to Clim. Past. Discuss.) to reproduce all plots (including supplement plots). Simulation output was extracted at SISAL v.2. sites for the last millennium (810-1850CE). The spatial resolution in the atmosphere is 2.5 by 3.75 degrees in the horizontal with 19 vertical levels, and 1.25 by 1.25 degrees in the ocean, with 20 irregularly spaced vertical levels. Details of the isotope-enabled model extension can be found in Tindall et al. (2009). The model simulations were performed with constant orbital forcing, and time-varying prescribed land cover changes, solar irradiance, volcanic aerosol optical depth and greenhouse gases (CO2, CH4, NO2). Note that changes in greenhouse gas forcings occur 50 years earlier than other forcings, which should not cause issues in interpretation due to the absence of large trends prior to 1800. We provide output for surface temperature, total precipitation and the precipitation-weighted oxygen isotope ratio extracted by bilinear interpolation at the cave site. To facilitate comparisons we provide both the temporally aggregated output (i.e. model output aggregated to the same temporal resolution as the speleothem data) as well as annual means. 

Comment:
These files contain the output of three ensemble members ('xnapa', 'xnapb' and 'xnapc') extracted for cave locations within the SISAL v.2. database (https://researchdata.reading.ac.uk/256/, Comas-Bru et al. (2020)). We include output for sites that pass the resolution and dating screening, meaning that have at least 2 radiometric dates (or are lamina-counted) and provide ten oxygen isotope ratio measurements within the last millennium. See the README.txt files for more details. 


References:
Bühler, J., Roesch, C., Kirschner, M., Sime, L., Holloway, M. and Rehfeld, K.: Comparison of the oxygen isotope signatures in speleothem records and iHadCM3 model simulations for the last millennium, submitted to Clim. Past. Discuss. (2020).

Comas-Bru, L., Rehfeld, K., Roesch, C., Amirnezhad-Mozhdehi, S., Harrison, S. P., Atsawawaranunt, K., Ahmad, S. M., Ait Brahim, Y., Baker, A., Bosomworth, M., Breitenbach, S. F. M., Burstyn, Y., Columbu, A., Deininger, M., Demény, A., Dixon, B., Fohlmeister, J., Hatvani, I. G., Hu, J., Kaushal, N., Kern, Z., Labuhn, I., Lechleitner, F. A., Lorrey, A., Martrat, B., Novello, V. F., Oster, J., Pérez-Mejías, C., Scholz, D., Scroxton, N., Sinha, N., Ward, B. M., Warken, S., Zhang, H., and the SISAL members: SISALv2: A comprehensive speleothem isotope database with multiple age-depth models, Earth Syst. Sci. Data Discuss., https://doi.org/10.5194/essd-2020-39, in review, 2020. 

Tindall, J. C., P. J. Valdes, and Louise C. Sime: Stable water isotopes in HadCM3: Isotopic signature of El Niño–Southern Oscillation and the tropical amount effect. J. Geophys. Res.: Atm. 114.D4 (2009).

# How to:

0:  all needed data is provided in the "Data" folder, all functions used are provided in the "Functions" folder, all libraries needed to run scripts are stated 
    
1: execute 1_read_dataset.R to read in all data that is required to plot

2: execute line 1-70 of 2_paper_plots.R to set the basis for all analysis. After that the plots are numbered from 1-9 and can be performed separately. The analysis for the plot is provided in the respective section and needs to be run before plotting. 

3: For supplement plots, run the respective section in 3_supplement_plots.R (be sure to have executed all analyses from 2_paper_plots.R first). 

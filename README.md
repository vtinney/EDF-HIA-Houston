
# Houston local health impact assessment of air pollution
### Environmental Defense Fund (EDF) and the George Washington University (GWU) Milken Institute School of Public Health, Department of Environmental and Occupational Health.

Contact for this repository: Veronica Southerland, vtinney@gwu.edu

This repository provides the base code for the health impact calculations for the Houston health impact assessment for fine particulate matter (PM2.5), nitrogen dioxide (NO2) and black carbon (BC). 

The framework for the code is as follows:

* **Health impact calculations**. Code should be run in the following order:

  * Houston_01.*concentration*.estimates.R - These files provide the health impact function calculation and maps, run in R/3.5.3. Within the code, results are aggregated to the census block group (CBG) and county level, with results exported to a dataframe (csv).
  
  * Houston_02.*concentration*.df.R - These files first extract summary statistics from all output rasters created in Houston_01 files. They then bring in the output csv files created in Houston_01 and clean the data.
  
  * Houston_03.*concentration*.long.df.R - These files convert the csv files created in Houston_02, further format the data, and spread the estimates across columns, such that they are more easily input into tables.
  
 * **Input files**
   * All files to run the code in steps Houston_01-03 is available in the inputs file folder. To run the code, the user needs to download all input files and change the working directories within the code.
   * pop.ls.night.17.ho.tif -	Population: LandScan USA 2017 (100m) with GPW v4 ages 0-17 years (1km)	counts	100m	Houston counties
   * pop.ls.night.25.ho.tif	- Population: LandScan USA 2017 (100m) with GPW v4 ages 25-99 years (1km)	counts	100m	Houston counties
   * pop.ls.night.65.ho.tif	- Population: LandScan USA 2017 (100m) with GPW v4 ages 65-99 years (1km)	counts	100m	Houston counties
   * pop.ls.night.ho.tif - Population: LandScan USA 2017 (100m) all ages	counts	100m	Houston counties
   * conc.no2.gsv.med.tif - Concentration:	GSV NO2 – point estimate (median of drive passes)	ppb	100m	GSV drive areas
   * conc.no2.gsv.lower.tif - Concentration:	GSV NO2 – lower confidence interval	ppb	100m	GSV drive areas
   * conc.no2.gsv.upper.tif - Concentration:	GSV NO2 – upper confidence interval	ppb	100m	GSV drive areas
   * conc.gsv.bc.med.tif - Concentration:	GSV BC – point estimate (median of drive passes)	ug/m3	100m	GSV drive areas
   * conc.gsv.bc.lower.tif - Concentration:	GSV BC – lower confidence interval	ug/m3	100m	GSV drive areas
   * conc.gsv.bc.upper.tif - Concentration:	GSV BC – upper confidence interval	ug/m3	100m	GSV drive areas
   * conc.lark.ho.tif - Concentration	NO2: Larkin et al. 2017	ppb	100m	Houston counties
   * conc.pm.mean.13.15.tif - Concentration	PM2.5: Di et al. 2016, mean of 2013 to 2015	ug/m3	100m	Houston counties
   * conc.vd.pm.2016.tif	 - Concentration PM2.5: van Donkelaar et al. 2016	ug/m3	100m	Houston counties
   * conc.bc.ho.tif	- Concentration BC: van Donkelaar et al. 2019	ug/m3	100m	Houston counties
   * ho.all.25.tif - Rate:	All-cause mortality, ages 25-99 years, age adjusted for the year 2016 from CDC Wonder, County level	Rate per 10,000	100m	Houston counties
   * ho.all.65.tif - Rate:	All-cause mortality, ages 65-99 years, age adjusted for the year 2016 from CDC Wonder, County level	Rate per 10,000	100m	Houston counties
   * ho.cbg.25.tif - Rate:	All-cause mortality, ages 25-99 years, crude rate per CBG from Kai Zhang, seven-year average numerators	Rate per 10,000	100m	Houston counties
   * ho.cbg.65.tif - Rate:	All-cause mortality, ages 65-99 years, per CBG from Kai Zhang, seven-year average numerators	Rate per 10,000	100m	Houston counties
   * ho.cbg.cvd.25.tif - Rate:	CVD mortality, ages 25-99 years, per CBG from Kai Zhang, seven-year average numerators	Rate per 10,000	100m	Houston counties
   * ho.cbg.cvd.65.tif - Rate:	CVD mortality, ages 25-99 years, per CBG from Kai Zhang, seven-year average numerators	Rate per 10,000	100m	Houston counties
   * ho.cbg.cvd.ha.tif - Rate:	CVD hospitalizations, ages 65-99 years, per CBG from Kai Zhang, seven-year average numerators	Rate per 10,000	100m	Houston counties
   * ho.cvd.25.tif - Rate:	CVD mortality, ages 25-99 years, age adjusted for the year 2016 from CDC Wonder, County level	Rate per 10,000	100m	Houston counties
   * ho.cvd.65.tif - Rate:	CVD mortality, ages 65-99 years, age adjusted for the year 2016 from CDC Wonder, County level	Rate per 10,000	100m	Houston counties
   * ho.inc.17.tif - Rate:	Asthma incidence from Wendt et al. 2014, 312 per 10,000 for the Houston area	Rate per 10,000	100m	Houston counties
   * bc_gsv_drives_100m.shp - Extent:	GSV drive areas for BC	n/a	100m grid shapefile	GSV drive areas
   * cbg_houston.shp	 - Extent: Houston CBGs	n/a	CBG	Houston counties
   * gsv_no2_100m_grid.shp - Extent:	GSV drive areas for NO2	n/a	100m grid shapefile	GSV drive areas
   * houston_co_1984.shp - Extent:	Houston 9 counties	n/a	County	Houston counties

 
 * **Intermediary Files** (IF)
    * IF files are provided for informational use only. Contact Veronica Southerland (vtinney@gwu.edu) for questions on generating intermediary files.
    
    * Houston_concentration_files.R - creates median and minimum concentration files using the concentration files provided in the inputs folder.
    * Houston_concentration_maps.R - maps the concentration raster files.
    * Houston_population_total.R - aggregates Houston population from input rasters.
    * calculate_gsv_drive_cb_overlap.R - calculates spatial overlap between census blocks and GSV drive areas.
    * houston_gdalwarp.txt - warps all files using GDAL to match input files.
    * mean_di_pm_rasters.R - creates a mean concentration file from Di et al. 2019 concentration files for years 2013 through 2015.




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
 
 * **Intermediary Files** (IF)
    * IF files are provided for informational use only. Contact Veronica Southerland (vtinney@gwu.edu) for questions on generating intermediary files.




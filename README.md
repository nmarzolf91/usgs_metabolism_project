# usgs_metabolism_project

This dataset is companion with the github repository https://github.com/nmarzolf91/usgs_metabolism_project that accesses USGS water quality and hydrologic data to estimate ecosystem metabolism in 59 rivers across the US from 2007-2021. Contained in this data citation are information about sites, raw time-series data at sub-daily intervals, prepared time series for modeling metabolism, metabolism outputs in raw form, processed and QAQC'd metabolism estimates, and supplementary information for the associated manuscript(s).


## Description of the data and file structure

All data are held in the data/ folder, which is gitignore'd to keep the file size down, but the code will access and create the file directory. The /data_citation folder represents what is published on Dryad, and contains the data to perform the analysis, from raw data accessed from USGS to cleaned data with metabolism estimates. 

All code are organized into subdirectories that explain what each script performs (eg., get data, prepare data, model data, evaluate model outputs, etc.). The /functions directory contains custom functions that are useful in the analysis and are sourced into the script where necessary.

Files can be looked up using site codes (nwis_) and by date (yyyy-mm-dd), which are consistent throughout the project.


## Sharing/Access information

Code and some data available at https://github.com/nmarzolf91/usgs_metabolism_project
Data available on dryad at: 


## Code/Software

R version 4.2.3 with various packages, including streamMetabolizer v0.12.0 and dataRetrieval v2.7.12
---
editor_options: 
  markdown: 
    wrap: 72
---

# Title of dataset: Estimates of ecosystem metabolism for 59 rivers in North America, 2008-2021

This dataset is companion with the github repository
<https://github.com/nmarzolf91/usgs_metabolism_project> that accesses
USGS water quality and hydrologic data to estimate ecosystem metabolism
in 59 rivers across the US from 2007-2021. Contained in this data
citation are information about sites, raw time-series data at sub-daily
intervals, prepared time series for modeling metabolism, metabolism
outputs in raw form, processed and QAQC'd metabolism estimates, and
supplementary information for the associated manuscript(s).

## Description of the data and file structure

-   1_site_info.csv Number of variables: 5 Variable list: Site Number:
    USGS code for gage station Name: USGS name for site Latitude: site
    latitude, in dd.ddd Longitude: site longitude, in dd.ddd Drainage
    Areak (km2): drainage area upstream of the gage station, in sq. km

-   2_timeseries_raw.zip In this folder are two sources of the sensor
    data. Data collected from 2007-2016 are stored in
    usgs_from_streampulse_old, and data collected from 2017-2021 are
    stored in one csv file, usgs_data_2017-2021. The type of data are
    exactly the same, but the difference is due to who and when the data
    were aggregated and how the data were accessed, described in the
    manuscript for this dataset.

    -   2_timeseries_raw/usgs_data_2017_2021.csv : high-frequency river
        data from USGS NWIS sites, collected 2017-2021.

        -   Number of variables: 6

        -   Variable list:

            -   ...1: row number index site: USGS site code

            -   DateTime: Timestamp of data collected

            -   disch: river discharge, in cfs

            -   wtr: water temperature, in Celsius

            -   doobs: dissolved oxygen concentration, in mg/L

    -   2_usgs_from_streampulse_old/: folder containing high-frequency
        river data from USGS NWIS sites, collected 2007 -- 2016

        -   Number of files: 63

        -   Naming convention:
            nwis\_{site_code}\_{startDate}\_{endDate}.csv, where each
            file contains the entire record of data for a given site
            from 2007-2016

        -   Number of variables in each file: 7

        -   Variable list:

            -   DateTime_UTC: Timestamp of each measurement, in UTC

            -   Region: Two character string for region description in
                StreamPulse portal

            -   Site: USGS site code

            -   Value: value of variable defined in variable column

            -   Variable: one of DO_mgL, Depth_m, Discharge_m3s,
                Light_PAR, WaterTemp_C, satDO_mgL

            -   Flagtype: indicates potentially bad data, identified
                visually

            -   Flagcomment: additional notes describing why a flagtype
                was added

-   3_usgs_sm_ready_all: this folder contains the merged form of the
    data in 2_timeseries_raw and modified to be directly input into
    streamMetabolizer models using the required format

    -   Number of files: 59

    -   Naming convention:
        nwis\_{site_code}\_{startDate}\_{endDate}.csv, here each file
        contains the entire record of data for a given site from
        2007-2021

    -   Number of variables in each file: 8

        -   site: nwis\_{sitecode}

        -   solar.time: timestamp of measurement, in solar time UTC

        -   temp.water: water temperature, in C

        -   DO.obs: dissolved oxygen, mg/L

        -   DO.sat: dissolved oxygen at saturation, mg/L

        -   depth: mean depth of the study reach, in m

        -   discharge: discharge, in m3/s

-   4_model_run_bayes: this directory contains the raw model outputs
    from streamMetabolizer, storing various outputs of the model in
    separate directories. The estimates, error, confidence intervals,
    Bayesian measures of model convergence of daily GPP, ER, and K600
    are available in daily/, model-wide fit, predicted DO
    concentrations, gas exchange scaling relationships, and diagnostics
    of models are available in this directory. Processed and usable
    forms of the data are presented in data citation
    5_usgs_metabolism.csv, and the relevant parameters are described in
    detail below.

    -   daily

        -   Number of files: 854

        -   Naming convention:
            nwis\_{sitecode}\_{startdate}*\_*{enddate}\_daily.csv

        -   Number of variables in each file: 74

    -   datadaily

        -   Number of files: 853

        -   Naming convention:
            nwis\_{sitecode}\_{startdate}\_{enddate}\_datadaily.csv

        -   Number of variables in each file: 7

    -   estimates

        -   Number of files: 845

        -   Naming convention: nwis\_{sitecode}\_year.csv

        -   Number of variables in each file: 26

    -   KQ_overall

        -   Number of files: 853

        -   Naming convention:
            nwis\_{sitecode}\_{startdate}\_{enddate}*\_*KQ_overall.csv

        -   Number of variables in each file: 13

    -   mod_and_obs_DO

        -   Number of files: 845

        -   Naming convention:
            nwis\_{sitecode}\_{startdate}\_{enddate}\_mod_and_obs_DO.csv

        -   Number of variables in each file: 9

    -   overall

        -   Number of files: 853

        -   Naming convention:
            nwis\_{sitecode}\_{startdate}\_{enddate}\_overall.csv

        -   Number of variables: 33

    -   specs

        -   Number of files: 853

        -   Naming convention:
            nwis\_{sitecode}\_{startdate}\_{enddate}\_specs.csv

        -   Number of variables: 1

    -   diagnostics.csv: For each site year, descriptive statistics that
        inform how well the model fit.

        -   Number of variables: 13

            -   site

            -   year

            -   n_days: number of days with estimates

            -   f_days: n_days/# of days in the year

            -   resolution: sampling frequency of the input sensor data

            -   K600_daily_sigma_Rhat: Gelman-Rubin convergence
                statistics for K600

            -   err_obs_iid_sigma_Rhat: observation error convergence
                statistic

            -   err_proc_iid_sigma_Rhat: process error convergence
                statistics

            -   K_median: median daily gas exchange

            -   K_range: range of gas exchange estimated for the given
                site year

            -   neg_GPP: % days with negative GPP estimates (\<-0.5 g O2
                m-2 d-1)

            -   pos_ER: % days with positive ER estimates (\>0.5 g O2
                m-2 d-1)

            -   ER_K\_r2: linear R2 between daily ER and K600, where
                high correlation between variables suggest poor model
                convergence

-   5_usgs_metabolism.csv

    -   Number of variables: 26

    -   Variable list:

        -   site: usgs sitecode

        -   resolution: resolution of sensor data

        -   date: date of estimate

        -   GPP: median GPP estimate

        -   GPP.lower: 2.5%-ile GPP estimate

        -   GPP.upper: 97.5%-ile GPP estimate

        -   GPP.n_eff: number of effective samples to estimate GPP

        -   GPP.Rhat: Gelman-Rubin convergence statistic for GPP

        -   ER: median ER estimate

        -   ER.lower: 2.5%-ile ER estimate

        -   ER.upper: 97.5%-ile GPP estimate

        -   ER.n_eff: number of effective samples to estimate ER

        -   ER.Rhat: Gelman-Rubin convergence statistic for ER

        -   K600: median: K600 estimate

        -   K600.lower: 2.5%-ile K600 estimate

        -   K600.upper: 97.5%-ile K600 estimate

        -   K600.n_eff: number of effective samples to estimate K600

        -   K600.Rhat: Gelman-Rubin convergence statistic for ER

        -   DO.obs: mean daily DO

        -   DO.sat: mean daily DO at saturation

        -   DO.amp: daily amplitude of DO for the

        -   DO.psat: mean daily %DO saturation

        -   depth: mean daily depth

        -   temp.water: mean daily temperature

        -   discharge: mean daily discharge

        -   shortwave: total daily incident shortwave radiation

-   6_fluxnet_metabolism.csv

    -   Number of variables: 12

    -   Variable list:

        -   site: Fluxnet site name

        -   biome: Fluxnet listed biome

        -   date: date of estimate

        -   year: year

        -   DOY: Julian Day

        -   GPP: GPP estimate (g C m-2 d-1)

        -   ER: ER estimate (g C m-2 d-1)

        -   Net: GPP - ER (g C m-2 d-1)

        -   Temp: temperature, C

        -   Precip: precipitation, mm

        -   VPD: vapor pressure deficit, hPa

        -   SW: shortwave radiation, W m-2

-   7_river_light.csv

    -   Number of variables: 5

    -   Variable list

        -   site: nwis\_{sitecode}

        -   date

        -   daily_SW_inc: total daily incoming shortwave radiation

        -   daily_LAI: daily leaf area index

        -   daily_PAR: total daily photosynthetically active radiation
            incident at the river surface

## Sharing/Access information

Code and some data available at
<https://github.com/nmarzolf91/usgs_metabolism_project> Data available
on dryad at: <https://doi.org/10.5061/dryad.bcc2fqzj2>

## Code/Software

R version 4.2.3 with various packages, including streamMetabolizer
v0.12.0 and dataRetrieval v2.7.12


<!-- README.md is generated from README.Rmd. Please edit that file -->

# README

<!-- badges: start -->
<!-- badges: end -->

In this R project, all the scripts for the manuscript *Plankton blooms
over the annual cycle shape trophic interactions under climate change*
are available.

All the analyses were done with R version 4.1.1 (2021-08-10) on the
platform x86_64-apple-darwin17.0.

Data and scripts are also stored on Zenodo
<https://zenodo.org/records/10708263>

# R scripts

``` r
# 1. Download the data
source("Scripts/DownloadData.R")

# 2. Calculate the phenology metrics and the phyisical variables
source("Scripts/PhenologyZP.R")
source("Scripts/PhenologyPP.R")
source("Scripts/Physical.R")

# 3. Run the analysis
source("Scripts/PlanktonSuccession.R")
source("Scripts/Trends.R")
source("Scripts/GLM.R")

# 4. Run the supplementary analyses
source("Scripts/DifferenceStations.R")
source("Scripts/Nauplii.R")
source("Scripts/FreqPhytoProp.R")
source("Scripts/Validation_Trends.R")
```

### DownloadData.R

This script can be used to download the phytoplankton and zooplankton
time series data as well as the abiotic parameter.

### PhenologyZP.R

In this script the *Zooplankton metrics* are calculated:

- Imported raw abundance data from
  [Sharkweb](https://sharkweb.smhi.se/hamta-data/)[^1]
- Biomass was calculated from individual bodymass based on location,
  life stage and sex according to the [HELCOM COMBINE
  manual](https://helcom.fi/action-areas/monitoring-and-assessment/monitoring-guidelines/combine-manual/)
- Species were filtered to keep only the one with a good data coverage
  and excluding Nauplii
- Density plot were performed based on the daily interpolation with a
  kernel smoothing
- Weekly interpolation were made over the full time series with the
  Rpackage *zoo* and Week were retrieved using the package *lubridate*
- Metric were calculated
  - Peak timing using the center of gravity method
  - Duration of the bloom using the cumulative biomass between the
    25<sup>th</sup> and 75<sup>th</sup> percentile
  - Magnitude of the bloom using the average of the biomass between the
    start and the end of the bloom
  - A final dataset was saved combining all metric together
    **“Output/Data/Weekly_Full_ZP.txt”**

### PhenologyPP.R

In this script the *Phytoplankton metrics* are calculated:

- Imported raw biomass data from
  [Sharkweb](https://sharkweb.smhi.se/hamta-data/)[^2]
- Species were filtered to keep only the ones with a good data coverage
  and exclude heterotroph organisms
- Density plot were performed based on the daily interpolation with a
  kernel smoothing
- Spring bloom was the sum of the phytoplankton between Week 5 and 2
- Summer bloom was the sum of the phytoplankton between Week 22 and 37
- Weekly interpolation were made over the full time series with the
  Rpackage *zoo* and DOY were retrieved using the package *lubridate*
- Metric were calculated
  - Peak timing using the center of gravity method
  - Duration of the bloom using the cumulative biomass between the
    25<sup>th</sup> and 75<sup>th</sup> percentile
  - Magnitude of the bloom using the average of the biomass between the
    start and the end of the bloom
- A final dataset was saved combining all metric together
  **“Output/Data/Weekly_Full_PP.txt”**

### Physical.R

In this script the *Abiotic factor* are calculated:

- Imported raw data from
  [Sharkweb](https://sharkweb.smhi.se/hamta-data/)[^3]
- Weekly interpolation were made over the full time series with the
  Rpackage *zoo* and DOY were retrieved using the package *lubridate*
- Monthly average are saved as **“Output/Data/Abiotic.txt”**

### PlanktonSuccession.R

In this script the **Fig. 1b** and **Fig. 3** are made:

- Imported metrics and abiotic factor from
  - **“Output/Data/Weekly_Full_ZP.txt”**
  - **“Output/Data/Weekly_Full_PP.txt”**
  - **“Output/Data/Abiotic.txt”**
- Panel a shows the water temperature between 0 to 20m for each month at
  each station (average and range min-max)
- Panel b shows the salinity between 0 to 20m for each month at each
  station (average and range min-max)
- Panels c and d show total and taxa specific dynamics
  - The peak timing (average between 2008 and 2022)
  - The start and end of the bloom (average between 2008 and 2022)
- A map is also plotted at the end of the script

### Trends.R

In this script the *Trends* are analysed:

- Imported data from
  - **“Output/Data/Weekly_Full_ZP.txt”**
  - **“Output/Data/Weekly_Full_PP.txt”**
  - **“Output/Data/Abiotic.txt”**
- Trends are analysed with the Kendall test from the *Kendall* package
  and effect size was assessed using the Sen’s slope from the *mblm*
  package for phytoplankton, zooplankton and seasonal salinity and
  temperature:
  - Peak timing
  - Peak initation
  - Peak termination
  - Peak magnitude
- Coefficients of the trends for the phenological metrics are shown in
  **Fig. 4** and **Fig. S3**
- z-scores of the peak timing and magnitudes were calculated for all
  taxa and visualized across the year in **Fig. 4** and **Fig. S3**
- Coefficients of the trends and dynamics across the years for the
  abiotic parameters are shown in **Fig. 2** and **Fig. S2**

### GLM.R

In this script the Generalized Linear Models are made:

- Imported data from
  - **“Output/Data/Weekly_Full_ZP.txt”**
  - **“Output/Data/Weekly_Full_PP.txt”**
  - **“Output/Data/Abiotic.txt”**
- Output of the GLM models are shown in **Fig. 5** and **Table S4**

### Nauplii.R

In this script **Fig. S5** is produced showing seasonal abundance
dynamics of *Acartia*, *Pseudocalanus*, *Temora* and *Centropages*
nauplii stages are plotted

### FreqPhytoProp.R

In this script the **Fig. S1** and **Fig. 1** are produced:

- For all three stations, based on:
  - Zooplankton sampling
  - Phytoplankton sampling
  - Temperature sampling
- Density plot showing the total phytoplankton and zooplankton biomass
  dynamics and the contribution of each taxa

### Validation_Trends.R

In this script **Fig. S6** is produced:

- We subsampled phytoplankton and zooplankton to have a similar temporal
  resolution than at BY15 and BY5
- We weekly interpolated the data and calculated the phenology metrics
  for all taxa at BY31
- We compared the subsampled data with the complete dataset

------------------------------------------------------------------------

# Data

First, the **raw data** were downloaded and are publicly available:

- Manually by going on [Sharkweb](https://sharkweb.smhi.se/hamta-data/)
- Using the shaRk package and the script `DownloadData`

Note that data were downloaded using the
[shaRk](https://github.com/andreasnovotny/shaRk) Rpackage for this
analysis. If the data are downloaded throughout the
[Sharkweb](https://sharkweb.smhi.se/hamta-data/), make sure to update
the headers accordingly.

The zooplankton bodymass were retrived from the [HELCOM COMBINE
manual](https://helcom.fi/action-areas/monitoring-and-assessment/monitoring-guidelines/combine-manual/),
are shown in the Table S1 of the manuscript, and should have this
structure to be compatible with the scripts:

    #> 'data.frame':    132 obs. of  8 variables:
    #>  $ Taxa   : chr  "Temora" "Temora" "Temora" "Temora" ...
    #>  $ STAGE  : chr  "AD" "AD" "AD" "C1" ...
    #>  $ SEXCO  : chr  "F" "M" "NS" "NS" ...
    #>  $ Station: chr  "BY31" "BY31" "BY31" "BY31" ...
    #>  $ Jan.Mar: num  60 55 57.5 9 18 60 55 57.5 9 18 ...
    #>  $ Apr.Jun: num  65 40 52.5 6 15 65 55 60 6 15 ...
    #>  $ Jul.Sep: num  60 45 52.5 6 15 40 45 42.5 6 15 ...
    #>  $ Oct.Dec: num  50 50 50 4 14 50 45 47.5 6 15 ...

Where:

- `Taxa` corresponds to the Taxa of interest
- `STAGE` corresponds to the Stage of the organism (`AD`, `C1`, `C4`,
  `JV` or `NS`)
- `SEXCO` corresponds to the sex code of the organism (`F`, `M`, `NS`)
- `Station`corresponds to the station (`BY31`, `BY15` or `BY5`)
- `Jan.Mar` corresponds to the bodymass in Winter
- `Apr.Jun` corresponds to the bodymass in Spring
- `Jul.Sep` corresponds to the bodymass in Summer
- `Oct.Dec` corresponds to the bodymass in Autumn

## Processed data

In the folder **Output/Data** all the needed data can be found.

1.  Weekly_Full_ZP.txt
2.  Weekly_Full_PP.txt
3.  Abiotic.txt

## Weekly_Full_ZP.txt

This dataset is produced at the end of the script `Phenology_ZP.R`

It has this structure:

    #> 'data.frame':    315 obs. of  11 variables:
    #>  $ Taxa     : chr  "Acartia" "Acartia" "Acartia" "Acartia" ...
    #>  $ Year     : int  2008 2008 2008 2009 2009 2009 2010 2010 2010 2011 ...
    #>  $ Station  : chr  "BY15" "BY31" "BY5" "BY15" ...
    #>  $ Group    : chr  "Copepoda" "Copepoda" "Copepoda" "Copepoda" ...
    #>  $ Timing   : int  21 26 24 29 32 27 32 34 29 26 ...
    #>  $ Maximum  : num  84.3 118.4 36.4 78.9 36.6 ...
    #>  $ Magnitude: num  52 66.8 24.6 38 18.6 ...
    #>  $ Start    : num  9 20 11.5 20 22.5 ...
    #>  $ End      : num  28.3 33.5 35.5 34.8 40.7 ...
    #>  $ Middle   : num  21 24.2 24.5 30 34 ...
    #>  $ Duration : num  19.3 13.5 24 14.8 18.2 ...

Where:

- `Taxa`: Mesozooplankton taxa: *character*
- `Year`: Year between 2008 and 2022, *integer*
- `Station`: the three stations BY31, BY15, and BY5, *character*
- `Group`: Group of mesozooplankton taxa: Copepoda, Cladocera, and
  Rotatoria, *character*
- `Timing`: Bloom peak timing (Week \#), *numeric*
- `Maximum`: Bloom magnitude maximum ($\mu$g L<sup>-1</sup>), *numeric*
- `Magnitude`: Bloom magnitude ($\mu$g L<sup>-1</sup>), *numeric*
- `Start`: Start of the growing season, correspond to the
  25<sup>th</sup> percentile of the cumulative biomass (Week \#),
  *numeric*
- `End`: End of the growing season, correspond to the 75<sup>th</sup>
  percentile of the cumulative biomass (Week \#), *numeric*
- `Middle`: Middle of the growing season, correspond to the
  50<sup>th</sup> percentile of the cumulative biomass (Week \#),
  *numeric*
- `Duration`: Time span between the Start and the End of the growing
  season (Week \#), *numeric*

## Weekly_Full_PP.txt

This dataset is produced at the end of the script `Phenology_PP.R`

It has this structure:

    #> 'data.frame':    404 obs. of  11 variables:
    #>  $ Taxa     : chr  "Cyanobacteria" "Cyanobacteria" "Cyanobacteria" "Dinoflagellates" ...
    #>  $ Year     : int  2008 2008 2008 2008 2008 2008 2008 2008 2008 2008 ...
    #>  $ Station  : chr  "BY15" "BY31" "BY5" "BY15" ...
    #>  $ Group    : chr  "Phytoplankton" "Phytoplankton" "Phytoplankton" "Phytoplankton" ...
    #>  $ Timing   : int  26 31 30 17 17 19 25 26 24 21 ...
    #>  $ Maximum  : num  60.3 14.3 117 67.7 43.4 ...
    #>  $ Magnitude: num  55.66 8.44 91.21 59.57 30.2 ...
    #>  $ Start    : num  23.7 24.8 27.6 14.5 14.8 ...
    #>  $ End      : num  29 34.2 30.9 19.1 18 ...
    #>  $ Middle   : num  26.4 29.8 29.5 16.7 16.6 ...
    #>  $ Duration : num  5.33 9.45 3.25 4.6 3.23 ...

Where:

- `Taxa`: Phytoplankton taxa, *See Table S3 for more information*
  *character*
- `Year`: Year between 2008 and 2021, *integer*
- `Station`: the three stations BY31, BY15, and BY5, *character*
- `Group`: Group of phytoplankton taxa: only Phytoplankton, *character*
- `Timing`: Bloom peak timing (Week \#), *numeric*
- `Maximum`: Bloom magnitude maximum ($\mu$gC L<sup>-1</sup>), *numeric*
- `Magnitude`: Bloom magnitude ($\mu$gC L<sup>-1</sup>), *numeric*
- `Start`: Start of the growing season, correspond to the
  25<sup>th</sup> percentile of the cumulative biomass (Week \#),
  *numeric*
- `End`: End of the growing season, correspond to the 75<sup>th</sup>
  percentile of the cumulative biomass (Week \#), *numeric*
- `Middle`: Middle of the growing season, correspond to the
  50<sup>th</sup> percentile of the cumulative biomass (Week \#),
  *numeric*
- `Duration`: Time span between the Start and the End of the growing
  season (Week \#), *numeric*

## Abiotic.txt

This dataset is produced with the script `Physical.R`

It has this structure:

    #> 'data.frame':    4600 obs. of  6 variables:
    #>  $ Year     : int  2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
    #>  $ month    : int  1 1 1 1 1 1 1 1 1 1 ...
    #>  $ Parameter: chr  "Nutrient" "Nutrient" "Nutrient" "Oxy_0.20" ...
    #>  $ Station  : chr  "BY15" "BY31" "BY5" "BY15" ...
    #>  $ Value    : num  2.64 3.81 3.04 7.92 7.85 ...
    #>  $ n        : int  1 1 1 1 1 1 1 1 1 1 ...

Where:

- `Year`: Year of the sampling event, *integer*
- `month`: Month of the sampling event. 1 corresponds to January, 2 to
  February, and so on, *integer*
- `Parameter`: Parameter sampled. *character*
  - Oxy_0.20: Integrated oxygen concentration between 0 and 20m depth,
    mL L<sup>-1</sup>
  - SST: Sea surface temperature, °C
  - Temp_0.20: Integrated temperature between 0 and 20m depth, °C
  - Salinity: Integrated salinity between 0 and 20m depth
  - Salinity_SSS: Sea surface salinity
  - Nutrient: Nitrate and nitrite concentration between 0 and 20m depth,
    $\mu$mol L<sup>-1</sup>
  - strat_index: Stratification index calculated as (SST - Temperature
    at 60m) / average temperature between 0 and 60m
- `Station`: Station sampled, *character*
- `Value`: value of the parameter, *numeric*
- `n`: number of observation, *integer*

[^1]: Note that data were downloaded using the
    [shaRk](https://github.com/andreasnovotny/shaRk) Rpackage for this
    analysis. If the data are downloaded throughout the
    [Sharkweb](https://sharkweb.smhi.se/hamta-data/), make sure to
    update the headers accordingly.

[^2]: Note that data were downloaded using the
    [shaRk](https://github.com/andreasnovotny/shaRk) Rpackage for this
    analysis. If the data are downloaded throughout the
    [Sharkweb](https://sharkweb.smhi.se/hamta-data/), make sure to
    update the headers accordingly.

[^3]: Note that data were downloaded using the
    [shaRk](https://github.com/andreasnovotny/shaRk) Rpackage for this
    analysis. If the data are downloaded throughout the
    [Sharkweb](https://sharkweb.smhi.se/hamta-data/), make sure to
    update the headers accordingly.

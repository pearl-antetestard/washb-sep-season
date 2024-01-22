# washb-sep-season

A pre-specified secondary analysis of the Water, Sanitation and Handwashing (WASH) Benefits Bangladesh cluster-randomized trial to assess the ability of the combined WSH intervention to reduce climate-related diarrhea risk among children along a gradient of socioeconomic position.

## Description

This repository includes R codes to support the paper: 

_"WASH interventions and child diarrhea at the interface of climate and socioeconomic position in Bangladesh"_

P.A. Ante-Testard, F. Rerolle, A. Nguyen, S. Ashraf, S.M. Parvez, A.B. Naser, T. Benmarhnia, M. Rahman, S.P. Luby, J. Benjamin-Chung, B.F. Arnold

If there are any inquiries regarding the files in this repository, please feel free to contact Pearl Ante-Testard at UCSF (pearl.ante@ucsf.edu).

## Additional Resources

### Open Science Framework

This repo is mirrored to the Open Science Framework, where we also archived the pre-analysis plan and de-identified data of the study:  https://osf.io/xwndg/.

### Systems Requirement

All analyses were running using R software version 4.2.1 on Mac OSX Ventura using the RStudio IDE (https://www.rstudio.com). 

`> sessionInfo()`

`R version 4.2.1 (2022-06-23)`

`Platform: x86_64-apple-darwin17.0 (64-bit)`

`Running under: macOS Ventura 13.4.1`

### Installation Guide

You can download and install R from CRAN: https://cran.r-project.org

You can download and install RStudio from their website: https://www.rstudio.com

All R packages required to run the analyses are sourced in the file `0-config.R`.

The installation time should be < 10 minutes total on a typical desktop computer.

### Instructions for Use

To run the analyses:
1. Clone this GitHub repository.
2. Create subdirectories named: `1-data/0-untouched`,`1-data/0-gadm`,`1-data/0-terraclim`, `1-data/0-terraclim/ppt`, `1-data/0-terraclim/tmax`, `1-data/0-terraclim/soil`, `1-data/0-worldpop` and `1-data/2-final`.
3. If you want to reproduce the formatted dataset that contains the created wealth and season variables, you can download the public raw datasets (`washb-bangladesh-enrol-public.csv`, `washb-bangladesh-tr-public.csv` and `washb-bangladesh-diar-public.csv`) from the WASH Benefits Bangladesh trial from https://osf.io/wvyn4/. Then, save them in the subdirectory `1-data/0-untouched`. 
4. Otherwise, you can download the formatted datasets directly from https://osf.io/xwndg/ to skip data management `1-dm` and proceed with the analyses. Save the formatted datasets in `1-data/2-final`.
5. The Bangladesh geospatial data can be downloaded from:
GADM shapefiles: https://gadm.org/ (save to `1-data/0-gadm`)
WorldPop population and wealth layers: https://www.worldpop.org/ (save to `1-data/0-worldpop`)
Rural/urban layer: `https://ghsl.jrc.ec.europa.eu` (rural/urban raster layer save to `1-data/2-final`)
Precipitation data used to create the heavy rainfall variable: `https://www.gloh2o.org/mswep/` (The formatted dataset is not included since it includes the GPS. However, the script on how to create the heavy rainfall indicator is available `4-supplementary/R/6_heavy_rain_ind)`
TerraClimate data: https://www.climatologylab.org/terraclimate.html (save to `1-data/0-terraclim`)
Precipitation: `1-data/0-terraclim/ppt`
Maximum temperature: `1-data/0-terraclim/tmax`
Soil moisture: `1-data/0-terraclim/soil`
6. Create subdirectories to save the output for each analysis: effect modification analysis (`2-initial-analysis/output`), spatial analysis (`3-secondary-analysis/output`) and supplementary analysis (`4-supplementary/output`).

N.B. The GPS of the study clusters are not available. Our IRB has deemed that sharing this information could potentially compromise participant privacy. However, this limitation only applies to the spatial analysis; the scripts are accessible, excluding the geographic location data. 






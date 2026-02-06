---
title: "BiomeBGC_validationFluxTower"
author: 
  - Dominique Caron
  - Céline Boisvenue
date: "February 2026"
output:
  html_document:
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Compares Biome-BGC gross primary productivity predictions to Eddy covariance flux tower's estimates.

# Parameters

Provide a summary of user-visible parameters.


|paramName        |paramClass |default      |min |max |paramDesc                                                                                                                  |
|:----------------|:----------|:------------|:---|:---|:--------------------------------------------------------------------------------------------------------------------------|
|resolution       |numeric    |250          |NA  |NA  |Defines the resolution for the raster created for the study site.                                                          |
|targetCRS        |character  |+proj=lc.... |NA  |NA  |Defines the resolution for the raster created for the study site.                                                          |
|.plots           |character  |screen       |NA  |NA  |Used by Plots function, which can be optionally used here                                                                  |
|.plotInitialTime |numeric    |0            |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                  |
|.plotInterval    |numeric    |NA           |NA  |NA  |Describes the simulation time interval between plot events.                                                                |
|.saveInitialTime |numeric    |NA           |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                  |
|.saveInterval    |numeric    |NA           |NA  |NA  |This describes the simulation time interval between save events.                                                           |
|.studyAreaName   |character  |NA           |NA  |NA  |Human-readable name for the study area used - e.g., a hash of the studyarea obtained using `reproducible::studyAreaName()` |
|.seed            |list       |             |NA  |NA  |Named list of seeds to use for each event (names).                                                                         |
|.useCache        |logical    |FALSE        |NA  |NA  |Should caching of events or module be used?                                                                                |

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

Usually, the module is used with `BiomeBGC_dataPrep` and `BiomeBGC_core`. The needed inputs are the flux tower data which needs to be downloaded (e.g., through AmeriFlux). The user also needs to provide the tower coordinates.


|objectName       |objectClass |desc                                                                                                                                                                                                     |sourceURL |
|:----------------|:-----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------|
|towerCoordinates |vector      |Must be provided by the user. A named vector with the longitude and latitude of the EC tower. The names of the variables must be 'lon' and 'lat'.                                                        |NA        |
|towerDailyFlux   |data.frame  |Must be provided by the user. The daily data from the Eddy covariance flux tower. A csv named 'XXX_XX-XXX_FLUXNET_FLUXMET_DD_XXXX-XXXX_XXX.csv' is usually available when downloading flux tower data.   |NA        |
|towerMonthlyFlux |data.frame  |Must be provided by the user. The monthly data from the Eddy covariance flux tower. A csv named 'XXX_XX-XXX_FLUXNET_FLUXMET_MM_XXXX-XXXX_XXX.csv' is usually available when downloading flux tower data. |NA        |
|towerAnnualFlux  |data.frame  |Must be provided by the user. The annual data from the Eddy covariance flux tower. A csv named 'XXX_XX-XXX_FLUXNET_FLUXMET_YY_XXXX-XXXX_XXX.csv' is usually available when downloading flux tower data.  |NA        |
|dailyOutput      |data.frame  |The daily GPP predicted by Biome-BGC. This is an output of the module BiomeBGC_core.                                                                                                                     |NA        |
|monthlyAverages  |data.frame  |The GPP predicted by Biome-BGC averaged at the monthly resolution. This is an output of the module BiomeBGC_core.                                                                                        |NA        |
|annualAverages   |data.frame  |The GPP predicted by Biome-BGC averaged at the monthly resolution. This is an output of the module BiomeBGC_core.                                                                                        |NA        |

## Output data

Description of the module outputs.


|objectName        |objectClass |desc                                                    |
|:-----------------|:-----------|:-------------------------------------------------------|
|studyArea         |SpatVector  |A point vector locating the Eddy covariance flux tower. |
|rasterToMatch     |SpatVector  |A 1 pixel raster.                                       |
|validationSummary |data.frame  |A data frame with validation metrics.                   |

# Links to other modules

Inputs can be prepared by `BiomeBGC_core` and `BiomeBGC_dataPrep`.

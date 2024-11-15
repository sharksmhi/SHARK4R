# SHARK4R <a href="https://sharksmhi.github.io/SHARK4R/"><img src="man/figures/logo.png" align="right" height="139" alt="SHARK4R website" /></a>

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/sharksmhi/SHARK4R/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sharksmhi/SHARK4R/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This R package is equipped with a client application designed for integration 
with API functions. It facilitates access to various databases, 
including [SHARK](https://shark.smhi.se/), 
[SLU Artdatabanken (Dyntaxa)](https://api-portal.artdatabanken.se/), and 
[WoRMS](http://www.marinespecies.org/rest/). The package serves a dual purpose, 
supporting both data analysis and automatic quality control of physical, 
chemical, and marine biological data. Developed by Sh at SMHI, modified 
from <https://iobis.github.io/obistools>

Provoost P and Bosch S (2018). "obistools: Tools for data enhancement
and quality control." Ocean Biogeographic Information System.
Intergovernmental Oceanographic Commission of UNESCO.
<https://cran.r-project.org/package=obistools>.

# Installation of R package

## Installing SHARK4R requires the devtools package:
```
install.packages("devtools")

library(devtools)

install_github("sharksmhi/SHARK4R")

library(SHARK4R)
```
# Running the R package SHARK4R programs

## Download data from SHARKdata

```get_shark_table()``` can be used to call the SHARK API to retrieve
data specified in the arguments. Available filtration options can be listed
with the ```get_shark_options()``` function. The higher taxonomy for biological data is loaded
from the database, but can be updated through the World Register of Marine
Species (WoRMS) and SLU Artdatabanken APIs, using the functions
```update_worms_taxonomy()``` and ```update_dyntaxa_taxonomy()```.

## Check required fields

```check_datatype()``` will check if all global SHARK required fields are
present in an occurrence table and if any values are missing. These
functions returns a dataframe of errors (if any).

## Plot points on a map

```plot_map()``` will generate a ggplot2 map of occurrence records,
```plot_map_leaflet()``` creates a zoomable interactive Leaflet map.

## Check points on land

```check_onland()``` uses the xylookup web service which internally uses land
polygons from OpenStreetMap to check if any points are located on land.
Other shapefiles can be used as well.

## Check depth

```check_depth()``` uses the xylookup web service to identify which records have
potentially invalid depths. Multiple checks are performed in this
function: missing depth column (warning) empty depth column (warning)
depth values that can't be converted to numbers (error) values that are
larger than the depth value in the bathymetry layer, after applying the
provided depthmargin (error) depth values that are negative for off
shore points, after applying the provided shoremargin (error) minimum
depth greater than maximum depth (error)

## Check outliers

```check_outliers_dataset()``` use the qc-service web service to identify which
records are statistical outliers. For species outlier checks are
performed for both environmental data (bathymetry, sea surface salinity
and sea surface temperature) as well as spatially. Outliers are
identified as all points that deviate more then six times the median
absolute deviation (MAD) or three times the interquartile range (IQR)
from the median. The list in the extra field of the debug level output
in the report provides all relevant statistics on which the outlier
analysis is based. The report also gives an overview of these outliers.
Outliers can be plotted with ```plot_outliers(report)```

## Taxon matching

```match_taxa()``` performs interactive taxon matching with the World Register
of Marine Species (WoRMS; <http://www.marinespecies.org/index.php>).
```match_taxon_name()``` matches taxa names through the SLU Artdatabanken API (Dyntaxa) and provides taxon_ids.

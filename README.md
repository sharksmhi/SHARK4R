# SHARK4R
![Image description](https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/SMHI_Logo.svg/500px-SMHI_Logo.svg.png)

R package for data analysis and automatic quality control of physical, chemical and marine biological data developed by Sh at SMHI, modified from https://iobis.github.io/obistools

Provoost P and Bosch S (2018). “obistools: Tools for data enhancement and quality control.” Ocean Biogeographic Information System. Intergovernmental Oceanographic Commission of UNESCO. https://cran.r-project.org/package=obistools.

# Installation of R package
## Installing SHARK4R requires the devtools package:

install.packages("devtools")

library(devtools)

install_github("sharksmhi/SHARK4R")

library(SHARK4R)

# Running the R package SHARK4R programs

## Check required fields
check_datatype() will check if all global SHARK required fields are present in an occurrence table and if any values are missing.
These functions returns a dataframe of errors (if any).

## Plot points on a map
plot_map() will generate a ggplot2 map of occurrence records, plot_map_leaflet() creates a zoomable interactive Leaflet map.

## Check points on land
check_onland() uses the xylookup web service which internally uses land polygons from OpenStreetMap to check if any points are located on land. Other shapefiles can be used as well.

## Check depth
check_depth uses the xylookup web service to identify which records have potentially invalid depths. Multiple checks are performed in this function:
missing depth column (warning)
empty depth column (warning)
depth values that can't be converted to numbers (error)
values that are larger than the depth value in the bathymetry layer, after applying the provided depthmargin (error)
depth values that are negative for off shore points, after applying the provided shoremargin (error)
minimum depth greater than maximum depth (error)

## Check outliers
check_outliers_dataset use the qc-service web service to identify which records are statistical outliers. For species outlier checks are performed for both environmental data (bathymetry, sea surface salinity and sea surface temperature) as well as spatially. Outliers are identified as all points that deviate more then six times the median absolute deviation (MAD) or three times the interquartile range (IQR) from the median. The list in the extra field of the debug level output in the report provides all relevant statistics on which the outlier analysis is based. The report also gives an overview of these outliers. Outliers can be plotted with plot_outliers(report)

## Taxon matching
match_taxa() performs interactive taxon matching with the World Register of Marine Species (WoRMS; http://www.marinespecies.org/index.php).

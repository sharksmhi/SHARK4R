# SHARK4R

![](https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/SMHI_Logo.svg/500px-SMHI_Logo.svg.png)

This R package is equipped with a client application designed for integration 
with API functions. It facilitates access to various databases, 
including [SHARKdata](https://sharkdata.smhi.se/), 
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

install.packages("devtools")

library(devtools)

install_github("sharksmhi/SHARK4R")

library(SHARK4R)

# Running the R package SHARK4R programs

## Download data from SHARKdata

download_sharkdata() can be used to call the SHARKdata API to download
datasets specified in the arguments. Available datasets can be listed
with the load_dataset_names() function. Data can be checked for
available updates. The higher taxonomy for biological data is loaded
from file, but can be updated through the World Register of Marine
Species (WoRMS) and SLU Artdatabanken APIs, using the functions
update_worms_taxonomy() and update_worms_taxonomy().

## Check required fields

check_datatype() will check if all global SHARK required fields are
present in an occurrence table and if any values are missing. These
functions returns a dataframe of errors (if any).

## Plot points on a map

plot_map() will generate a ggplot2 map of occurrence records,
plot_map_leaflet() creates a zoomable interactive Leaflet map.

## Check points on land

check_onland() uses the xylookup web service which internally uses land
polygons from OpenStreetMap to check if any points are located on land.
Other shapefiles can be used as well.

## Check depth

check_depth uses the xylookup web service to identify which records have
potentially invalid depths. Multiple checks are performed in this
function: missing depth column (warning) empty depth column (warning)
depth values that can't be converted to numbers (error) values that are
larger than the depth value in the bathymetry layer, after applying the
provided depthmargin (error) depth values that are negative for off
shore points, after applying the provided shoremargin (error) minimum
depth greater than maximum depth (error)

## Check outliers

check_outliers_dataset use the qc-service web service to identify which
records are statistical outliers. For species outlier checks are
performed for both environmental data (bathymetry, sea surface salinity
and sea surface temperature) as well as spatially. Outliers are
identified as all points that deviate more then six times the median
absolute deviation (MAD) or three times the interquartile range (IQR)
from the median. The list in the extra field of the debug level output
in the report provides all relevant statistics on which the outlier
analysis is based. The report also gives an overview of these outliers.
Outliers can be plotted with plot_outliers(report)

## Taxon matching

match_taxa() performs interactive taxon matching with the World Register
of Marine Species (WoRMS; <http://www.marinespecies.org/index.php>).
match_taxon_name() matches taxa names through the SLU Artdatabanken API (Dyntaxa) and provides taxon_ids.


Please read the [conditions](https://www.artdatabanken.se/tjanster-och-miljodata/oppna-data-och-apier/api-villkor/) and [register](https://api-portal.artdatabanken.se/) before using the SLU Artdatabanken API.

Data collected through the API is stored at SLU Artdatabanken.

Genom att anvanda denna applikation atar jag mig att folja regler for
anvandning av SLU Artdatabankens information, inklusive att respektera tredje
mans upphovsratt. Detta atagande gors aven direkt mot SLU. Vid brott mot detta
kan jag forlora ratten att anvanda applikationen

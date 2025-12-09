# SHARK4R: Accessing and Validating Marine Environmental Data from 'SHARK' and Related Databases <a href="https://sharksmhi.github.io/SHARK4R/"><img src="man/figures/logo.png" align="right" height="139" alt="SHARK4R website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/sharksmhi/SHARK4R/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sharksmhi/SHARK4R/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/SHARK4R)](https://CRAN.R-project.org/package=SHARK4R)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![codecov](https://codecov.io/gh/sharksmhi/SHARK4R/graph/badge.svg?token=187ZJ0O9R8)](https://app.codecov.io/gh/sharksmhi/SHARK4R)
<!-- badges: end -->

## Overview

This R package is equipped with a client application designed for integration 
with API functions. It facilitates access to various databases, 
including [SHARK](https://shark.smhi.se/en/), [SLU Artdatabanken (Dyntaxa)](https://artfakta.se/), 
[WoRMS](https://www.marinespecies.org/rest/), [AlgaeBase](https://www.algaebase.org/), 
[IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae](https://www.marinespecies.org/hab/), 
[IOC-UNESCO Toxins database](https://toxins.hais.ioc-unesco.org/), [OBIS xylookup web service](https://iobis.github.io/xylookup/) 
and [Nordic Microalgae](https://nordicmicroalgae.org/). The package serves a dual purpose, 
supporting both data analysis and automatic quality control of physical, chemical, and marine biological data. 
Several quality control components, originally developed by Provoost and Bosch (2018), 
have been adapted for compatibility with the SHARK format.

## Installation

You can install the latest version of `SHARK4R` from CRAN using:
```r
install.packages("SHARK4R")
```

Or install the development version (requiring the `remotes` package):
```r
# install.packages("remotes")
remotes::install_github("sharksmhi/SHARK4R", 
                        dependencies = TRUE)
```

## Documentation and Tutorials

### Website

For detailed information, please visit the project's [webpage](https://sharksmhi.github.io/SHARK4R/).

### Tutorials

Explore the features and capabilities of `SHARK4R` through these tutorials:

- [Retrieve Data From SHARK](https://sharksmhi.github.io/SHARK4R/articles/retrieve_shark_data.html)
- [Retrieve Data From Dyntaxa](https://sharksmhi.github.io/SHARK4R/articles/retrieve_dyntaxa_data.html)
- [Retrieve Data From WoRMS](https://sharksmhi.github.io/SHARK4R/articles/retrieve_worms_data.html)
- [Retrieve Data From AlgaeBase](https://sharksmhi.github.io/SHARK4R/articles/retrieve_algaebase_data.html)
- [Retrieve Data From IOC HAB and Toxin databases](https://sharksmhi.github.io/SHARK4R/articles/retrieve_hab_data.html)
- [Retrieve Data From Nordic Microalgae](https://sharksmhi.github.io/SHARK4R/articles/retrieve_nordic_microalgae_data.html)
- [Quality Control of SHARK Data](https://sharksmhi.github.io/SHARK4R/articles/quality_control.html)

### Reference

For a detailed overview of all available `SHARK4R` functions, please visit the reference section:

- [Function Reference](https://sharksmhi.github.io/SHARK4R/reference/index.html)

## Repository

For more details and the latest updates, visit the [GitHub repository](https://github.com/sharksmhi/SHARK4R/).

## License

This package is licensed under the MIT License.

## References

Provoost P, Bosch S (2024). `obistools`: Tools for data enhancement and quality control. Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. R package version 0.1.0, <https://iobis.github.io/obistools/>.

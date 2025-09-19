# SHARK4R: Retrieving, Analyzing, and Validating Marine Data from SHARK and Nordic Microalgae <a href="https://sharksmhi.github.io/SHARK4R/"><img src="man/figures/logo.png" align="right" height="139" alt="SHARK4R website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/sharksmhi/SHARK4R/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sharksmhi/SHARK4R/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14169399.svg)](https://doi.org/10.5281/zenodo.14169399)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov](https://codecov.io/gh/sharksmhi/SHARK4R/graph/badge.svg?token=187ZJ0O9R8)](https://app.codecov.io/gh/sharksmhi/SHARK4R)
<!-- badges: end -->

## Overview

This R package is equipped with a client application designed for integration 
with API functions. It facilitates access to various databases, 
including [SHARK](https://shark.smhi.se/), [SLU Artdatabanken (Dyntaxa)](https://api-portal.artdatabanken.se/), 
[WoRMS](https://www.marinespecies.org/rest/), [AlgaeBase](https://www.algaebase.org/), 
[IOC-UNESCO Taxonomic Reference List of Harmful Micro Algae](https://www.marinespecies.org/hab/), 
[IOC-UNESCO Toxins database](https://toxins.hais.ioc-unesco.org/) and [Nordic Microalgae](https://nordicmicroalgae.org/). 
The package serves a dual purpose, supporting both data analysis and automatic quality control of physical, 
chemical, and marine biological data. Several quality control components originate from Provoost and Bosch (2018).

## Installation

Installing `SHARK4R` requires the `remotes` package. Install the latest version of `SHARK4R` from GitHub:
```r
# install.packages("remotes")
remotes::install_github("sharksmhi/SHARK4R", 
                        ref = remotes::github_release(),
                        dependencies = TRUE)
```

Or install the development version:
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

Provoost, P. and Bosch, S. (2018). `obistools`: Tools for data enhancement and quality control. Ocean Biogeographic Information System. Intergovernmental Oceanographic Commission of UNESCO. <https://iobis.github.io/obistools/>.

# SHARK4R (development version)

## New features

* Added `get_nua_media_metadata()` to retrieve detailed metadata for media items from Nordic Microalgae
* Added `get_nua_image_labeling_links()` to retrieve media URLs for automated imaging images (e.g., IFCB) from Nordic Microalgae
* Added `get_nua_image_labeling_metadata()` to retrieve detailed metadata for automated imaging images from Nordic Microalgae

## Security improvements

* `load_shark4r_fields()` now downloads field definitions as a binary `.rds` file instead of sourcing a remote R script, eliminating a remote code execution risk.
* `get_shark_data()` now validates that `file_path` does not contain `..` path components, preventing path traversal when used in downstream applications.
* `get_shark_datasets()` now sanitizes dataset names from the API with `basename()` before writing to disk.

## Bug fixes and CRAN compliance

* Fixed `get_dyntaxa_records()` silently returning a character string on API error instead of raising a condition with `stop()`.
* Fixed `get_shark_data()` silently dropping `municipalities`, `waterCategories`, and `typOmraden` parameters when computing the pre-download row count, which could bypass the chunked download path.
* Fixed operator precedence bug `nrow(genus > 0)` to `nrow(genus) > 0` in `construct_dyntaxa_missing_table()`.
* Replaced unsafe `1:length()` and `1:nrow()` loops with `seq_along()` and `seq_len()` in `construct_dyntaxa_missing_table()` and `construct_dyntaxa_table()` to prevent errors on zero-length input.
* Replaced `message("ERROR: ...")` calls with `warning()` in `check_value_logical()`, `check_zero_value()`, `check_zero_positions()`, `check_codes()`, and `read_shark_deliv()` so conditions can be caught programmatically with `tryCatch()`.
* Removed duplicate `cachefile` assignment in `cache_call()` and dead `list.files()` call in `list_cache()`.
* All internal cache paths now use the `cache_dir()` helper, which redirects to `tempdir()` during R CMD check. Previously several functions called `tools::R_user_dir()` directly, which could create `~/.cache/R/SHARK4R` during CRAN checks.
* Fixed evaluation order bug in `clean_shark4r_cache()` where `search_pattern = NULL` (the default) could cause an error.
* Fixed deprecation warning version numbers in algaebase functions (now correctly reference `1.0.0`).
* Fixed spelling of "Microalgae" and other minor typos in `get_hab_list()` documentation.

# SHARK4R 1.0.3

## New features

* Added a new `harmful_non_toxic_only` argument to `get_hab_list()` to download only non-toxigenic harmful microalgal species from the IOC-UNESCO HABs list
* Added a new `species_only` argument to `get_hab_list()` to return only species-level records, which is now also the default option. This filter is ignored when `harmful_non_toxic_only = TRUE`
* Added a new `verbose` argument to `positions_are_near_land()` and `get_hab_list(harmful_non_toxic_only = TRUE)` to enable printing of progress messages during data retrieval

## Minor improvements and fixes

* EEA coastline data are now obtained from EEA map services in `positions_are_near_land()`, replacing direct file server downloads that were unstable
* All data frame outputs are now consistently returned as tibbles
* Documentation updated with minor clarifications and corrections

# SHARK4R 1.0.2

## Bug fixes

* `SHARK4R:::.type_lookup` now includes the missing datatype `Jellyfish` (#39)
* Enhanced documentation and example execution in vignettes - updated examples to run correctly and improved clarity in vignettes and help files
* HTML widgets and interactive objects are now displayed only in pkgdown articles; they are suppressed in CRAN vignettes to reduce size and improve build safety
* Added tests for helper functions – covering `extract_complete_toxins()` and `repair_toxins_json()`
* Cache is now cleared after R CMD check

# SHARK4R 1.0.1

## Bug fixes and CRAN compliance

* Corrected formatting of package and software names throughout the package.
* Fixed broken URL in `README.md`
* Improved examples to run reliably and quickly
* Ensured functions and examples no longer write to the user's home directory
* Added `lon` and `lat` arguments to the `lookup_xy()` function for more flexible lookup options
* Minor documentation improvements throughout the package
* `get_toxin_list()` now handles partial JSON responses

# SHARK4R 1.0.0

## New features
* Add new functions: `get_delivery_template()` and `find_required_fields()` to get SHARK delivery templates and required fields from the web
* Add new function `get_shark_datasets()` to retrieve available SHARK datasets from API
* Add new function `get_nomp_list()` to download and read the latest NOMP biovolume Excel lists
* Add new function `get_peg_list()` to download and read the PEG biovolume Excel list
* Add new function `read_ptbx()` to read Plankton Toolbox files
* Add new function `get_shark_codes()` to download and read the current SHARK code lists
* Add new function `clean_shark4r_cache()` to clear cached files
* Add new function `check_setup()` and `run_qc_app()` to download and run SHARK QC scripts and Shiny App
* Add new function `get_shark_statistics()` to download SHARK data and extract summary statistics for numeric parameters
* Add new function `translate_shark_datatype()` to translate user-facing datatype names to internal SHARK4R names
* Add new function `load_shark4r_stats()` to download and load precomputed SHARK4R statistical datasets (e.g., threshold or summary statistics) from a GitHub repository
* Add new function `load_shark4r_fields()` to download and load fields definitions from a GitHub repository
* Add new function `get_worms_classification()` to retrieve higher taxonomic information from WoRMS records
* Add new function `get_worms_taxonomy_tree()` to retrieve and constructs a hierarchical taxonomy tree from WoRMS records
* Add new function `convert_ddmm_to_dd()` to convert coordinates from DDMM format to decimal degrees

## Enhancements
* Re-export functions from the `iRfcb` package: `which_basin()` and `positions_are_near_land()`
* Cache downloaded DwCA files in `get_dyntaxa_dwca()`, `get_shark_codes()`, `get_nomp_list()` and `get_peg_list()` to avoid repeated downloads
* Add unit tests for the majority of the package functions
* Update and correct `check_depth()`
* Fix bugs in check_* functions
* Fix various documentation issues
* Updated several OBIS-dependent functions using `lookup_xy()`
* Updated `scatterplot()` function to allow plotting of multiple parameters
* Updated `match_worms_taxa()` function to clean taxon names from problematic special characters before being passed to API call
* Added `plot_leaflet` argument to functions `check_station_distance()` and `check_onland()`
* Added `utv` argument to functions `get_shark_options()`, `get_shark_data()`, `get_shark_datasets()` and `get_shark_table_counts()`.
* Added `add_rank_to_hierarchy` argument to the `add_worms_taxonomy()` function
* `match_worms_taxa()` now handles bulk API requests using the `bulk` argument
* Add SHARK4R Bio-QC Tool Shiny App to bundle, with improved performance (initialized by `run_qc_app()`)

## Deprecated
* Deprecated functions: `ifcb_is_near_land()` and `ifcb_which_basin()` (replaced by re-exported functions with improved cache behavior)
* Deprecated functions: `get_algaebase_species()`, `get_algaebase_genus()` and `match_algaebase()` are now replaced by `match_algaebase_species()`, `match_algaebase_genus()` and `match_algaebase_taxa()`
* Deprecated functions: Parameter and datatype-specific `check_*_*_logical()` functions replaced by general functions `check_parameter_rules` and `check_logical_parameter()` function
* Deprecated functions: Datatype-specific field check functions `check_*()` and `check_*_deliv()` replaced by a general `check_fields()` function
* Deprecated functions: Parameter and datatype-specific `check_*_*()` functions to check for outliers replaced by a general `check_outliers()` function
* Deprecated functions: `shark_read_deliv()` and `shark_read_deliv_xls()` are now replaced by `read_shark_deliv()`
* Deprecated functions: `shark_read()` and `shark_read_zip()` are now replaced by `read_shark()`
* Deprecated function: `match_dyntaxa()` is now replaced by `is_in_dyntaxa()`
* Deprecated function: `plot_map_leaflet_deliv()` is now replaced by `plot_map_leaflet()`
* Deprecated function: `check_code_proj()` is now replaced by `check_codes()`
* Deprecated function: `match_taxon_name()` is now replaced by `match_dyntaxa_taxa()`
* Deprecated function: `get_worms_records_name()` is now replaced by `match_worms_taxa()`
* Deprecated function: `nominal_station()` is now replaced by `check_nominal_station()`
* Deprecated function: `match_wormstaxa()` is now replaced by `match_worms_taxa()`
* Deprecated argument: `apikey` replaced by `subscription_key` in `get_algaebase_genus()`, `get_algaebase_species()` and `match_algaebase()`
* Deprecated argument: `aphia_id` replaced by `aphia_ids` in `get_worms_records()` and `add_worms_taxonomy()`
* Deprecated argument: `scientific_name` replaced by `scientific_names` in `add_worms_taxonomy()` and `parse_scientific_names()`
* Deprecated argument: `genus` replaced by `genera` in `match_algaebase_taxa()`

## Defunct / Removed
* Defunct function `get_shark_table()`

## Documentation
* Add `NEWS.md` file
* Add spell check

# SHARK4R 0.1.7

* Add option to specify `row_limits` in `get_shark_data()` to retrieve data in yearly chunks
* Add functions to call APIs to retrieve IOC HAB and IOC IPHAB Toxin lists: `get_hab_list()` and `get_toxin_list()`

# SHARK4R 0.1.6

Patch release

* Fix issue #16
* Add unparsed output option for `get_shark_option()`

# SHARK4R 0.1.5

Patch release

* Fix issues with `construct_dyntaxa_table()`
* Fix parsing issue in `get_shark_data()`
* Add more custom groups to `assign_plankton_group()`
* Add more flexibility to `get_shark_data()` parameters, e.g. boundary and year ranges

# SHARK4R 0.1.4

* Add algaebase API functions
* Defunct sharkdata functions
* Fix parsing issue in shark_data when data are reported as "-", which is now a NA pattern
* Cleanup of large files
* UTF-8 encoding

# SHARK4R 0.1.3

* Minor bug-fixes and updated documentation

# SHARK4R 0.1.2

* Patch release

# SHARK4R 0.1.1

* Fix download of large datasets
* Add more WoRMS functionality, including plankton group assignment

# SHARK4R 0.1.0

* Add SHARK API functionality
* Improve Dyntaxa API functionality
* Fix documentation issues

# SHARK4R 0.0.1

* Initial development version.

# SHARK4R (development version)

## New features
* Add new function `get_shark_datasets()` to retrieve available SHARK datasets from API.
* Add new function `get_nomp_list()` to download and read the latest NOMP biovolume Excel lists.
* Add new function `get_peg_list()` to download and read the PEG biovolume Excel list.
* Add new function `read_ptbx()` to read Plankton Toolbox files.
* Add new function `get_shark_codes()` to download and read the current SHARK code lists.
* Add new function `clean_shark4r_cache()` to clear cached files.
* Add new function `check_setup()` to download and run SHARK QC scripts and Shiny App.
* Add new function `get_shark_statistics()` to download SHARK data and extract summary statistics for numeric parameters.

## Enhancements
* Re-export functions from the `iRfcb` package: `which_basin()` and `positions_are_near_land()`.
* Cache downloaded DwCA files in `get_dyntaxa_dwca()`, `get_shark_codes()`, `get_nomp_list()` and `get_peg_list()` to avoid repeated downloads.
* Add unit tests for the majority of the package functions.
* Fix bugs in check_* functions.
* Fix various documentation issues.
* Updated several OBIS-dependent functions using `lookup_xy()`.

## Deprecated
* Deprecated functions: `ifcb_is_near_land()` and `ifcb_which_basin()` (replaced by re-exported functions with improved cache behavior)
* Deprecated functions: `get_algaebase_species()`, `get_algaebase_genus()` and `match_algaebase()` are now replaced by `match_algaebase_species()`, `match_algaebase_genus()` and `match_algaebase_taxa()`.
* Deprecated functions: Parameter and datatype-specific `check_*_*_logical()` functions replaced by a general `check_logical_parameter()` function.
* Deprecated function: `match_dyntaxa()` is now replaced by `is_in_dyntaxa()`.
* Deprecated function: `match_taxon_name()` is now replaced by `match_dyntaxa_taxa()`.
* Deprecated function: `get_worms_records_name()` is now replaced by `match_worms_taxa()`.
* Deprecated function: `match_wormstaxa()` is now replaced by `match_worms_taxa_interactive()`.
* Deprecated argument: `apikey` replaced by `subscription_key` in `get_algaebase_genus()`, `get_algaebase_species()` and `match_algaebase()`

## Defunct / Removed
* Defunct function `get_shark_table()`.

## Documentation
* Add `NEWS.md` file.
* Add spell check.

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

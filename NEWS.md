# SHARK4R (development version)

* Add tests
* Add NEWS.md
* Re-export functions from `iRfcb`
* New function: `get_shark_datasets()`
* Defunct `get_shark_table()`
* Cache downloaded DwCA file from `get_dyntaxa_dwca()`
* Add reader for Plankton Toolbox files: `read_ptbx()`

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

* Add algaebase api functions
* Defunct sharkdata functions
* Fix parsing issue in shark_data when data are reported as "-", which is now a NA pattern
* Cleanup of large files
* UTF-8 encoding

# SHARK4R 0.1.3

* Minor buggfixes and updated documentation

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

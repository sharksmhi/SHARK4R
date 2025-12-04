# Download and set up SHARK4R support files

This function downloads the `products` folder from the SHARK4R GitHub
repository and places them in a user-specified directory. These folders
contain Shiny applications and R Markdown documents used for quality
control (QC) of SHARK data.

## Usage

``` r
check_setup(path = ".", run_app = FALSE, force = FALSE, verbose = TRUE)
```

## Arguments

- path:

  Character string specifying the target directory where the `products`
  folder should be stored. Defaults to the current working directory
  (`"."`).

- run_app:

  Logical, if `TRUE` runs the QC Shiny app located in the `products`
  folder after setup. Default is `FALSE`.

- force:

  Logical, if `TRUE` forces a re-download and overwrites existing
  folder. Default is `FALSE`.

- verbose:

  Logical, if `TRUE` prints progress messages. Default is `TRUE`.

## Value

An (invisible) list with the path to the local `products` folder:

## Details

By default, the folders are downloaded into the current working
directory. If the folders already exist, the download will be skipped
unless `force = TRUE` is specified. Optionally, the function can launch
the QC Shiny app directly after setup.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download support files into current working directory
check_setup()

# Download into a specific folder in current working directory
check_setup(path = "shark_qc")

# Force re-download if already present
check_setup(force = TRUE)

# Download and run the QC Shiny app
check_setup(run_app = TRUE)
} # }
```

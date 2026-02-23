# Download and set up SHARK4R support files

This function downloads the `products` folder from the SHARK4R GitHub
repository and places them in a user-specified directory. These folders
contain Shiny applications and R Markdown documents used for quality
control (QC) of SHARK data.

## Usage

``` r
check_setup(path, run_app = FALSE, force = FALSE, verbose = TRUE)
```

## Arguments

- path:

  Character string giving the directory where the products folder should
  be created. Must be provided by the user.

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

If the `path` folders already exist, the download will be skipped unless
`force = TRUE` is specified. Optionally, the function can launch the QC
Shiny app directly after setup.

## Examples

``` r
# \donttest{
# Download support files into a temporary directory
check_setup(path = tempdir())
#> Downloading setup files for SHARK4R...
#> Setup complete. Files are available in /tmp/Rtmpl0NGD4

# Force re-download if already present
check_setup(path = tempdir(), force = TRUE)
#> Downloading setup files for SHARK4R...
#> Setup complete. Files are available in /tmp/Rtmpl0NGD4

# Download and run the QC Shiny app
if(interactive()){
 check_setup(path = tempdir(), run_app = TRUE)
}
# }
```

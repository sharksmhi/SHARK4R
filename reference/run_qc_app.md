# Launch the SHARK4R Bio-QC Tool

This function launches the interactive Shiny application for performing
quality control (QC) on SHARK data. The application provides a graphical
interface for exploring and validating data before or after submission
to SHARK.

## Usage

``` r
run_qc_app(interactive = TRUE)
```

## Arguments

- interactive:

  Logical value whether the session is interactive or not.

## Value

This function is called for its side effect of launching a Shiny
application. It does not return a value.

## Details

The function checks that all required packages for the app are installed
before launching. If any are missing, the user is notified. In
interactive sessions, the function will prompt whether the missing
packages should be installed automatically. In non-interactive sessions
(e.g. scripts or CI), the function instead raises an error and lists the
missing packages so they can be installed manually.

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the SHARK4R Bio-QC Tool
run_qc_app()
} # }
```

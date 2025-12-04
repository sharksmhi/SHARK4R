# Retrieve available search options from SHARK

The `get_shark_options()` function retrieves available search options
from the SHARK database. It sends a GET request to the SHARK API and
returns the results as a structured named list.

## Usage

``` r
get_shark_options(prod = TRUE, utv = FALSE, unparsed = FALSE)
```

## Arguments

- prod:

  Logical value that selects the production server when `TRUE` and the
  test server when `FALSE`, unless `utv` is `TRUE`.

- utv:

  Logical value that selects the UTV server when `TRUE`.

- unparsed:

  Logical. If `TRUE`, returns the complete JSON response as a nested
  list without parsing. Defaults to `FALSE`.

## Value

A named `list` of available search options from the SHARK API. If
`unparsed = TRUE`, returns the raw JSON structure as a list. If
`unparsed = FALSE`, returns a simplified list of character or numeric
vectors.

## Details

This function sends a GET request to the `/api/options` endpoint of the
SHARK API to retrieve available search filters and options that can be
used in SHARK data queries.

## See also

[`get_shark_data()`](https://sharksmhi.github.io/SHARK4R/reference/get_shark_data.md)
for retrieving actual data from the SHARK API.

<https://shark.smhi.se> for the SHARK database portal.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Retrieve available search options (simplified)
  shark_options <- get_shark_options()
  names(shark_options)

  # Retrieve full unparsed JSON response
  raw_options <- get_shark_options(unparsed = TRUE)

  # View available datatypes
  print(shark_options$dataTypes)
} # }
```

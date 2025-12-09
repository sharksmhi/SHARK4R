# Translate SHARK4R datatype names

Converts user-facing datatype names (e.g., "Grey seal") to internal
SHARK4R names (e.g., "GreySeal") based on `SHARK4R:::.type_lookup`. See
available user-facing datatypes in `get_shark_options()$dataTypes`.

## Usage

``` r
translate_shark_datatype(x)
```

## Arguments

- x:

  Character vector of datatype names to translate

## Value

Character vector of translated datatype names

## Examples

``` r
# Example strings
datatypes <- c("Grey seal", "Primary production", "Physical and Chemical")

# Basic translation
translate_shark_datatype(datatypes)
#> [1] "GreySeal"          "PrimaryProduction" "PhysicalChemical" 
```

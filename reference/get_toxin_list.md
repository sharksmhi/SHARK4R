# Retrieve marine biotoxin data from IOC-UNESCO Toxins Database

This function collects data from the [IOC-UNESCO Toxins
Database](https://toxins.hais.ioc-unesco.org/) and returns information
about toxins.

## Usage

``` r
get_toxin_list(return_count = FALSE, insecure = FALSE)
```

## Arguments

- return_count:

  Logical. If `TRUE`, the function returns the count of toxins available
  in the database. If `FALSE` (default), it returns detailed toxin data.

- insecure:

  Logical. If `TRUE`, the request is made without verifying the server's
  TLS certificate. This is a workaround for periods when the
  `toxins.hais.ioc-unesco.org` certificate has expired or is otherwise
  invalid. Defaults to `FALSE`. When `FALSE` and a certificate error is
  encountered, an interactive session will prompt before retrying
  without verification, while a non-interactive session aborts with
  instructions to set `insecure = TRUE`. Disabling verification removes
  protection against tampering and should only be used when the
  certificate issue is known and trusted.

## Value

If `return_count = TRUE`, the function returns a numeric value
representing the number of toxins in the database. Otherwise, it returns
a `tibble` of toxins with detailed information.

## Details

The TLS certificate for `toxins.hais.ioc-unesco.org` may occasionally
lapse. When this happens the default (secure) request fails with a
certificate error. The `insecure` argument provides a deliberate, opt-in
escape hatch: in an interactive session the function prompts before
retrying without verification, while a non-interactive session aborts
and instructs the caller to set `insecure = TRUE`. Only disable
verification when you have confirmed that the failure is caused by the
known certificate issue, as it removes protection against a tampered or
spoofed response.

## See also

<https://toxins.hais.ioc-unesco.org/> for IOC-UNESCO Toxins Database.

## Examples

``` r
# \donttest{
# Retrieve the full list of toxins
try(toxin_list <- get_toxin_list())
if (exists("toxin_list")) head(toxin_list)
#> # A tibble: 6 × 55
#>      id toxin_group recommended_name synonyms recommended_acronym acronyms
#>   <int> <chr>       <chr>            <list>   <chr>               <list>  
#> 1   624 Azaspiracid Azaspiracid-68   <NULL>   AZA-68              <NULL>  
#> 2   623 Azaspiracid Azaspiracid-67   <NULL>   AZA-67              <NULL>  
#> 3   622 Azaspiracid Azaspiracid-66   <NULL>   AZA-66              <NULL>  
#> 4   621 Azaspiracid Azaspiracid-65   <NULL>   AZA-65              <NULL>  
#> 5   620 Azaspiracid Azaspiracid-61   <NULL>   AZA-61              <NULL>  
#> 6   619 Azaspiracid Azaspiracid-60   <NULL>   AZA-60              <NULL>  
#> # ℹ 49 more variables: cas_number <chr>, alternative_cas_numbers <list>,
#> #   formula <chr>, exact_mono_isotopic_mass <dbl>, molfile <chr>,
#> #   alternative_molfiles <lgl>, smiles <chr>, alternative_smiles <list>,
#> #   inchi_key <chr>, alternative_inchi_keys <list>, inchi <chr>,
#> #   alternative_inchies <lgl>, spectra_available <lgl>, certified <lgl>,
#> #   non_certified_reference_material <lgl>, chemical_analysis_research <lgl>,
#> #   chemical_analysis_standardized <lgl>, chemical_analysis_validated <lgl>, …

# Retrieve only the count of toxins
try(toxin_count <- get_toxin_list(return_count = TRUE))
if (exists("toxin_count")) print(toxin_count)
#> [1] 536

# If the server's TLS certificate has expired, the verification step can be
# bypassed explicitly. Only do this when the certificate issue is known and
# trusted, as it disables protection against tampering.
try(toxin_list <- get_toxin_list(insecure = TRUE))
#> Warning: TLS certificate verification disabled — this download is not protected against
#> tampering.
if (exists("toxin_list")) head(toxin_list)
#> # A tibble: 6 × 55
#>      id toxin_group recommended_name synonyms recommended_acronym acronyms
#>   <int> <chr>       <chr>            <list>   <chr>               <list>  
#> 1   624 Azaspiracid Azaspiracid-68   <NULL>   AZA-68              <NULL>  
#> 2   623 Azaspiracid Azaspiracid-67   <NULL>   AZA-67              <NULL>  
#> 3   622 Azaspiracid Azaspiracid-66   <NULL>   AZA-66              <NULL>  
#> 4   621 Azaspiracid Azaspiracid-65   <NULL>   AZA-65              <NULL>  
#> 5   620 Azaspiracid Azaspiracid-61   <NULL>   AZA-61              <NULL>  
#> 6   619 Azaspiracid Azaspiracid-60   <NULL>   AZA-60              <NULL>  
#> # ℹ 49 more variables: cas_number <chr>, alternative_cas_numbers <list>,
#> #   formula <chr>, exact_mono_isotopic_mass <dbl>, molfile <chr>,
#> #   alternative_molfiles <lgl>, smiles <chr>, alternative_smiles <list>,
#> #   inchi_key <chr>, alternative_inchi_keys <list>, inchi <chr>,
#> #   alternative_inchies <lgl>, spectra_available <lgl>, certified <lgl>,
#> #   non_certified_reference_material <lgl>, chemical_analysis_research <lgl>,
#> #   chemical_analysis_standardized <lgl>, chemical_analysis_validated <lgl>, …
# }
```

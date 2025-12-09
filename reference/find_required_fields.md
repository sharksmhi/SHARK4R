# Find required fields in a SHARK delivery template

Identifies which columns are mandatory in the SHARK delivery template
based on rows starting with "\*" (one or more). You can specify how many
levels of asterisks to include.

## Usage

``` r
find_required_fields(
  datatype,
  stars = 1,
  bacterioplankton_subtype = "abundance"
)
```

## Arguments

- datatype:

  Character. The datatype name. Available options include:

  - "Bacterioplankton" (subtypes: "abundance", "production")

  - "Chlorophyll"

  - "Epibenthos" (dive transect)

  - "Dropvideo" (epibenthos drop video)

  - "Grey seal"

  - "Harbour seal"

  - "Ringed seal"

  - "Harbour Porpoise"

  - "Physical and Chemical"

  - "Primary production"

  - "Phytoplankton"

  - "Picoplankton"

  - "Sedimentation"

  - "Seal pathology"

  - "Profile"

  - "Zooplankton"

  - "Zoobenthos"

- stars:

  Integer. Maximum number of "*" levels to include. Default = 1 (only
  single "*"). For example, `stars = 2` includes "*" and "\*\*",
  `stars = 3` includes "*", "**", and "**\*".

- bacterioplankton_subtype:

  Character. For "Bacterioplankton" only: either "abundance" (default)
  or "production". Ignored for other datatypes.

## Value

A character vector of column names that are required in the template.

## Details

Note: A single "\*" marks required fields in the standard SHARK
template. A double "\*\*" is often used to specify columns required for
**national monitoring only**. For more information, see:
https://www.smhi.se/data/hav-och-havsmiljo/datavardskap-oceanografi-och-marinbiologi/leverera-data

## Examples

``` r
# \donttest{
# Only single "*" required columns
find_required_fields("Bacterioplankton")
#>  [1] "MYEAR"    "STATN"    "PROJ"     "ORDERER"  "SHIPC"    "SDATE"   
#>  [7] "LATIT"    "LONGI"    "POSYS"    "WADEP"    "SMPDEP"   "SLABO"   
#> [13] "ACKR_SMP" "SMTYP"    "SMVOL"    "SPVOL"    "COEFF"    "SUBNO"   
#> [19] "METFP"    "BCTCNT"   "BCTABU"   "BABUFLG"  "BACTBIOM" "ALABO"   
#> [25] "ACKR_ANA" "ANADATE"  "METDC"   

# Include both "*" and "**" required columns (national monitoring too)
find_required_fields("Bacterioplankton", stars = 2)
#>  [1] "MYEAR"    "STATN"    "PROJ"     "ORDERER"  "SHIPC"    "SDATE"   
#>  [7] "LATIT"    "LONGI"    "POSYS"    "WADEP"    "MPROG"    "SMPDEP"  
#> [13] "SLABO"    "ACKR_SMP" "SMTYP"    "SMVOL"    "SPVOL"    "COEFF"   
#> [19] "SUBNO"    "METFP"    "BCTCNT"   "BCTABU"   "BABUFLG"  "BACTBIOM"
#> [25] "ALABO"    "ACKR_ANA" "ANADATE"  "METDC"   

# Include up to three levels of "*"
find_required_fields("Phytoplankton", stars = 3)
#>  [1] "MYEAR"    "STATN"    "PROJ"     "ORDERER"  "SHIPC"    "SDATE"   
#>  [7] "LATIT"    "LONGI"    "POSYS"    "WADEP"    "MSTAT"    "MNDEP"   
#> [13] "MXDEP"    "SLABO"    "ACKR_SMP" "SMTYP"    "SMVOL"    "LATNM"   
#> [19] "SFLAG"    "TRPHY"    "COUNT"    "COEFF"    "SIZCL"    "SIZRF"   
#> [25] "SDVOL"    "QFLAG"    "TAXNM"    "METOA"    "ALABO"    "ACKR_ANA"
#> [31] "ANADATE"  "METDC"   
# }
```

# Get a delivery template for a SHARK datatype

Downloads and reads the SHARK Excel delivery template for a given
datatype. The template contains the column definitions and headers used
for submission.

## Usage

``` r
get_delivery_template(
  datatype,
  sheet = "Kolumner",
  header_row = 4,
  skip = 1,
  bacterioplankton_subtype = "abundance",
  force = FALSE,
  clean_cache_days = 1
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

- sheet:

  Character or numeric. Name (e.g., "Kolumner") or index (e.g., 1) of
  the sheet in the Excel file to read. Default is "Kolumner".

- header_row:

  Integer. Row number in the Excel file that contains the column
  headers. Default is 4.

- skip:

  Integer. Number of rows to skip before reading data. Default is 1.

- bacterioplankton_subtype:

  Character. For "Bacterioplankton" only: either "abundance" (default)
  or "production". Ignored for other datatypes.

- force:

  Logical; if `TRUE`, forces re-download even if cached copy exists.

- clean_cache_days:

  Numeric; if not `NULL`, cached template files older than this number
  of days are deleted automatically. Default is 1.

## Value

A `tibble` containing the delivery template. Column names are set from
the header row.

## Examples

``` r
# \donttest{
# Bacterioplankton abundance
abun <- get_delivery_template("Bacterioplankton",
                              bacterioplankton_subtype = "abundance")

print(abun)
#> # A tibble: 3 × 57
#>   `Tabellhuvud:`   MYEAR  STATN STATION_ID SITE_ID PROJ  ORDERER SHIPC CRUISE_NO
#>   <chr>            <chr>  <chr> <chr>      <chr>   <chr> <chr>   <chr> <chr>    
#> 1 * = Obligatorisk *      *     NA         NA      *     *       *     NA       
#> 2 Förklaring:      År (Y… Stat… Övervakni… Provpl… Proj… Bestäl… Prov… Expediti…
#> 3 Tabellhuvud:     MYEAR  STATN STATION_ID SITE_ID PROJ  ORDERER SHIPC CRUISE_NO
#> # ℹ 48 more variables: SERNO <chr>, SDATE <chr>, LATIT <chr>, LONGI <chr>,
#> #   POSYS <chr>, WADEP <chr>, MSTAT <chr>, PURPM <chr>, MPROG <chr>,
#> #   COMNT_VISIT <chr>, WINDIR <chr>, WINSP <chr>, AIRTEMP <chr>, AIRPRES <chr>,
#> #   WEATH <chr>, CLOUD <chr>, WAVES <chr>, ICEOB <chr>, SECCHI <chr>,
#> #   Q_SECCHI <chr>, STIME <chr>, SMPDEP <chr>, SLABO <chr>, ACKR_SMP <chr>,
#> #   SMTYP <chr>, SMVOL <chr>, SMPNO <chr>, COMNT_SAMP <chr>, SPVOL <chr>,
#> #   COEFF <chr>, SUBNO <chr>, METFP <chr>, BCTCNT <chr>, BCTABU <chr>, …

# Bacterioplankton production
prod <- get_delivery_template("Bacterioplankton",
                              bacterioplankton_subtype = "production")

# Phytoplankton template
phyto <- get_delivery_template("Phytoplankton")

# Phytoplankton column explanation (sheet number 3)
phyto_column_explanation <- get_delivery_template("Phytoplankton",
                                                  sheet = 3,
                                                  header_row = 4,
                                                  skip = 3)

print(phyto_column_explanation)
#> # A tibble: 68 × 6
#>    ...1  ...2  `Förklaring/ Svenskt namn`  `Fältnamn/ Kort-namn` Enhet 
#>    <chr> <lgl> <chr>                       <chr>                 <chr> 
#>  1 NA    NA    Förklaring/ Svenskt namn    Fältnamn/ Kort-namn   Enhet 
#>  2 *     NA    År (YYYY)                   MYEAR                 YYYY  
#>  3 NA    NA    NA                          NA                    NA    
#>  4 *     NA    Stationsnamn (text)         STATN                 text  
#>  5 NA    NA    ÖvervakningsstationsID      STATION_ID            nnnnnn
#>  6 NA    NA    ProvplatsID                 SITE_ID               nnnnnn
#>  7 *     NA    Projekt (kod)               PROJ                  kod   
#>  8 *     NA    Beställare (kod)            ORDERER               kod   
#>  9 *     NA    Provtagningsplattform (kod) SHIPC                 kod   
#> 10 NA    NA    Expeditionsnummer (text)    CRUISE_NO             text  
#> # ℹ 58 more rows
#> # ℹ 1 more variable: `Kommentar/ Beskrivning` <chr>
# }
```

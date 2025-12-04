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

A data frame containing the delivery template. Column names are set from
the header row.

## Examples

``` r
if (FALSE) { # \dontrun{
# Bacterioplankton abundance
abun <- get_delivery_template("Bacterioplankton", bacterioplankton_subtype = "abundance")

# Bacterioplankton production
prod <- get_delivery_template("Bacterioplankton", bacterioplankton_subtype = "production")

# Phytoplankton template
phyto <- get_delivery_template("Phytoplankton")

# Phytoplankton column explanation (sheet number 3)
phyto_column_explanation <- get_delivery_template("Phytoplankton",
                                                  sheet = 3,
                                                  header_row = 4,
                                                  skip = 3)
} # }
```

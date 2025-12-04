# Read SHARK Excel delivery files (.xls or .xlsx)

Reads Excel files delivered to SHARK in a standardized format. The
function automatically detects whether the file is `.xls` or `.xlsx` and
reads the specified sheet, skipping a configurable number of rows.
Column types are automatically converted, and if a column `"SDATE"`
exists, it is converted to `Date`.

## Usage

``` r
read_shark_deliv(filename, skip = 2, sheet = 2)
```

## Arguments

- filename:

  Path to the Excel file to be read.

- skip:

  Minimum number of rows to skip before reading anything (column names
  or data). Leading empty rows are automatically skipped, so this is a
  lower bound. Ignored if `sheet` or `range` specifies a starting row.
  Default is 2.

- sheet:

  Sheet to read. Either a string (sheet name) or integer (sheet index).
  If neither is specified, defaults to the second sheet.

## Value

A data frame containing the parsed contents of the Excel file, or `NULL`
if the file does not exist, is empty, or cannot be read.

## See also

[`read_shark()`](https://sharksmhi.github.io/SHARK4R/reference/read_shark.md)
for reading SHARK tab- or semicolon-delimited export files or
zip-archives.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read the second sheet of a .xlsx file (default)
df_xlsx <- read_shark_deliv("shark_delivery.xlsx")

# Read the first sheet of a .xls file, skipping 3 rows
df_xls <- read_shark_deliv("shark_delivery.xls", skip = 3, sheet = 1)
} # }
```

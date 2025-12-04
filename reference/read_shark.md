# Read SHARK export files (tab- or semicolon-delimited, plain text or zipped)

Reads tab- or semicolon-delimited SHARK export files with standardized
format. The function can handle plain text files (`.txt`) or zip
archives (`.zip`) containing a file named `shark_data.txt`. It
automatically detects and converts column types and can optionally
coerce the `"value"` column to numeric. The `"sample_date"` column is
converted to `Date` if it exists.

## Usage

``` r
read_shark(
  filename,
  delimiters = "point-tab",
  encoding = "utf_8",
  guess_encoding = TRUE,
  value_numeric = TRUE
)
```

## Arguments

- filename:

  Path to the SHARK export file. Can be a `.txt` or `.zip` file. If a
  zip file is provided, it should contain a file named `shark_data.txt`.

- delimiters:

  Character. Specifies the delimiter used in the file. Options:
  `"point-tab"` (tab-separated, default) or `"point-semi"`
  (semicolon-separated).

- encoding:

  Character. File encoding. Options: `"cp1252"`, `"utf_8"`, `"utf_16"`,
  `"latin_1"`. Default is `"utf_8"`. If `guess_encoding = TRUE`,
  detected encoding overrides this value.

- guess_encoding:

  Logical. If `TRUE` (default), automatically detect file encoding. If
  `FALSE`, the function uses only the user-specified encoding.

- value_numeric:

  Logical. If `TRUE` (default), attempts to convert the `"value"` column
  to numeric. If `FALSE`, leaves `"value"` as-is.

## Value

A data frame containing the parsed contents of the SHARK export file, or
`NULL` if the file is empty or could not be read.

## Details

This function is robust to file encoding issues. By default
(`guess_encoding = TRUE`), it attempts to automatically detect the file
encoding and will use it if it differs from the user-specified
`encoding`. Automatic detection can be disabled.

## See also

[`read_shark_deliv()`](https://sharksmhi.github.io/SHARK4R/reference/read_shark_deliv.md)
for reading SHARK Excel delivery files (`.xls`/`.xlsx`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Read a plain text SHARK export
df_txt <- read_shark("sharkweb_data.txt")

# Read a SHARK export from a zip archive
df_zip <- read_shark("shark_data.zip")

# Read with explicit encoding and do not convert value
df_custom <- read_shark("shark_data.txt",
                        encoding = "latin_1",
                        guess_encoding = FALSE,
                        value_numeric = FALSE)
} # }
```

# Read .xlsx files delivered to SHARK

**\[deprecated\]** This function is deprecated and has been replaced by
[`read_shark_deliv()`](https://sharksmhi.github.io/SHARK4R/reference/read_shark_deliv.md).

Uses readxl to read excel files with standardized delivery format

## Usage

``` r
shark_read_deliv(filename, skip = 2, sheet = 2)
```

## Arguments

- filename:

  path to file to be read

- skip:

  Minimum number of rows to skip before reading anything, be it column
  names or data. Leading empty rows are automatically skipped, so this
  is a lower bound. Ignored if range is given. Default is 2.

- sheet:

  Sheet to read. Either a string (the name of a sheet), or an integer
  (the position of the sheet). Ignored if the sheet is specified via
  range. If neither argument specifies the sheet, defaults to the second
  sheet.

## Value

Data frame of file

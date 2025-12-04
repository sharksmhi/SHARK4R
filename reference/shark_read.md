# Read tab delimited files downloaded from SHARK

**\[deprecated\]** This function is deprecated and has been replaced by
[`read_shark()`](https://sharksmhi.github.io/SHARK4R/reference/read_shark.md).

## Usage

``` r
shark_read(filename, delimiters = "point-tab", encoding = "latin_1")
```

## Arguments

- filename:

  Path to file to be read.

- delimiters:

  Character. Specifies the delimiter used to separate values in
  `filename`. Options are `"point-tab"` (tab-separated) or
  `"point-semi"` (semicolon-separated). Default is `"point-tab"`.

- encoding:

  Character. Specifies the text encoding of `filename`. Options are
  `"cp1252"`, `"utf_8"`, `"utf_16"`, or `"latin_1"`. Default is
  `"utf_8"`. If an encoding mismatch is detected, the detected encoding
  is used.

## Value

A data frame containing the parsed contents of the SHARK export file.

## Details

Uses `read_delim()` to read tab-delimited or semicolon-delimited files
with standardized export format from SHARK.

This function is robust to encoding issues: it accepts a user-specified
encoding (`cp1252`, `utf_8`, `utf_16`, or `latin_1`) but also attempts
to automatically detect the file encoding. If the detected encoding
differs from the specified one, the detected encoding will be used
instead. This helps in cases where the file encoding has been wrongly
specified, mislabeled, or varies between SHARK exports.

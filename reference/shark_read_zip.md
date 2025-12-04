# Read zip archive and unzip tab delimited files downloaded from SHARK

**\[deprecated\]** This function is deprecated and has been replaced by
[`read_shark()`](https://sharksmhi.github.io/SHARK4R/reference/read_shark.md).

## Usage

``` r
shark_read_zip(zipname, delimiters = "point-tab", encoding = "latin_1")
```

## Arguments

- zipname:

  Path to the zip archive containing SHARK data (expects a file named
  `shark_data.txt` inside).

- delimiters:

  Character. Specifies the delimiter used to separate values in the
  file. Options are `"point-tab"` (tab-separated) or `"point-semi"`
  (semicolon-separated). Default is `"point-tab"`.

- encoding:

  Character. Specifies the text encoding of the file. Options are
  `"cp1252"`, `"utf_8"`, `"utf_16"`, or `"latin_1"`. Default is
  `"utf_8"`. If encoding mismatch is detected, the detected encoding is
  used.

## Value

A data frame containing the parsed contents of the SHARK export file.

## Details

Uses [`unz()`](https://rdrr.io/r/base/connections.html) and
`read_delim()` to extract and read tab-delimited or semicolon-delimited
files with standardized export format from SHARK.

Like
[`shark_read()`](https://sharksmhi.github.io/SHARK4R/reference/shark_read.md),
this function is tolerant to encoding issues. It allows a user-specified
encoding (`cp1252`, `utf_8`, `utf_16`, or `latin_1`), but also
automatically detects the encoding from the file content. If the
detected encoding does not match the specified one, the detected
encoding is preferred. This ensures files with wrongly labeled or
inconsistent encodings are still read correctly.

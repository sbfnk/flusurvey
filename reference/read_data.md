# This reads in flusurvey data for a given year

This reads different surveys and returns a list of data tables
containing the surveys. It does minimal cleaning and conversion into R
data formats. The csv files that should be provided can be obtained via
SQL dump, e.g. \copy pollster_results_intake to 'filename.csv' csv
header

## Usage

``` r
read_data(files, year, ...)
```

## Arguments

- files:

  a (named!) list of files. The names stand for the surveys (e.g.,
  "symptom", "background", "contact", etc.)

- year:

  the year from which the data come

- ...:

  options to be passed to `read.csv`

## Value

a list of data tables with the data

## Details

and similarly for weekly surveys

## Author

seb

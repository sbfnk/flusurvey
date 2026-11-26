# Simulated FluSurvey data

A small simulated data set in the format that
[`extract_data`](https://sbfnk.github.io/flusurvey/reference/extract_data.md)
reads: a list with one element per season (named "2013" and "2014", for
the 2012/13 and 2013/14 seasons), each a list of the "background",
"contact" and "symptom" questionnaires as returned by
[`read_data`](https://sbfnk.github.io/flusurvey/reference/read_data.md).
There are around 300 participants per season, of whom about 70 per cent
take part in both seasons.

## Usage

``` r
simulated_data
```

## Format

A named list of seasons, each a named list of
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)s

## Details

The data are not derived from any survey responses.
`data-raw/simulated_data.r` generates them from the questionnaire
definitions in `questions` and `options` alone, with a fixed random
seed. The values are only plausible enough for the functions in this
package to run on them. Do not use them to draw any conclusion about
influenza-like illness.

## Examples

``` r
names(simulated_data)
#> [1] "2013" "2014"
sapply(simulated_data[["2013"]], nrow)
#> background    contact    symptom 
#>        317        937       2879 
```

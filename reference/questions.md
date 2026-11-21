# Question codes of the FluSurvey questionnaires

Readable names for the question codes, by season and questionnaire.
[`read_data`](https://sbfnk.github.io/flusurvey/reference/read_data.md)
uses them to rename the columns of the files it reads.

## Usage

``` r
questions
```

## Format

A list with one element per season (named by the year in which the
season ends), each a list with one named character vector per
questionnaire that maps question codes to readable names

## Source

`R/define_questions.r`

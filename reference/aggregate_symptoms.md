# Aggregate symptoms in symptom table to work out ILI etc.

The recorded onset is kept in the column `onset`: "sudden",
"not.sudden", "dont_know" (an explicit answer), "not.answered" (the
question was asked but not answered) or "no.question" (the data have no
onset question). None of the last three establishes sudden onset.
`ili.sudden.unknown` sets how the last three are interpreted in the
derived columns `suddenly`, `ili` and `ili.fever`. It leaves `onset` and
the answers in the data unchanged. A report that already fails another
ILI criterion is not ILI, whatever its onset.

## Usage

``` r
aggregate_symptoms(symptoms, ili.sudden.unknown = NA)
```

## Arguments

- symptoms:

  a data table of symptom reports

- ili.sudden.unknown:

  how to interpret reports where onset is unknown (see above): `NA` (the
  default) leaves `suddenly`, `ili` and `ili.fever` missing where onset
  is needed to decide, `1` counts unknown onset as sudden and `0` counts
  it as not sudden.

## Value

a data table with added columns, including `onset`

## Author

seb

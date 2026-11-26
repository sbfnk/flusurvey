# Evidence behind the bouts of illness

[`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md)
gives one row per bout. The evidence it rests on is in the list returned
here, from the same reconstruction. The bout membership is the same as
in `bouts_of_illness(x)`. Date policies affect selected boundaries, not
the original report evidence.

## Usage

``` r
bout_evidence(
  x,
  start.date = c("earliest", "last"),
  missing.start = c("missing", "first.report"),
  inverted.dates = c("keep", "clear", "reset.end"),
  expand.days = TRUE,
  precedence = c("dates", "continuation", "report.date"),
  unresolved = c("fill", "missing"),
  chronic = c("none", "duration"),
  chronic.days = NULL
)
```

## Arguments

- x:

  the data to extract bouts from, as for
  [`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md)

- start.date, missing.start, inverted.dates:

  Date policies as in
  [`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md);
  `start.date` defaults to "earliest" in both. Evidence defaults to
  leaving an unreported onset missing; the summary interface defaults to
  the first report. These policies do not replace the original report
  evidence.

- expand.days:

  whether to expand established illness intervals to individual days.
  Set FALSE for interval matching without daily expansion.

- precedence, unresolved:

  Bout formation and treatment of a bout with no reported end date, as
  in
  [`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md).

- chronic, chronic.days:

  Chronic report rule as in
  [`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md).

## Value

a list of data tables: `bouts`, `reports`, `intervals` and `days`

## Details

`reports` has a row for each symptomatic report that belongs to a bout,
with the bout it belongs to, the dates it reported (`report.start`,
`report.end`) and the days of illness it supports, `first.day` to
`last.day`. The column `basis` says how: "reported" (start and end date
given, the end counted no later than the report), "still.ill"
(continuing illness from the start date to the report),
"still.ill.unknown.onset" (continuing illness with no start date: only
the day of the report is established, and neither the onset nor the
recovery) or "unresolved" (no usable interval, so no day).

`days` has a row for each day of illness the reports establish, with
`source` "reported" for days within a reported or continuing interval
and "report.date" for days established only by a report of continuing
illness without a start date. Days between the intervals of a bout are
included with source "within.bout": illness persists between established
illness days of the same bout. Selected fallback boundaries are not used
to establish illness. The `intervals` table contains the first and last
established day per bout without daily expansion.

`bouts` has a row per bout with dates selected by the supplied date
policies, and `onset.known` and `recovery.known`, which say whether any
report of the bout gave a matching start or end date for the selected
boundary. The columns `onset.source` and `recovery.source` distinguish
reported dates, missing boundaries and first/last-report fallbacks. The
columns `n.reports` and the case definitions follow: the symptom columns
of all reports of a bout are combined (a symptom counts if any report
has it, the onset if any report records it as sudden) and the
definitions applied to the result. The classification belongs to the
bout and does not say that the symptoms were present together on a day.
The definitions are `ili`, `ili.fever`, `ili.self`, `resp.symptoms`
(ARI) and the others of
[`aggregate_symptoms`](https://sbfnk.github.io/flusurvey/reference/aggregate_symptoms.md)
that are in the data; `onset.conflict` records conflicting explicit
onset answers; `onset` keeps the highest-precedence onset answer
("sudden", "not.sudden", "dont_know", "not.answered", "no.question").
Where the columns that a definition is built from are not in the data,
it is positive if any report of the bout is.

## Examples

``` r
dt <- extract_data(simulated_data, years = 2013:2014)
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use contact_illness().
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use contact_illness().
evidence <- bout_evidence(dt)
head(evidence$days)
#>    participant_id  season  bout        day   source
#>             <int>  <char> <int>     <Date>   <char>
#> 1:              2 2013/14     1 2014-02-09 reported
#> 2:              2 2013/14     1 2014-02-10 reported
#> 3:              2 2013/14     1 2014-02-11 reported
#> 4:              2 2013/14     1 2014-02-12 reported
#> 5:              2 2013/14     1 2014-02-13 reported
#> 6:              2 2013/14     1 2014-02-14 reported
```

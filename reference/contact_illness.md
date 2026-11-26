# Illness status of contact reports

Classifies the day described by each contact report (`contact.date`, the
day before the report) as ill, healthy or unknown, following a
longitudinal model of each participant in each season. A symptom report
is not needed on the contact day itself.

## Usage

``` r
contact_illness(
  contacts,
  symptoms,
  min.reports = 3,
  healthy = c("follow-up", "report"),
  chronic = c("none", "duration"),
  chronic.days = NULL,
  follow_up_end = c("last_healthy", "end_of_illness")
)
```

## Arguments

- contacts:

  a data table of contact reports with columns `global_id` and
  `contact.date`, and `season` if the symptom reports have one

- symptoms:

  a data table of symptom reports, as returned by
  [`extract_data`](https://sbfnk.github.io/flusurvey/reference/extract_data.md),
  with columns `global_id`, `date`, `no.symptoms`,
  `symptoms.start.date`, `symptoms.end.date` and `symptoms.end.option`.
  The columns `ili` (symptom-based ILI), `ili.fever` (symptom-based ILI
  with fever), `ili.self` (self-assessed ILI) and `resp.symptoms` (ARI)
  are used where present; a missing column gives missing flags. The
  column `same` (the answer to whether the symptoms are the same as in
  the previous report) is used where present, and the symptom columns
  that
  [`aggregate_symptoms`](https://sbfnk.github.io/flusurvey/reference/aggregate_symptoms.md)
  derives the definitions from, so that bouts can be reconstructed and
  classified as in
  [`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md).

- min.reports:

  the minimum number of days with symptom reports for a participant to
  be eligible in a season

- healthy:

  the rule for a day outside a bout within follow-up: "follow-up" counts
  it as healthy, "report" only if a later report without symptoms covers
  it

- chronic:

  an optional heuristic for chronic symptom reports: `"none"` (the
  default) applies no rule; `"duration"` treats reports with a valid
  interval of more than `chronic.days` days as chronic. This is an
  assumption: a long duration alone does not show that symptoms are
  chronic, and no rule for identifying chronic-only reports has been
  agreed. A chronic condition is deliberately unused, because people
  with one can also have an acute illness.

- chronic.days:

  the number of days beyond which a report counts as chronic, for
  `chronic = "duration"`

- follow_up_end:

  where follow-up ends: `"last_healthy"` (the default) at the last
  report without symptoms, or `"end_of_illness"` at the later of that
  report and the last established illness day. A reported recovery is
  capped at the report date; a continuing illness without a recovery
  establishes illness only through the days supported by reports.
  Contact days after follow-up have unknown status. Reports excluded as
  chronic do not extend follow-up.

## Value

the contact reports with added columns `follow_up` (see above),
`current_symptoms` ("symptoms" or "no symptoms"), and a daily column
`current_*` and a bout column `bout_*` for each of `ili`
(symptom-based), `ili_fever` (symptom-based with fever), `ili_self`
(self-assessed), all "ILI" or "no ILI", and `ari` ("ARI" or "no ARI").
Each is missing where the information is lacking

## Details

*Eligibility and follow-up.* A participant is eligible in a season if
they filed symptom reports on at least `min.reports` different days and
at least one of them reported no symptoms. Follow-up runs from the first
report without symptoms to the end given by `follow_up_end`. Bouts are
reconstructed from all symptom reports, including those of ineligible
participants and those outside follow-up; eligibility and follow-up only
decide which contact days are classified. The column `follow_up` records
the result: "in follow-up", "before follow-up", "after follow-up" or
"not eligible". Contact days outside follow-up have an unknown status
and are never called healthy.

*Bouts.* Bouts and the days of illness come from
[`bout_evidence`](https://sbfnk.github.io/flusurvey/reference/bout_evidence.md),
the same reconstruction as in
[`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md):
the answer to whether the symptoms are the same as at the previous
report, the dates of onset and recovery, and intervening reports without
symptoms decide which reports form a bout. A report with a start and an
end date (the end counted no later than the report) supports the days
between them. A report of continuing illness supports the days from its
start date to the report date, or only the report date when it gives no
start date: the onset is then unknown and the days before it are
unknown, but the report date is a day of illness, and a contact
describing it is classified as ill. Recovery on that date, or illness on
the following one, is not established. Days between known illness days
of the same bout are included as continuing illness. The first
symptomatic report is retained by the shared reconstruction even when
onset is unknown. Missing onset may be recovered from other reports
assigned to that same bout.

*Case definitions of bouts.* The symptoms of all reports of a bout are
combined first and the definitions applied to the result, so symptoms
reported in different questionnaires of one bout can jointly meet a
definition (see
[`bout_evidence`](https://sbfnk.github.io/flusurvey/reference/bout_evidence.md)).
Where the data lack the symptom columns a definition is built from, it
is positive if any report of the bout is. Two adjacent illnesses that
the answers separate keep their own definitions.

*Daily status.* Within follow-up, `current_symptoms` is "symptoms" on
days in a bout. With `healthy = "follow-up"` every other day is "no
symptoms", with or without a report that day. With `healthy = "report"`
a day is "no symptoms" only if a report without symptoms filed after it
covers it, from the date of the previous report up to the day before its
own. Days before a participant's first report, and days with no such
report, are unknown. The exception is a report with symptoms (or without
a known symptom status) whose dates give no usable interval: missing,
inconsistent, or an unknown end that is not "still ill". Its illness
cannot be placed, so every day since the participant's previous report,
up to and including its own date, is unknown (missing) unless it lies in
a bout. The same holds for the days before the report date of a
continuing illness without a start date. A continuing-illness report
also leaves the days after its date until the next report unknown unless
bout evidence covers them. A report without symptoms is taken as no
acute illness. By default (`chronic = "none"`) the function applies no
rule for chronic symptoms. With `chronic = "duration"`, the function
treats reports flagged by that assumption as no acute illness. Such
reports support no bout and leave no day unknown.

The case-definition flags are known per report, not per day. A bout
meeting a definition at some point need not have had every symptom
present on every day of it. The `bout_` columns therefore describe the
whole bout and are the ones for bout-based exposure: positive on every
day of a bout that meets the definition on the combined symptoms of its
reports, negative if the combined evidence rules it out, and missing
otherwise. Uncertain report dates do not change a bout flag on only some
days. The daily `current_` columns are positive or negative within a
bout only on dates with a symptom report whose interval covers that
date, assuming that such a report describes its own day, and are missing
on other days of a bout: the symptoms of those days are not known, but
the participant is known to be ill. Several reports on a day are
combined: positive if any is positive, otherwise missing if any is
unknown. Outside bouts, within follow-up and where status is not
unknown, both are negative. The four definitions (symptom-based ILI,
with fever, self-assessed ILI and ARI) stay separate.

Read the symptom data with a `clean` argument in
[`extract_data`](https://sbfnk.github.io/flusurvey/reference/extract_data.md)
that cannot drop symptom reports, as the default cleaning (e.g.,
`"remove.first"`) can remove reports that support a bout. Use
`clean = c("unsuccessful.join", "remove.bad.symptom.dates")` to clear
invalid symptom dates without dropping reports when reading a single
survey. Cleared dates leave the report without a usable interval, which
is handled as above. If both tables have a `season` column, eligibility,
follow-up and bouts are determined per participant and season.

## Examples

``` r
contacts <- extract_data(
  simulated_data, years = 2013:2014, surveys = c("background", "contact")
)
symptoms <- extract_data(
  simulated_data, years = 2013:2014, surveys = "symptom",
  clean = c("unsuccessful.join", "remove.bad.symptom.dates")
)
table(contact_illness(contacts, symptoms)$current_symptoms, useNA = "ifany")
#> 
#> no symptoms    symptoms        <NA> 
#>        1012          71         742 
```

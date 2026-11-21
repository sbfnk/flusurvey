# Illness status of contact reports

Classifies the day described by each contact report (`contact.date`, the
day before the report) as ill, healthy or unknown, following a
longitudinal model of each participant in each season. A symptom report
is not needed on the contact day itself.

## Usage

``` r
contact_illness(contacts, symptoms, min.reports = 3)
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
  are used where present; a missing column gives missing flags.

- min.reports:

  the minimum number of days with symptom reports for a participant to
  be eligible in a season

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
to the last report without symptoms. The last report without symptoms is
the default end of follow-up; the end of the last illness is not used as
an alternative. All symptom reports, including those of ineligible
participants and those outside follow-up, are used to reconstruct bouts;
eligibility and follow-up only decide which contact days are classified.
The column `follow_up` records the result: "in follow-up", "before
follow-up", "after follow-up" or "not eligible". Contact days outside
follow-up have an unknown status and are never called healthy.

*Bouts.* A bout is built from all symptom reports of a participant in a
season: each report with symptoms supports the interval from its
`symptoms.start.date` to its `symptoms.end.date`, or to the report
`date` if the participant was still ill. Overlapping or adjacent
intervals are merged into one bout. This differs from
[`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md),
which splits and joins reports by the answer to whether the symptoms are
the same as at the previous report and by intervening reports without
symptoms, and keeps one row per bout. Here a bout is a period of days,
so that contact days can be located within it, and a participant
reporting no symptoms between two overlapping intervals does not split
them.

*Daily status.* Within follow-up, `current_symptoms` is "symptoms" on
days in a bout and "no symptoms" on every other day, with or without a
report that day. The exception is a report with symptoms (or without a
known symptom status) whose dates give no usable interval: missing,
inconsistent, or an unknown end that is not "still ill". Its illness
cannot be placed, so every day since the participant's previous report,
up to and including its own date, is unknown (missing) unless it lies in
a bout. A report without symptoms is taken as no acute illness. The data
hold no indicator of chronic symptoms, and the maintainers have not
defined one (nor a duration after which symptoms count as chronic), so
no rule for them is applied.

The case-definition flags are known per report, not per day. A bout
meeting a definition at some point need not have had every symptom
present on every day of it. The `bout_` columns therefore describe the
whole bout and are the ones for bout-based exposure: positive on every
day of a bout if any report in it meets the definition, negative if all
reports have it unmet and none is unresolved, and missing otherwise. The
daily `current_` columns are positive or negative within a bout only on
days with a symptom report whose interval includes that day, assuming
that such a report describes its own day, and are missing on other days
of a bout: the symptoms of those days are not known, but the participant
is known to be ill. Several reports on a day are combined: positive if
any is positive, otherwise missing if any is unknown. Outside bouts,
within follow-up and where status is not unknown, both are negative. The
four definitions (symptom-based ILI, with fever, self-assessed ILI and
ARI) stay separate.

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
#>        1301          27         708 
```

# Extract bouts of illness from a flusurvey data table

Extract bouts of illness from a flusurvey data table

## Usage

``` r
bouts_of_illness(
  x,
  symptomatic.only = TRUE,
  as.data.frame = TRUE,
  start.date = c("earliest", "last"),
  missing.start = c("first.report", "missing"),
  inverted.dates = c("keep", "clear", "reset.end"),
  daily.flags = c("none", "carry.forward", "bout", "report"),
  precedence = c("dates", "continuation", "report.date"),
  unresolved = c("fill", "missing"),
  chronic = c("none", "duration"),
  chronic.days = NULL
)
```

## Arguments

- x:

  the data to extract bouts from

- symptomatic.only:

  whether to only include symptomatic periods

- as.data.frame:

  currently has no effect

- start.date:

  which reported start date to use for a bout: the "earliest" one
  reported (the default, because the last reported date can fall after a
  report that already gives symptoms of the bout), or the "last" one

- missing.start:

  what to use as start date if none was reported for a bout: the date of
  its "first.report", or "missing"

- inverted.dates:

  what to do if the end date is before the start date: "keep" both, set
  both to "clear" (missing), or "reset.end" to the last report date of
  the bout

- daily.flags:

  how to assign the case-definition flags set by
  [`aggregate_symptoms`](https://sbfnk.github.io/flusurvey/reference/aggregate_symptoms.md)
  to the days of a bout. `"none"` returns one row per bout. The other
  values return one row per day, in the column `day`, from
  `symptoms.start.date` to `symptoms.end.date`. `"carry.forward"` takes
  the flags from the last report on or before the day. `"bout"` gives
  every day the flags of the bout. `"report"` sets flags only on days
  with a report. Missing or inverted selected intervals are not
  expanded.

- precedence:

  how a report joins the previous bout. A report answered "no" always
  starts a new bout. Otherwise, with "dates", a report whose start date
  is after the previous end date starts a new bout, even if it is
  answered "yes". With "continuation", a report answered "yes" stays in
  the bout whatever its dates. With "report.date", a report whose start
  date is after the previous report date starts a new bout.

- unresolved:

  what to do with a bout with no reported end date: "fill" its end with
  the last report date, or leave it "missing"

- chronic:

  Rule for chronic-only reports: "none" applies no rule; "duration"
  excludes symptomatic reports with a valid interval longer than
  `chronic.days` days from bout reconstruction. Duration alone does not
  establish that symptoms are chronic.

- chronic.days:

  Number of days beyond which a report is excluded when
  `chronic = "duration"`.

## Value

a data table of bouts of illness, or of their days

## Details

The flags of a bout (those of the case definitions in
[`aggregate_symptoms`](https://sbfnk.github.io/flusurvey/reference/aggregate_symptoms.md))
are computed from the symptoms of all its reports together: a bout meets
the symptom-based ILI definition if its reports jointly have the
symptoms and a sudden onset, even if no single report does. The flags
describe the bout and do not say that the symptoms were present together
on one day. The days of illness that the reports establish, and which
dates are reported and which only the date of a report, are in
[`bout_evidence`](https://sbfnk.github.io/flusurvey/reference/bout_evidence.md).

## Author

seb

## Examples

``` r
dt <- extract_data(simulated_data, years = 2013:2014)
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use contact_illness().
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use contact_illness().
bouts <- bouts_of_illness(dt)
#> Merging columns:
#>    no.education 
#>    education.gcse 
#>    education.alevels 
#>    education.bsc 
#>    education.msc 
#>    education.stillin 
#>    frequent.contact.children 
#>    frequent.contact.elderly 
#>    frequent.contact.patients 
#>    frequent.contact.people 
#>    frequent.contact.none 
#>    household.0.4 
#>    household.5.18 
#>    household.19.44 
#>    household.45.64 
#>    household.65+ 
#>    why.vaccine.riskgroup 
#>    why.vaccine.protected 
#>    why.vaccine.protect.others 
#>    why.vaccine.doctor 
#>    why.vaccine.work.recommended 
#>    why.vaccine.convenient 
#>    why.vaccine.free 
#>    why.vaccine.nomiss.work 
#>    why.vaccine.always 
#>    why.vaccine.other 
#>    why.not.vaccine.notyet 
#>    why.not.vaccine.notoffered 
#>    why.not.vaccine.norisk 
#>    why.not.vaccine.natural 
#>    why.not.vaccine.noteffective 
#>    why.not.vaccine.minor 
#>    why.not.vaccine.unlikely 
#>    why.not.vaccine.cause 
#>    why.not.vaccine.side.effects 
#>    why.not.vaccine.dont.like 
#>    why.not.vaccine.unavailable 
#>    why.not.vaccine.not.free 
#>    why.not.vaccine.no.reason 
#>    why.not.vaccine.doctor 
#>    why.not.vaccine.other 
#>    risk.none 
#>    risk.asthma 
#>    risk.diabetes 
#>    risk.lung 
#>    risk.heart 
#>    risk.kidney 
#>    risk.immune 
#>    allergy.hayfever 
#>    allergy.dust 
#>    allergy.animals 
#>    allergy.other 
#>    allergy.none 
#>    diet.none 
#>    diet.vegetarian 
#>    diet.vegan 
#>    diet.low-calorie 
#>    diet.other 
#>    pets.none 
#>    pets.dogs 
#>    pets.cats 
#>    pets.birds 
#>    pets.other 
#>    howhear.radio.tv 
#>    howhear.paper.magazine 
#>    howhear.internet 
#>    howhear.poster 
#>    howhear.family.friends 
#>    howhear.other 
#>    no.symptoms 
#>    fever 
#>    chills 
#>    blocked.runny.nose 
#>    sneezing 
#>    sore.throat 
#>    cough 
#>    shortness.breath 
#>    headache 
#>    muscle.and.or.joint.pain 
#>    chest.pain 
#>    tired 
#>    loss.appetite 
#>    phlegm 
#>    watery.eyes 
#>    nausea 
#>    vomiting 
#>    diarrhoea 
#>    stomach.ache 
#>    other.symptoms 
#>    visit.medical.service.no 
#>    visit.medical.service.gp 
#>    visit.medical.service.ae 
#>    visit.medical.service.hospital 
#>    visit.medical.service.other 
#>    visit.medical.service.appointment 
#>    contact.medical.service.no 
#>    contact.medical.service.gp.receptionist 
#>    contact.medical.service.gp.doctor 
#>    contact.medical.service.nhs 
#>    contact.medical.service.npfs 
#>    contact.medical.service.other 
#>    no.medication 
#>    medication.painkillers 
#>    medication.cough 
#>    medication.antiviral 
#>    medication.antibiotic 
#>    medication.other 
#>    medication.dontknow 
#>    suddenly 
#>    fever.symptoms 
#>    systemic.symptoms 
#>    resp.symptoms 
#>    general.symptoms 
#>    ili.symptoms 
#>    ili 
#>    ili.fever 
#>    ili.self 
#>    gi 
#>    why.vaccine.given.at.school 
#>    howhear.school.work 
#>    howhear.bsa 
cols <- c("participant_id", "season", "symptoms.start.date",
          "symptoms.end.date")
head(as.data.frame(bouts)[, cols])
#>   participant_id  season symptoms.start.date symptoms.end.date
#> 1              1 2012/13          2013-03-22        2013-03-22
#> 2              2 2013/14          2014-02-09        2014-02-15
#> 3              5 2013/14          2014-01-25        2014-02-05
#> 4              7 2013/14          2013-11-22        2013-11-17
#> 5             12 2012/13          2013-01-27        2013-01-27
#> 6             12 2013/14          2014-01-10        2014-01-18
```

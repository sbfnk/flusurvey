# Flag contact reports made on a day of illness

A contact report describes the contacts of one day, so whether the
participant was ill on that day comes from their episodes of illness,
not from joining the contact and symptom surveys by report date (see
[`merge_data`](https://sbfnk.github.io/flusurvey/reference/merge_data.md)).
This matches the contact day of each contact report against the start
and end dates of the participant's episodes.

## Usage

``` r
flag_ill_contacts(contacts, bouts)
```

## Arguments

- contacts:

  a data table of contact reports, usually from
  `extract_data(..., surveys = "contact")` or with the background survey
  too. It needs the columns `global_id` and `contact.date`.

- bouts:

  the episodes of illness, as returned by
  [`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md)
  for the symptom survey of the same participants (extracted without the
  contact survey). It needs the columns `global_id`,
  `symptoms.start.date` and `symptoms.end.date`. Episodes with a missing
  start or end date are not matched.

## Value

`contacts` as a data table with the logical column `ill` added: `TRUE`
if the contact day falls on or between the start and end dates of any
episode of the same participant, `FALSE` otherwise (including
participants without episodes). The rows are kept in their original
order.

## Author

sbfnk

## Examples

``` r
symptoms <- extract_data(
  simulated_data, years = 2013, surveys = c("background", "symptom")
)
contacts <- extract_data(
  simulated_data, years = 2013, surveys = c("background", "contact")
)
bouts <- bouts_of_illness(symptoms)
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
table(flag_ill_contacts(contacts, bouts)$ill)
#> 
#> FALSE  TRUE 
#>  1010    12 
```

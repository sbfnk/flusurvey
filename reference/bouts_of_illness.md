# Extract bouts of illness from a flusurvey data table

Extract bouts of illness from a flusurvey data table

## Usage

``` r
bouts_of_illness(x, symptomatic.only = TRUE, as.data.frame = TRUE)
```

## Arguments

- x:

  the data to extract bouts from

- symptomatic.only:

  whether to only include symptomatic periods

- as.data.frame:

  currently has no effect

## Value

a data table of bouts of illness

## Author

seb

## Examples

``` r
dt <- extract_data(simulated_data, years = 2013:2014)
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use flag_ill_contacts().
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use flag_ill_contacts().
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
#> 1              3 2012/13          2012-11-28        2012-11-30
#> 2              3 2012/13          2012-12-29        2013-01-05
#> 3              5 2012/13          2013-01-01              <NA>
#> 4              5 2012/13          2013-02-19              <NA>
#> 5              7 2012/13          2012-12-16              <NA>
#> 6              8 2013/14          2013-08-13        2013-11-11
```

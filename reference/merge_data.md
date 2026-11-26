# Clean and merge flusurvey data tables

Clean and merge flusurvey data tables

## Usage

``` r
merge_data(
  data,
  clean = c("remove.first", "remove.bad.symptom.dates", "remove.bad.health.scores",
    "limit.season", "remove.postcodes", "n.reports", "unsuccessful.join",
    "only.symptoms"),
  min.reports = 3,
  ranges = "sample",
  age.breaks = c(18, 44, 64),
  ili.sudden.unknown = NA,
  max.band.count = Inf
)
```

## Arguments

- data:

  the data to merge and clean, usually the result of `read_data`. The
  surveys are joined in a fixed order (background, contact, symptom),
  regardless of their order in `data`. The last one present drives a
  rolling join. The result has one row per report of that survey (the
  symptom reports, if present), each taking the latest report of the
  other surveys on or before its date. For several reports from an
  earlier survey on the same day, the last report is used. Any other
  survey name gives an error.

  Joining the contact and symptom surveys gives a warning. The join
  pairs each symptom report with the latest earlier contact report,
  however long before it was filed, and a contact report describes one
  day whereas a symptom report describes the week before it. With
  'unsuccessful.join' the symptom reports without an earlier contact
  report are dropped, shortening episodes of illness. To link the two,
  extract the symptom and contact surveys separately and classify the
  contact reports with
  [`contact_illness`](https://sbfnk.github.io/flusurvey/reference/contact_illness.md).

- clean:

  cleaning options, NULL for no cleaning, otherwise a vector of cleans
  to perform (by default all): - 'remove.first', whether to remove
  reports from the first reporting day, - 'remove.bad.symptom.dates',
  whether to remove bad symptom dates: symptom end of symptoms before
  start of symptoms, symptoms reported to have started or ended after
  date of report. Such dates are set to missing. Other start dates are
  kept as reported, and the logical column `illness.ongoing.at.entry`
  flags those before the participant's first report in the data, after
  any season limiting. When the first reporting day is removed, its flag
  is copied to reports in the same bout under the default bout boundary
  rules.
  [`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md)
  copies the flag to the bouts.
  [`get_incidence`](https://sbfnk.github.io/flusurvey/reference/get_incidence.md)
  excludes bouts with `illness.ongoing.at.entry` from the incidence if
  called with `exclude = "ongoing.at.entry"`. -
  'remove.bad.health.scores', whether to remove health scores \<0 or
  \>100 - 'limit.season', whether to limit a flu season to November -\>
  April (when surveys are merged, the limit applies to the last survey
  merged: the symptom survey, if present) - 'remove.postcodes', whether
  to remove postcodes - 'n.reports', whether to exclude those with fewer
  than `min.reports` symptom reporting days - 'unsuccessful.join',
  whether to exclude those with unsuccesful joins (e.g. if symptoms are
  reported without a background survey present; the web site should have
  prevented this, but doesn't appear to have done so) - 'only.symptoms',
  whether to exclude those that have no report without symptoms in a
  season. Someone who is ill at every report may have joined or reported
  only because they were ill, so keeping them may introduce selection
  bias, most likely upwards in incidence and prevalence. Leave it out of
  `clean` for analyses that need these participants.

- min.reports:

  minimum number of symptom reporting days per participant (ignored if
  'min.reports' is not given as a cleaning option)

- ranges:

  what to do if a number of contacts is given as a range: sample from
  the range ("sample"), or apply a function, given as character string
  (e.g., "mean")

- age.breaks:

  a vector of the highest age (in whole years) in each age group except
  the last (first age group starts at 0 years of age). The last group is
  open-ended. The default gives the groups 0-18, 19-44, 45-64 and 65+,
  matching the age bands of the contact questions.

- ili.sudden.unknown:

  how to interpret reports where sudden onset is unknown, passed to
  [`aggregate_symptoms`](https://sbfnk.github.io/flusurvey/reference/aggregate_symptoms.md):
  `NA` (the default) leaves `ili` and `ili.fever` missing where onset is
  needed to decide, `1` counts unknown onset as sudden and `0` as not
  sudden. The recorded onset is kept in `onset`.

- max.band.count:

  the largest number of people accepted in a single age band of the
  household. There is no limit by default, as households in institutions
  such as care homes can be large. Larger counts are set to missing.
  `nb.household` is then missing too. The counts include the
  participant. A band ticked as present without a number counts as one
  person. A household with no band ticked gets a missing `nb.household`.

## Value

a rolling-joined data table. `day.of.week` (1 for Monday to 7 for
Sunday) is that of the report date. If the contact survey is included,
`contact.date` is the day the contacts describe (the day before the
report) and `contact.day.of.week` is its day of the week. Use these
rather than `day.of.week` when analysing contacts.

## Author

seb

## Examples

``` r
dt <- merge_data(simulated_data[["2013"]])
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use contact_illness().
table(dt$ili)
#> 
#>    f    t 
#> 2172   93 

## only remove each participant's first reporting day
merge_data(simulated_data[["2013"]], clean = "remove.first")
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use contact_illness().
#> Key: <date, global_id>
#>       work.postcode postcode background.id global_id         self gender
#>              <char>   <char>         <int>    <char>       <fctr> <fctr>
#>    1:            E7     LE10           145   sim0146         self female
#>    2:          NE92     WC1B           282   sim0284         self   male
#>    3:          <NA>      S32            19   sim0019         self female
#>    4:          SO42      G22            38   sim0038         self female
#>    5:          EC3V      E17            79   sim0080 someone_else female
#>   ---                                                                   
#> 2555:          <NA>     SE26           229   sim0230         self female
#> 2556:          <NA>     EC3M           250   sim0252         self   male
#> 2557:           M26      TR1           266   sim0268         self   male
#> 2558:          <NA>      B50           295   sim0298         self female
#> 2559:          <NA>       S4           304   sim0307         self female
#>       birthmonth             main.activity work.postcode.option    occupation
#>           <char>                    <fctr>               <fctr>        <fctr>
#>    1:    1967-05 paid_employment_full_time                  yes  other_manual
#>    2:    1958-01 paid_employment_part_time                  yes  professional
#>    3:    1996-07                    school                 <NA>          <NA>
#>    4:    1976-02 paid_employment_full_time                  yes  professional
#>    5:    1963-04 paid_employment_full_time                  yes  other_manual
#>   ---                                                                        
#> 2555:    1928-04                   retired                 <NA>          <NA>
#> 2556:    1946-12                   retired                 <NA>          <NA>
#> 2557:    1976-01 paid_employment_full_time                  yes office_worker
#> 2558:    1957-05                     other                 <NA>          <NA>
#> 2559:    1928-05                   retired                 <NA>          <NA>
#>       no.education education.gcse education.alevels education.bsc education.msc
#>             <fctr>         <fctr>            <fctr>        <fctr>        <fctr>
#>    1:            f              t                 f             f             f
#>    2:            f              f                 t             f             f
#>    3:            f              f                 f             f             f
#>    4:            f              f                 t             f             f
#>    5:            f              f                 f             t             f
#>   ---                                                                          
#> 2555:            f              f                 f             f             t
#> 2556:            f              f                 f             t             f
#> 2557:            f              f                 f             f             t
#> 2558:            f              f                 t             f             f
#> 2559:            f              f                 f             t             f
#>       education.stillin frequent.contact.children frequent.contact.elderly
#>                  <fctr>                    <fctr>                   <fctr>
#>    1:                 f                         f                        f
#>    2:                 f                         f                        f
#>    3:                 t                         f                        f
#>    4:                 f                         f                        f
#>    5:                 f                         t                        f
#>   ---                                                                     
#> 2555:                 f                         f                        f
#> 2556:                 f                         f                        t
#> 2557:                 f                         f                        f
#> 2558:                 f                         f                        f
#> 2559:                 f                         t                        f
#>       frequent.contact.patients frequent.contact.people frequent.contact.none
#>                          <fctr>                  <fctr>                <fctr>
#>    1:                         f                       f                     t
#>    2:                         f                       f                     t
#>    3:                         f                       f                     t
#>    4:                         f                       f                     t
#>    5:                         f                       f                     f
#>   ---                                                                        
#> 2555:                         f                       f                     t
#> 2556:                         f                       f                     f
#> 2557:                         f                       t                     f
#> 2558:                         f                       f                     t
#> 2559:                         f                       f                     f
#>       household.0.4 nb.household.0.4 household.5.18 nb.household.5.18
#>              <fctr>            <int>         <fctr>             <int>
#>    1:             f                0              f                 0
#>    2:             f                0              t                 1
#>    3:             f                0              f                 0
#>    4:             t                1              f                 0
#>    5:             t                1              f                 0
#>   ---                                                                
#> 2555:             f                0              f                 0
#> 2556:             f                0              f                 0
#> 2557:             f                0              t                 1
#> 2558:             t                1              f                 0
#> 2559:             f                0              f                 0
#>       household.19.44 nb.household.19.44 household.45.64 nb.household.45.64
#>                <fctr>              <int>          <fctr>              <int>
#>    1:               t                  1               f                  0
#>    2:               f                  0               t                  1
#>    3:               t                  2               f                  0
#>    4:               t                  1               f                  0
#>    5:               t                  1               f                  0
#>   ---                                                                      
#> 2555:               f                  0               t                  1
#> 2556:               f                  0               t                  1
#> 2557:               t                  2               f                  0
#> 2558:               f                  0               f                  0
#> 2559:               f                  0               t                  1
#>       household.65+ nb.household.65+ children.school        transport
#>              <fctr>            <int>          <fctr>           <fctr>
#>    1:             t                1               4            other
#>    2:             f                0            none              car
#>    3:             f                0            none              car
#>    4:             f                0            none public_transport
#>    5:             f                0            none          walking
#>   ---                                                                
#> 2555:             t                1            none             bike
#> 2556:             t                1            none public_transport
#> 2557:             f                0            none              car
#> 2558:             t                1               3              car
#> 2559:             t                1            none              car
#>       howlong.transport howoften.flulike vaccine.last.year vaccine.this.year
#>                  <fctr>           <fctr>            <fctr>            <fctr>
#>    1:    (0,30)_minutes   (0,2]_annually                no                no
#>    2:    (0,30)_minutes   (0,2]_annually                no                no
#>    3:   [30,90)_minutes   (0,2]_annually               yes               yes
#>    4:  [90,360)_minutes   (0,2]_annually                no                no
#>    5:              none   (0,2]_annually                no                no
#>   ---                                                                       
#> 2555:              none   (2,5]_annually               yes               yes
#> 2556:  [90,360)_minutes   (0,2]_annually                no                no
#> 2557:   [30,90)_minutes        dont_know                no               yes
#> 2558:    (0,30)_minutes   (2,5]_annually         dont_know               yes
#> 2559:   [30,90)_minutes   (0,2]_annually                no                no
#>       date.vaccine.option date.vaccine why.vaccine.riskgroup
#>                    <fctr>       <char>                <fctr>
#>    1:                <NA>                               <NA>
#>    2:                <NA>                               <NA>
#>    3:                 yes   2012-09-18                     f
#>    4:                <NA>                               <NA>
#>    5:                <NA>                               <NA>
#>   ---                                                       
#> 2555:                 yes   2012-10-21                     f
#> 2556:                <NA>                               <NA>
#> 2557:                 yes   2012-09-29                     t
#> 2558:                 yes   2012-10-21                     f
#> 2559:                <NA>                               <NA>
#>       why.vaccine.protected why.vaccine.protect.others why.vaccine.doctor
#>                      <fctr>                     <fctr>             <fctr>
#>    1:                  <NA>                       <NA>               <NA>
#>    2:                  <NA>                       <NA>               <NA>
#>    3:                     t                          f                  t
#>    4:                  <NA>                       <NA>               <NA>
#>    5:                  <NA>                       <NA>               <NA>
#>   ---                                                                    
#> 2555:                     f                          t                  t
#> 2556:                  <NA>                       <NA>               <NA>
#> 2557:                     f                          f                  f
#> 2558:                     f                          f                  f
#> 2559:                  <NA>                       <NA>               <NA>
#>       why.vaccine.work.recommended why.vaccine.convenient why.vaccine.free
#>                             <fctr>                 <fctr>           <fctr>
#>    1:                         <NA>                   <NA>             <NA>
#>    2:                         <NA>                   <NA>             <NA>
#>    3:                            f                      t                f
#>    4:                         <NA>                   <NA>             <NA>
#>    5:                         <NA>                   <NA>             <NA>
#>   ---                                                                     
#> 2555:                            f                      f                f
#> 2556:                         <NA>                   <NA>             <NA>
#> 2557:                            f                      f                t
#> 2558:                            f                      t                f
#> 2559:                         <NA>                   <NA>             <NA>
#>       why.vaccine.nomiss.work why.vaccine.always why.vaccine.other
#>                        <fctr>             <fctr>            <fctr>
#>    1:                    <NA>               <NA>              <NA>
#>    2:                    <NA>               <NA>              <NA>
#>    3:                       t                  t                 f
#>    4:                    <NA>               <NA>              <NA>
#>    5:                    <NA>               <NA>              <NA>
#>   ---                                                             
#> 2555:                       f                  f                 f
#> 2556:                    <NA>               <NA>              <NA>
#> 2557:                       f                  f                 f
#> 2558:                       f                  t                 f
#> 2559:                    <NA>               <NA>              <NA>
#>       why.not.vaccine.notyet why.not.vaccine.notoffered why.not.vaccine.norisk
#>                       <fctr>                     <fctr>                 <fctr>
#>    1:                      t                          f                      f
#>    2:                      f                          f                      f
#>    3:                   <NA>                       <NA>                   <NA>
#>    4:                      f                          f                      f
#>    5:                      f                          f                      t
#>   ---                                                                         
#> 2555:                   <NA>                       <NA>                   <NA>
#> 2556:                      f                          f                      t
#> 2557:                   <NA>                       <NA>                   <NA>
#> 2558:                   <NA>                       <NA>                   <NA>
#> 2559:                      f                          t                      f
#>       why.not.vaccine.natural why.not.vaccine.noteffective
#>                        <fctr>                       <fctr>
#>    1:                       f                            f
#>    2:                       f                            f
#>    3:                    <NA>                         <NA>
#>    4:                       f                            f
#>    5:                       f                            f
#>   ---                                                     
#> 2555:                    <NA>                         <NA>
#> 2556:                       f                            t
#> 2557:                    <NA>                         <NA>
#> 2558:                    <NA>                         <NA>
#> 2559:                       f                            f
#>       why.not.vaccine.minor why.not.vaccine.unlikely why.not.vaccine.cause
#>                      <fctr>                   <fctr>                <fctr>
#>    1:                     f                        f                     f
#>    2:                     f                        f                     f
#>    3:                  <NA>                     <NA>                  <NA>
#>    4:                     f                        f                     f
#>    5:                     f                        f                     f
#>   ---                                                                     
#> 2555:                  <NA>                     <NA>                  <NA>
#> 2556:                     f                        f                     f
#> 2557:                  <NA>                     <NA>                  <NA>
#> 2558:                  <NA>                     <NA>                  <NA>
#> 2559:                     f                        f                     f
#>       why.not.vaccine.side.effects why.not.vaccine.dont.like
#>                             <fctr>                    <fctr>
#>    1:                            f                         f
#>    2:                            f                         f
#>    3:                         <NA>                      <NA>
#>    4:                            f                         t
#>    5:                            f                         f
#>   ---                                                       
#> 2555:                         <NA>                      <NA>
#> 2556:                            t                         f
#> 2557:                         <NA>                      <NA>
#> 2558:                         <NA>                      <NA>
#> 2559:                            f                         f
#>       why.not.vaccine.unavailable why.not.vaccine.not.free
#>                            <fctr>                   <fctr>
#>    1:                           f                        f
#>    2:                           f                        f
#>    3:                        <NA>                     <NA>
#>    4:                           f                        f
#>    5:                           f                        f
#>   ---                                                     
#> 2555:                        <NA>                     <NA>
#> 2556:                           f                        f
#> 2557:                        <NA>                     <NA>
#> 2558:                        <NA>                     <NA>
#> 2559:                           f                        f
#>       why.not.vaccine.no.reason why.not.vaccine.doctor why.not.vaccine.other
#>                          <fctr>                 <fctr>                <fctr>
#>    1:                         f                      f                     f
#>    2:                         f                      t                     f
#>    3:                      <NA>                   <NA>                  <NA>
#>    4:                         f                      f                     f
#>    5:                         f                      f                     f
#>   ---                                                                       
#> 2555:                      <NA>                   <NA>                  <NA>
#> 2556:                         t                      f                     f
#> 2557:                      <NA>                   <NA>                  <NA>
#> 2558:                      <NA>                   <NA>                  <NA>
#> 2559:                         f                      f                     f
#>       risk.none risk.asthma risk.diabetes risk.lung risk.heart risk.kidney
#>          <fctr>      <fctr>        <fctr>    <fctr>     <fctr>      <fctr>
#>    1:         t           f             f         f          f           f
#>    2:         t           f             f         f          f           f
#>    3:         t           f             f         f          f           f
#>    4:         t           f             f         f          f           f
#>    5:         t           f             f         f          f           f
#>   ---                                                                     
#> 2555:         t           f             f         f          f           f
#> 2556:         t           f             f         f          f           f
#> 2557:         t           f             f         f          f           f
#> 2558:         t           f             f         f          f           f
#> 2559:         t           f             f         f          f           f
#>       risk.immune pregnant pregnant.trimester        smoke allergy.hayfever
#>            <fctr>   <fctr>              <int>       <fctr>           <fctr>
#>    1:           f     <NA>                 NA           no                f
#>    2:           f     <NA>                 NA           no                f
#>    3:           f     <NA>                 NA           no                f
#>    4:           f       no                 NA           no                f
#>    5:           f     <NA>                 NA           no                f
#>   ---                                                                      
#> 2555:           f     <NA>                 NA           no                f
#> 2556:           f     <NA>                 NA           no                f
#> 2557:           f     <NA>                 NA           no                f
#> 2558:           f     <NA>                 NA           no                f
#> 2559:           f     <NA>                 NA occasionally                f
#>       allergy.dust allergy.animals allergy.other allergy.none diet.none
#>             <fctr>          <fctr>        <fctr>       <fctr>    <fctr>
#>    1:            f               f             f            t         t
#>    2:            f               f             f            t         t
#>    3:            f               f             f            t         t
#>    4:            f               f             f            t         t
#>    5:            f               f             f            t         t
#>   ---                                                                  
#> 2555:            f               f             f            t         t
#> 2556:            f               f             f            t         t
#> 2557:            f               f             f            t         t
#> 2558:            f               f             f            t         t
#> 2559:            f               f             f            t         t
#>       diet.vegetarian diet.vegan diet.low-calorie diet.other pets.none
#>                <fctr>     <fctr>           <fctr>     <fctr>    <fctr>
#>    1:               f          f                f          f         f
#>    2:               f          f                f          f         t
#>    3:               f          f                f          f         t
#>    4:               f          f                f          f         t
#>    5:               f          f                f          f         t
#>   ---                                                                 
#> 2555:               f          f                f          f         t
#> 2556:               f          f                f          f         t
#> 2557:               f          f                f          f         f
#> 2558:               f          f                f          f         t
#> 2559:               f          f                f          f         t
#>       pets.dogs pets.cats pets.birds pets.other howhear.radio.tv
#>          <fctr>    <fctr>     <fctr>     <fctr>           <fctr>
#>    1:         t         t          f          f                f
#>    2:         f         f          f          f                f
#>    3:         f         f          f          f                f
#>    4:         f         f          f          f                f
#>    5:         f         f          f          f                f
#>   ---                                                           
#> 2555:         f         f          f          f                f
#> 2556:         f         f          f          f                t
#> 2557:         t         f          f          f                t
#> 2558:         f         f          f          f                f
#> 2559:         f         f          f          f                f
#>       howhear.paper.magazine howhear.internet howhear.poster
#>                       <fctr>           <fctr>         <fctr>
#>    1:                      t                f              f
#>    2:                      f                t              f
#>    3:                      f                f              f
#>    4:                      f                t              f
#>    5:                      t                f              f
#>   ---                                                       
#> 2555:                      f                f              f
#> 2556:                      f                f              f
#> 2557:                      f                f              f
#> 2558:                      f                t              f
#> 2559:                      f                f              f
#>       howhear.family.friends howhear.other       date  birthdate   age agegroup
#>                       <fctr>        <fctr>     <Date>     <Date> <num>   <fctr>
#>    1:                      f             f 2012-11-08 1967-05-01    45    45-64
#>    2:                      t             f 2012-11-08 1958-01-01    54    45-64
#>    3:                      t             f 2012-11-11 1996-07-01    16     0-18
#>    4:                      f             t 2012-11-11 1976-02-01    36    19-44
#>    5:                      f             f 2012-11-11 1963-04-01    49    45-64
#>   ---                                                                          
#> 2555:                      t             f 2013-04-10 1928-04-01    84      65+
#> 2556:                      f             f 2013-04-10 1946-12-01    65      65+
#> 2557:                      f             f 2013-04-10 1976-01-01    37    19-44
#> 2558:                      f             f 2013-04-10 1957-05-01    55    45-64
#> 2559:                      t             f 2013-04-10 1928-05-01    84      65+
#>       living.with.children  country settlement.type                   region
#>                      <int>   <fctr>           <int>                   <fctr>
#>    1:                    0  england               5            east_midlands
#>    2:                    1  england               5                   london
#>    3:                    0  england               7            east_midlands
#>    4:                    1 scotland               1                 scotland
#>    5:                    1  england               5                   london
#>   ---                                                                       
#> 2555:                    0  england               5                   london
#> 2556:                    0  england               5                   london
#> 2557:                    1  england               5       south_west_england
#> 2558:                    1  england               6            west_midlands
#> 2559:                    0  england               5 yorkshire_and_the_humber
#>       urban.rural work.country work.settlement.type        work.region
#>            <fctr>       <fctr>                <int>             <fctr>
#>    1:       urban      england                    5             london
#>    2:       urban      england                    5 north_east_england
#>    3:       rural         <NA>                   NA               <NA>
#>    4:       urban      england                    6 south_east_england
#>    5:       urban      england                    5             london
#>   ---                                                                 
#> 2555:       urban         <NA>                   NA               <NA>
#> 2556:       urban         <NA>                   NA               <NA>
#> 2557:       urban      england                    5 north_west_england
#> 2558:       rural         <NA>                   NA               <NA>
#> 2559:       urban         <NA>                   NA               <NA>
#>       work.urban.rural highest.education nb.household nb.household.children
#>                 <fctr>            <fctr>        <num>                 <num>
#>    1:            urban    education.gcse            2                     0
#>    2:            urban education.alevels            2                     1
#>    3:             <NA> education.stillin            2                     0
#>    4:            rural education.alevels            2                     1
#>    5:            urban     education.bsc            2                     1
#>   ---                                                                      
#> 2555:             <NA>     education.msc            2                     0
#> 2556:             <NA>     education.bsc            2                     0
#> 2557:            urban     education.msc            3                     1
#> 2558:             <NA> education.alevels            2                     1
#> 2559:             <NA>     education.bsc            2                     0
#>       contact.id conversational.home.0-4 conversational.home.5-18
#>            <int>                   <int>                    <int>
#>    1:        420                       0                        0
#>    2:        838                       0                        3
#>    3:         54                       0                        0
#>    4:        112                       1                        1
#>    5:        224                       1                        1
#>   ---                                                            
#> 2555:        676                       1                        1
#> 2556:        734                       0                        0
#> 2557:        782                       0                        3
#> 2558:        873                       2                        0
#> 2559:        898                       0                        0
#>       conversational.home.19-44 conversational.home.45-64
#>                           <int>                     <int>
#>    1:                         3                         1
#>    2:                         0                         4
#>    3:                         4                         0
#>    4:                         0                         3
#>    5:                         1                         1
#>   ---                                                    
#> 2555:                         0                         0
#> 2556:                         1                         1
#> 2557:                         3                         1
#> 2558:                         0                         1
#> 2559:                         0                         1
#>       conversational.home.65+ conversational.work.0-4 conversational.work.5-18
#>                         <int>                   <int>                    <int>
#>    1:                       1                       2                        3
#>    2:                       0                       1                        3
#>    3:                       2                       1                        1
#>    4:                       1                       3                        1
#>    5:                       1                       0                        1
#>   ---                                                                         
#> 2555:                       1                       0                        0
#> 2556:                       1                       0                        0
#> 2557:                       0                       3                        0
#> 2558:                       1                       1                        0
#> 2559:                       2                       1                        0
#>       conversational.work.19-44 conversational.work.45-64
#>                           <int>                     <int>
#>    1:                         3                         1
#>    2:                         3                         3
#>    3:                         2                         0
#>    4:                         3                         4
#>    5:                         1                         2
#>   ---                                                    
#> 2555:                         1                         1
#> 2556:                         1                         0
#> 2557:                         2                         1
#> 2558:                         0                         0
#> 2559:                         0                         0
#>       conversational.work.65+ conversational.other.0-4
#>                         <int>                    <int>
#>    1:                       0                        0
#>    2:                       0                        1
#>    3:                       1                        2
#>    4:                       1                        0
#>    5:                       0                        0
#>   ---                                                 
#> 2555:                       1                        1
#> 2556:                       0                        2
#> 2557:                       1                        0
#> 2558:                       1                        2
#> 2559:                       0                        2
#>       conversational.other.5-18 conversational.other.19-44
#>                           <int>                      <int>
#>    1:                         3                          1
#>    2:                         2                          5
#>    3:                         1                          2
#>    4:                         1                          1
#>    5:                         1                          1
#>   ---                                                     
#> 2555:                         2                          3
#> 2556:                         0                          3
#> 2557:                         1                          1
#> 2558:                         0                          3
#> 2559:                         1                          1
#>       conversational.other.45-64 conversational.other.65+ physical.home.0-4
#>                            <int>                    <int>             <int>
#>    1:                          2                        1                 0
#>    2:                          2                        1                 0
#>    3:                          1                        0                 0
#>    4:                          1                        2                 0
#>    5:                          1                        0                 1
#>   ---                                                                      
#> 2555:                          0                        2                 0
#> 2556:                          1                        0                 0
#> 2557:                          2                        0                 0
#> 2558:                          1                        1                 0
#> 2559:                          1                        0                 0
#>       physical.home.5-18 physical.home.19-44 physical.home.45-64
#>                    <int>               <int>               <int>
#>    1:                  0                   2                   1
#>    2:                  1                   0                   1
#>    3:                  0                   4                   0
#>    4:                  0                   0                   2
#>    5:                  1                   1                   1
#>   ---                                                           
#> 2555:                  0                   0                   0
#> 2556:                  0                   1                   0
#> 2557:                  1                   2                   0
#> 2558:                  0                   0                   0
#> 2559:                  0                   0                   1
#>       physical.home.65+ physical.work.0-4 physical.work.5-18
#>                   <int>             <int>              <int>
#>    1:                 0                 1                  0
#>    2:                 0                 0                  2
#>    3:                 0                 1                  0
#>    4:                 1                 1                  1
#>    5:                 0                 0                  1
#>   ---                                                       
#> 2555:                 0                 0                  0
#> 2556:                 0                 0                  0
#> 2557:                 0                 1                  0
#> 2558:                 0                 1                  0
#> 2559:                 2                 0                  0
#>       physical.work.19-44 physical.work.45-64 physical.work.65+
#>                     <int>               <int>             <int>
#>    1:                   0                   0                 0
#>    2:                   1                   1                 0
#>    3:                   1                   0                 1
#>    4:                   3                   3                 0
#>    5:                   0                   0                 0
#>   ---                                                          
#> 2555:                   1                   0                 0
#> 2556:                   0                   0                 0
#> 2557:                   1                   1                 1
#> 2558:                   0                   0                 1
#> 2559:                   0                   0                 0
#>       physical.other.0-4 physical.other.5-18 physical.other.19-44
#>                    <int>               <int>                <int>
#>    1:                  0                   2                    0
#>    2:                  0                   2                    3
#>    3:                  1                   0                    2
#>    4:                  0                   0                    0
#>    5:                  0                   0                    0
#>   ---                                                            
#> 2555:                  1                   1                    3
#> 2556:                  2                   0                    2
#> 2557:                  0                   0                    0
#> 2558:                  0                   0                    2
#> 2559:                  2                   1                    0
#>       physical.other.45-64 physical.other.65+ public.transport
#>                      <int>              <int>           <fctr>
#>    1:                    1                  1             none
#>    2:                    1                  0   (240,)_minutes
#>    3:                    0                  0             none
#>    4:                    0                  1   (0,30]_minutes
#>    5:                    0                  0 (90,240]_minutes
#>   ---                                                         
#> 2555:                    0                  1             none
#> 2556:                    0                  0             none
#> 2557:                    1                  0   (0,30]_minutes
#> 2558:                    0                  1             none
#> 2559:                    0                  0 (90,240]_minutes
#>       enclosed.indoor.space furthest.travelled conversational
#>                      <fctr>             <fctr>          <int>
#>    1:      (90,240]_minutes        [1,5)_miles             21
#>    2:        (0,30]_minutes        [0,1)_miles             28
#>    3:       (30,90]_minutes     [30,100]_miles             17
#>    4:       (30,90]_minutes       (100,)_miles             23
#>    5:                  none     [30,100]_miles             12
#>   ---                                                        
#> 2555:      (90,240]_minutes      [10,30)_miles             14
#> 2556:        (240,)_minutes     [30,100]_miles             10
#> 2557:                  none        [1,5)_miles             18
#> 2558:        (0,30]_minutes     [30,100]_miles             13
#> 2559:      (90,240]_minutes       (100,)_miles              9
#>       conversational.home conversational.work conversational.other
#>                     <int>               <int>                <int>
#>    1:                   5                   9                    7
#>    2:                   7                  10                   11
#>    3:                   6                   5                    6
#>    4:                   6                  12                    5
#>    5:                   5                   4                    3
#>   ---                                                             
#> 2555:                   3                   3                    8
#> 2556:                   3                   1                    6
#> 2557:                   7                   7                    4
#> 2558:                   4                   2                    7
#> 2559:                   3                   1                    5
#>       conversational.0-4 conversational.5-18 conversational.19-44
#>                    <int>               <int>                <int>
#>    1:                  2                   6                    7
#>    2:                  2                   8                    8
#>    3:                  3                   2                    8
#>    4:                  4                   3                    4
#>    5:                  1                   3                    3
#>   ---                                                            
#> 2555:                  2                   3                    4
#> 2556:                  2                   0                    5
#> 2557:                  3                   4                    6
#> 2558:                  5                   0                    3
#> 2559:                  3                   1                    1
#>       conversational.45-64 conversational.65+ physical physical.home
#>                      <int>              <int>    <int>         <int>
#>    1:                    4                  2        8             3
#>    2:                    9                  1       12             2
#>    3:                    1                  3       10             4
#>    4:                    8                  4       12             3
#>    5:                    4                  1        5             4
#>   ---                                                               
#> 2555:                    1                  4        7             0
#> 2556:                    2                  1        5             1
#> 2557:                    4                  1        8             3
#> 2558:                    2                  3        5             0
#> 2559:                    2                  2        6             3
#>       physical.work physical.other physical.0-4 physical.5-18 physical.19-44
#>               <int>          <int>        <int>         <int>          <int>
#>    1:             1              4            1             2              2
#>    2:             4              6            0             5              4
#>    3:             3              3            2             0              7
#>    4:             8              1            1             1              3
#>    5:             1              0            1             2              1
#>   ---                                                                       
#> 2555:             1              6            1             1              4
#> 2556:             0              4            2             0              3
#> 2557:             4              1            1             1              3
#> 2558:             2              3            1             0              2
#> 2559:             0              3            2             1              0
#>       physical.45-64 physical.65+ contact.date contact.day.of.week symptom.id
#>                <int>        <int>       <Date>               <num>      <int>
#>    1:              2            1   2012-11-07                   3       1272
#>    2:              3            0   2012-11-07                   3       2555
#>    3:              0            1   2012-11-10                   6        166
#>    4:              5            2   2012-11-10                   6        326
#>    5:              1            0   2012-11-10                   6        665
#>   ---                                                                        
#> 2555:              0            1   2013-04-09                   2       2051
#> 2556:              0            0   2013-04-09                   2       2238
#> 2557:              2            1   2013-01-16                   3       2379
#> 2558:              0            2   2013-02-05                   2       2662
#> 2559:              1            2   2012-12-01                   6       2746
#>       no.symptoms  fever chills blocked.runny.nose sneezing sore.throat  cough
#>            <fctr> <fctr> <fctr>             <fctr>   <fctr>      <fctr> <fctr>
#>    1:           t      f      f                  f        f           f      f
#>    2:           t      f      f                  f        f           f      f
#>    3:           t      f      f                  f        f           f      f
#>    4:           t      f      f                  f        f           f      f
#>    5:           t      f      f                  f        f           f      f
#>   ---                                                                         
#> 2555:           t      f      f                  f        f           f      f
#> 2556:           t      f      f                  f        f           f      f
#> 2557:           f      f      f                  t        f           t      t
#> 2558:           t      f      f                  f        f           f      f
#> 2559:           t      f      f                  f        f           f      f
#>       shortness.breath headache muscle.and.or.joint.pain chest.pain  tired
#>                 <fctr>   <fctr>                   <fctr>     <fctr> <fctr>
#>    1:                f        f                        f          f      f
#>    2:                f        f                        f          f      f
#>    3:                f        f                        f          f      f
#>    4:                f        f                        f          f      f
#>    5:                f        f                        f          f      f
#>   ---                                                                     
#> 2555:                f        f                        f          f      f
#> 2556:                f        f                        f          f      f
#> 2557:                f        f                        t          f      t
#> 2558:                f        f                        f          f      f
#> 2559:                f        f                        f          f      f
#>       loss.appetite phlegm watery.eyes nausea vomiting diarrhoea stomach.ache
#>              <fctr> <fctr>      <fctr> <fctr>   <fctr>    <fctr>       <fctr>
#>    1:             f      f           f      f        f         f            f
#>    2:             f      f           f      f        f         f            f
#>    3:             f      f           f      f        f         f            f
#>    4:             f      f           f      f        f         f            f
#>    5:             f      f           f      f        f         f            f
#>   ---                                                                        
#> 2555:             f      f           f      f        f         f            f
#> 2556:             f      f           f      f        f         f            f
#> 2557:             f      f           f      f        f         f            f
#> 2558:             f      f           f      f        f         f            f
#> 2559:             f      f           f      f        f         f            f
#>       other.symptoms   same symptoms.start.option symptoms.start.date
#>               <fctr> <fctr>                <fctr>              <Date>
#>    1:              f   <NA>                  <NA>                <NA>
#>    2:              f   <NA>                  <NA>                <NA>
#>    3:              f   <NA>                  <NA>                <NA>
#>    4:              f   <NA>                  <NA>                <NA>
#>    5:              f   <NA>                  <NA>                <NA>
#>   ---                                                                
#> 2555:              f   <NA>                  <NA>                <NA>
#> 2556:              f   <NA>                  <NA>                <NA>
#> 2557:              f     no             dont_know                <NA>
#> 2558:              f   <NA>                  <NA>                <NA>
#> 2559:              f   <NA>                  <NA>                <NA>
#>       symptoms.end.option symptoms.end.date symptoms.suddenly
#>                    <fctr>            <Date>            <fctr>
#>    1:                <NA>              <NA>              <NA>
#>    2:                <NA>              <NA>              <NA>
#>    3:                <NA>              <NA>              <NA>
#>    4:                <NA>              <NA>              <NA>
#>    5:                <NA>              <NA>              <NA>
#>   ---                                                        
#> 2555:                <NA>              <NA>              <NA>
#> 2556:                <NA>              <NA>              <NA>
#> 2557:           still_ill              <NA>                no
#> 2558:                <NA>              <NA>              <NA>
#> 2559:                <NA>              <NA>              <NA>
#>       fever.start.option fever.start.date fever.suddenly fever.temperature
#>                   <fctr>           <Date>         <fctr>            <fctr>
#>    1:               <NA>             <NA>           <NA>              <NA>
#>    2:               <NA>             <NA>           <NA>              <NA>
#>    3:               <NA>             <NA>           <NA>              <NA>
#>    4:               <NA>             <NA>           <NA>              <NA>
#>    5:               <NA>             <NA>           <NA>              <NA>
#>   ---                                                                     
#> 2555:               <NA>             <NA>           <NA>              <NA>
#> 2556:               <NA>             <NA>           <NA>              <NA>
#> 2557:               <NA>             <NA>           <NA>              <NA>
#> 2558:               <NA>             <NA>           <NA>              <NA>
#> 2559:               <NA>             <NA>           <NA>              <NA>
#>       fever.temperature.value visit.medical.service.no visit.medical.service.gp
#>                        <fctr>                   <fctr>                   <fctr>
#>    1:                    <NA>                     <NA>                     <NA>
#>    2:                    <NA>                     <NA>                     <NA>
#>    3:                    <NA>                     <NA>                     <NA>
#>    4:                    <NA>                     <NA>                     <NA>
#>    5:                    <NA>                     <NA>                     <NA>
#>   ---                                                                          
#> 2555:                    <NA>                     <NA>                     <NA>
#> 2556:                    <NA>                     <NA>                     <NA>
#> 2557:                    <NA>                        t                        f
#> 2558:                    <NA>                     <NA>                     <NA>
#> 2559:                    <NA>                     <NA>                     <NA>
#>       visit.medical.service.ae visit.medical.service.hospital
#>                         <fctr>                         <fctr>
#>    1:                     <NA>                           <NA>
#>    2:                     <NA>                           <NA>
#>    3:                     <NA>                           <NA>
#>    4:                     <NA>                           <NA>
#>    5:                     <NA>                           <NA>
#>   ---                                                        
#> 2555:                     <NA>                           <NA>
#> 2556:                     <NA>                           <NA>
#> 2557:                        f                              f
#> 2558:                     <NA>                           <NA>
#> 2559:                     <NA>                           <NA>
#>       visit.medical.service.other visit.medical.service.appointment
#>                            <fctr>                            <fctr>
#>    1:                        <NA>                              <NA>
#>    2:                        <NA>                              <NA>
#>    3:                        <NA>                              <NA>
#>    4:                        <NA>                              <NA>
#>    5:                        <NA>                              <NA>
#>   ---                                                              
#> 2555:                        <NA>                              <NA>
#> 2556:                        <NA>                              <NA>
#> 2557:                           f                                 f
#> 2558:                        <NA>                              <NA>
#> 2559:                        <NA>                              <NA>
#>       visit.medical.service.howsoon.gp.receptionist
#>                                              <fctr>
#>    1:                                          <NA>
#>    2:                                          <NA>
#>    3:                                          <NA>
#>    4:                                          <NA>
#>    5:                                          <NA>
#>   ---                                              
#> 2555:                                          <NA>
#> 2556:                                          <NA>
#> 2557:                                          <NA>
#> 2558:                                          <NA>
#> 2559:                                          <NA>
#>       visit.medical.service.howsoon.gp.doctor.nurse
#>                                              <fctr>
#>    1:                                          <NA>
#>    2:                                          <NA>
#>    3:                                          <NA>
#>    4:                                          <NA>
#>    5:                                          <NA>
#>   ---                                              
#> 2555:                                          <NA>
#> 2556:                                          <NA>
#> 2557:                                          <NA>
#> 2558:                                          <NA>
#> 2559:                                          <NA>
#>       visit.medical.service.howsoon.nhs visit.medical.service.howsoon.other
#>                                  <fctr>                              <fctr>
#>    1:                              <NA>                                <NA>
#>    2:                              <NA>                                <NA>
#>    3:                              <NA>                                <NA>
#>    4:                              <NA>                                <NA>
#>    5:                              <NA>                                <NA>
#>   ---                                                                      
#> 2555:                              <NA>                                <NA>
#> 2556:                              <NA>                                <NA>
#> 2557:                              <NA>                                <NA>
#> 2558:                              <NA>                                <NA>
#> 2559:                              <NA>                                <NA>
#>       contact.medical.service.no contact.medical.service.gp.receptionist
#>                           <fctr>                                  <fctr>
#>    1:                       <NA>                                    <NA>
#>    2:                       <NA>                                    <NA>
#>    3:                       <NA>                                    <NA>
#>    4:                       <NA>                                    <NA>
#>    5:                       <NA>                                    <NA>
#>   ---                                                                   
#> 2555:                       <NA>                                    <NA>
#> 2556:                       <NA>                                    <NA>
#> 2557:                          t                                       f
#> 2558:                       <NA>                                    <NA>
#> 2559:                       <NA>                                    <NA>
#>       contact.medical.service.gp.doctor contact.medical.service.nhs
#>                                  <fctr>                      <fctr>
#>    1:                              <NA>                        <NA>
#>    2:                              <NA>                        <NA>
#>    3:                              <NA>                        <NA>
#>    4:                              <NA>                        <NA>
#>    5:                              <NA>                        <NA>
#>   ---                                                              
#> 2555:                              <NA>                        <NA>
#> 2556:                              <NA>                        <NA>
#> 2557:                                 f                           f
#> 2558:                              <NA>                        <NA>
#> 2559:                              <NA>                        <NA>
#>       contact.medical.service.npfs contact.medical.service.other
#>                             <fctr>                        <fctr>
#>    1:                         <NA>                          <NA>
#>    2:                         <NA>                          <NA>
#>    3:                         <NA>                          <NA>
#>    4:                         <NA>                          <NA>
#>    5:                         <NA>                          <NA>
#>   ---                                                           
#> 2555:                         <NA>                          <NA>
#> 2556:                         <NA>                          <NA>
#> 2557:                            f                             f
#> 2558:                         <NA>                          <NA>
#> 2559:                         <NA>                          <NA>
#>       contact.medical.service.howsoon.gp.receptionist
#>                                                <fctr>
#>    1:                                            <NA>
#>    2:                                            <NA>
#>    3:                                            <NA>
#>    4:                                            <NA>
#>    5:                                            <NA>
#>   ---                                                
#> 2555:                                            <NA>
#> 2556:                                            <NA>
#> 2557:                                            <NA>
#> 2558:                                            <NA>
#> 2559:                                            <NA>
#>       contact.medical.service.howsoon.gp.doctor.nurse
#>                                                <fctr>
#>    1:                                            <NA>
#>    2:                                            <NA>
#>    3:                                            <NA>
#>    4:                                            <NA>
#>    5:                                            <NA>
#>   ---                                                
#> 2555:                                            <NA>
#> 2556:                                            <NA>
#> 2557:                                            <NA>
#> 2558:                                            <NA>
#> 2559:                                            <NA>
#>       contact.medical.service.howsoon.nhs contact.medical.service.howsoon.other
#>                                    <fctr>                                <fctr>
#>    1:                                <NA>                                  <NA>
#>    2:                                <NA>                                  <NA>
#>    3:                                <NA>                                  <NA>
#>    4:                                <NA>                                  <NA>
#>    5:                                <NA>                                  <NA>
#>   ---                                                                          
#> 2555:                                <NA>                                  <NA>
#> 2556:                                <NA>                                  <NA>
#> 2557:                                <NA>                                  <NA>
#> 2558:                                <NA>                                  <NA>
#> 2559:                                <NA>                                  <NA>
#>       no.medication medication.painkillers medication.cough
#>              <fctr>                 <fctr>           <fctr>
#>    1:          <NA>                   <NA>             <NA>
#>    2:          <NA>                   <NA>             <NA>
#>    3:          <NA>                   <NA>             <NA>
#>    4:          <NA>                   <NA>             <NA>
#>    5:          <NA>                   <NA>             <NA>
#>   ---                                                      
#> 2555:          <NA>                   <NA>             <NA>
#> 2556:          <NA>                   <NA>             <NA>
#> 2557:             f                      f                f
#> 2558:          <NA>                   <NA>             <NA>
#> 2559:          <NA>                   <NA>             <NA>
#>       medication.antiviral medication.antibiotic medication.other
#>                     <fctr>                <fctr>           <fctr>
#>    1:                 <NA>                  <NA>             <NA>
#>    2:                 <NA>                  <NA>             <NA>
#>    3:                 <NA>                  <NA>             <NA>
#>    4:                 <NA>                  <NA>             <NA>
#>    5:                 <NA>                  <NA>             <NA>
#>   ---                                                            
#> 2555:                 <NA>                  <NA>             <NA>
#> 2556:                 <NA>                  <NA>             <NA>
#> 2557:                    t                     f                f
#> 2558:                 <NA>                  <NA>             <NA>
#> 2559:                 <NA>                  <NA>             <NA>
#>       medication.dontknow medication.howsoon alter.routine still.altered
#>                    <fctr>              <int>        <fctr>        <fctr>
#>    1:                <NA>                 NA          <NA>          <NA>
#>    2:                <NA>                 NA          <NA>          <NA>
#>    3:                <NA>                 NA          <NA>          <NA>
#>    4:                <NA>                 NA          <NA>          <NA>
#>    5:                <NA>                 NA          <NA>          <NA>
#>   ---                                                                   
#> 2555:                <NA>                 NA          <NA>          <NA>
#> 2556:                <NA>                 NA          <NA>          <NA>
#> 2557:                   f                  2            no          <NA>
#> 2558:                <NA>                 NA          <NA>          <NA>
#> 2559:                <NA>                 NA          <NA>          <NA>
#>       howlong.altered what.do.you.think health.score        onset suddenly
#>                <fctr>            <fctr>        <int>       <char>   <fctr>
#>    1:            <NA>              <NA>           97 not.answered     <NA>
#>    2:            <NA>              <NA>           76 not.answered     <NA>
#>    3:            <NA>              <NA>           93 not.answered     <NA>
#>    4:            <NA>              <NA>           86 not.answered     <NA>
#>    5:            <NA>              <NA>           74 not.answered     <NA>
#>   ---                                                                     
#> 2555:            <NA>              <NA>           89 not.answered     <NA>
#> 2556:            <NA>              <NA>           89 not.answered     <NA>
#> 2557:            <NA>         dont_know           39   not.sudden        f
#> 2558:            <NA>              <NA>           80 not.answered     <NA>
#> 2559:            <NA>              <NA>           91 not.answered     <NA>
#>       fever.symptoms systemic.symptoms resp.symptoms general.symptoms
#>               <fctr>            <fctr>        <fctr>           <fctr>
#>    1:              f                 f             f                f
#>    2:              f                 f             f                f
#>    3:              f                 f             f                f
#>    4:              f                 f             f                f
#>    5:              f                 f             f                f
#>   ---                                                                
#> 2555:              f                 f             f                f
#> 2556:              f                 f             f                f
#> 2557:              f                 t             t                t
#> 2558:              f                 f             f                f
#> 2559:              f                 f             f                f
#>       ili.symptoms    ili ili.fever ili.self     gi nReports   min.date
#>             <fctr> <fctr>    <fctr>   <fctr> <fctr>    <int>     <Date>
#>    1:            f      f         f        f      f       17 2012-11-05
#>    2:            f      f         f        f      f        4 2012-11-05
#>    3:            f      f         f        f      f        3 2012-11-05
#>    4:            f      f         f        f      f        5 2012-11-05
#>    5:            f      f         f        f      f        7 2012-11-05
#>   ---                                                                  
#> 2555:            f      f         f        f      f       18 2012-11-10
#> 2556:            f      f         f        f      f       16 2012-11-05
#> 2557:            t      f         f        f      f       11 2013-01-10
#> 2558:            f      f         f        f      f       11 2013-01-23
#> 2559:            f      f         f        f      f       18 2012-11-18
#>         max.date day.of.week month
#>           <Date>       <num> <int>
#>    1: 2013-03-22           4    11
#>    2: 2012-11-22           4    11
#>    3: 2012-11-24           7    11
#>    4: 2012-12-08           7    11
#>    5: 2012-12-30           7    11
#>   ---                             
#> 2555: 2013-04-10           3     4
#> 2556: 2013-04-10           3     4
#> 2557: 2013-04-10           3     4
#> 2558: 2013-04-10           3     4
#> 2559: 2013-04-10           3     4
```

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
  ili.sudden.unknown = NA
)
```

## Arguments

- data:

  the data to merge and clean, usually the result of `read_data`. The
  surveys are joined in a fixed order (background, contact, symptom),
  regardless of their order in `data`. The last one present drives a
  rolling join. The result has one row per report of that survey (the
  symptom reports, if present), each taking the latest report of the
  other surveys on or before its date. When an earlier survey has
  several reports on the same day, its last report is used. Any other
  survey name gives an error.

  Joining the contact and symptom surveys gives a warning. The join
  pairs each symptom report with the latest earlier contact report,
  however long before it was filed, and a contact report describes one
  day whereas a symptom report describes the week before it. With
  'unsuccessful.join' the symptom reports without an earlier contact
  report are dropped, shortening episodes of illness. To link the two,
  extract the symptom and contact surveys separately, get the episodes
  with
  [`bouts_of_illness`](https://sbfnk.github.io/flusurvey/reference/bouts_of_illness.md)
  and flag the contact reports made during an episode with
  [`flag_ill_contacts`](https://sbfnk.github.io/flusurvey/reference/flag_ill_contacts.md).

- clean:

  cleaning options, NULL for no cleaning, otherwise a vector of cleans
  to perform (by default all): - 'remove.first', whether to remove
  reports from the first reporting day, - 'remove.bad.symptom.dates',
  whether to remove bad symptom dates: symptom start before date of
  first or end after end of last report, end of symptoms before start of
  symptoms, symptoms reported to have started or ended after date of
  report - 'remove.bad.health.scores', whether to remove health scores
  \<0 or \>100 - 'limit.season', whether to limit a flu season to
  November -\> April - 'remove.postcodes', whether to remove postcodes -
  'n.reports', whether to exclude those with fewer than `min.reports`
  symptom reporting days - 'unsuccessful.join', whether to exclude those
  with unsuccesful joins (e.g. if symptoms are reported without a
  background survey present; the web site should have prevented this,
  but doesn't appear to have done so) - 'only.symptoms', whether to
  exclude those that have no report without symptoms

- min.reports:

  minimum number of symptom reporting days per user (ignored if
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
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use flag_ill_contacts().
table(dt$ili)
#> 
#>    f    t 
#> 2792   43 

## only remove each participant's first reporting day
merge_data(simulated_data[["2013"]], clean = "remove.first")
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use flag_ill_contacts().
#> Key: <date, global_id>
#>       work.postcode postcode background.id global_id             self gender
#>              <char>   <char>         <int>    <char>           <fctr> <fctr>
#>    1:          LL40      PA6             4   sim0004             self   male
#>    2:          <NA>      PA9            35   sim0035             self   male
#>    3:          <NA>     HP12            52   sim0052     someone_else female
#>    4:            M1     SO52           101   sim0101             self female
#>    5:          <NA>     AB53           141   sim0141             self   male
#>   ---                                                                       
#> 3064:          <NA>      CR0           165   sim0166             self female
#> 3065:          <NA>     RG45           233   sim0234             self   male
#> 3066:           G44      RM8           237   sim0238             self   male
#> 3067:           B16     KT11           262   sim0264 household_member female
#> 3068:          <NA>     PE10           278   sim0281             self   male
#>       birthmonth             main.activity work.postcode.option    occupation
#>           <char>                    <fctr>               <fctr>        <fctr>
#>    1:    1989-11 paid_employment_full_time                  yes  professional
#>    2:    1941-12                   retired                 <NA>          <NA>
#>    3:    2011-06                    school                 <NA>          <NA>
#>    4:    1954-07 paid_employment_part_time                  yes office_worker
#>    5:    2002-01                    school                 <NA>          <NA>
#>   ---                                                                        
#> 3064:    2010-05                    school                 <NA>          <NA>
#> 3065:    1968-12                home_maker                 <NA>          <NA>
#> 3066:    1987-12 paid_employment_full_time                  yes  professional
#> 3067:    1975-07 paid_employment_full_time                  yes office_worker
#> 3068:    1940-01                   retired                 <NA>          <NA>
#>       no.education education.gcse education.alevels education.bsc education.msc
#>             <fctr>         <fctr>            <fctr>        <fctr>        <fctr>
#>    1:            f              t                 f             f             f
#>    2:            f              f                 t             f             f
#>    3:            f              f                 f             f             f
#>    4:            f              f                 f             f             t
#>    5:            f              f                 f             f             f
#>   ---                                                                          
#> 3064:            f              f                 f             f             f
#> 3065:            f              t                 f             f             f
#> 3066:            f              t                 f             f             f
#> 3067:            f              f                 f             t             f
#> 3068:            f              t                 f             f             f
#>       education.stillin frequent.contact.children frequent.contact.elderly
#>                  <fctr>                    <fctr>                   <fctr>
#>    1:                 f                         f                        f
#>    2:                 f                         f                        f
#>    3:                 t                         f                        f
#>    4:                 f                         f                        f
#>    5:                 t                         f                        f
#>   ---                                                                     
#> 3064:                 t                         f                        f
#> 3065:                 f                         t                        t
#> 3066:                 f                         f                        f
#> 3067:                 f                         f                        f
#> 3068:                 f                         f                        t
#>       frequent.contact.patients frequent.contact.people frequent.contact.none
#>                          <fctr>                  <fctr>                <fctr>
#>    1:                         t                       f                     f
#>    2:                         f                       t                     f
#>    3:                         f                       f                     t
#>    4:                         f                       f                     t
#>    5:                         t                       f                     f
#>   ---                                                                        
#> 3064:                         t                       t                     f
#> 3065:                         f                       t                     f
#> 3066:                         f                       f                     t
#> 3067:                         f                       f                     t
#> 3068:                         f                       t                     f
#>       household.0.4 nb.household.0.4 household.5.18 nb.household.5.18
#>              <fctr>            <int>         <fctr>             <int>
#>    1:             t                2              t                 1
#>    2:             f                0              f                 0
#>    3:             t                1              t                 1
#>    4:             f                0              f                 0
#>    5:             f                0              t                 1
#>   ---                                                                
#> 3064:             f                0              f                 0
#> 3065:             t                1              f                 0
#> 3066:             f                0              f                 0
#> 3067:             t                1              t                 1
#> 3068:             f                0              t                 1
#>       household.19.44 nb.household.19.44 household.45.64 nb.household.45.64
#>                <fctr>              <int>          <fctr>              <int>
#>    1:               f                  0               f                  0
#>    2:               f                  0               f                  0
#>    3:               t                  2               t                  2
#>    4:               t                  2               f                  0
#>    5:               t                  3               f                  0
#>   ---                                                                      
#> 3064:               f                  0               f                  0
#> 3065:               t                  1               f                  0
#> 3066:               t                  1               f                  0
#> 3067:               t                  1               f                  0
#> 3068:               f                  0               f                  0
#>       household.65+ nb.household.65+ children.school        transport
#>              <fctr>            <int>          <fctr>           <fctr>
#>    1:             f                0            none              car
#>    2:             f                0               2              car
#>    3:             f                0            none             bike
#>    4:             f                0            none public_transport
#>    5:             f                0            none public_transport
#>   ---                                                                
#> 3064:             f                0            none              car
#> 3065:             f                0            none              car
#> 3066:             f                0            none              car
#> 3067:             f                0            (5,)          walking
#> 3068:             f                0               1 public_transport
#>       howlong.transport howoften.flulike vaccine.last.year vaccine.this.year
#>                  <fctr>           <fctr>            <fctr>            <fctr>
#>    1:    (0,30)_minutes        dont_know                no                no
#>    2:   [30,90)_minutes   (2,5]_annually               yes               yes
#>    3:   [30,90)_minutes     almost_never                no                no
#>    4:   [30,90)_minutes        dont_know               yes                no
#>    5:              none     almost_never                no                no
#>   ---                                                                       
#> 3064:  [90,360)_minutes   (0,2]_annually         dont_know                no
#> 3065:  [90,360)_minutes     almost_never                no                no
#> 3066:   [30,90)_minutes   (0,2]_annually               yes               yes
#> 3067:    (0,30)_minutes        dont_know               yes                no
#> 3068:   [30,90)_minutes   (2,5]_annually                no                no
#>       date.vaccine.option date.vaccine why.vaccine.riskgroup
#>                    <fctr>       <char>                <fctr>
#>    1:                <NA>                               <NA>
#>    2:                 yes   2012-10-06                     f
#>    3:                <NA>                               <NA>
#>    4:                <NA>                               <NA>
#>    5:                <NA>                               <NA>
#>   ---                                                       
#> 3064:                <NA>                               <NA>
#> 3065:                <NA>                               <NA>
#> 3066:                 yes   2012-10-10                     f
#> 3067:                <NA>                               <NA>
#> 3068:                <NA>                               <NA>
#>       why.vaccine.protected why.vaccine.protect.others why.vaccine.doctor
#>                      <fctr>                     <fctr>             <fctr>
#>    1:                  <NA>                       <NA>               <NA>
#>    2:                     t                          f                  f
#>    3:                  <NA>                       <NA>               <NA>
#>    4:                  <NA>                       <NA>               <NA>
#>    5:                  <NA>                       <NA>               <NA>
#>   ---                                                                    
#> 3064:                  <NA>                       <NA>               <NA>
#> 3065:                  <NA>                       <NA>               <NA>
#> 3066:                     f                          t                  f
#> 3067:                  <NA>                       <NA>               <NA>
#> 3068:                  <NA>                       <NA>               <NA>
#>       why.vaccine.work.recommended why.vaccine.convenient why.vaccine.free
#>                             <fctr>                 <fctr>           <fctr>
#>    1:                         <NA>                   <NA>             <NA>
#>    2:                            f                      t                t
#>    3:                         <NA>                   <NA>             <NA>
#>    4:                         <NA>                   <NA>             <NA>
#>    5:                         <NA>                   <NA>             <NA>
#>   ---                                                                     
#> 3064:                         <NA>                   <NA>             <NA>
#> 3065:                         <NA>                   <NA>             <NA>
#> 3066:                            f                      f                t
#> 3067:                         <NA>                   <NA>             <NA>
#> 3068:                         <NA>                   <NA>             <NA>
#>       why.vaccine.nomiss.work why.vaccine.always why.vaccine.other
#>                        <fctr>             <fctr>            <fctr>
#>    1:                    <NA>               <NA>              <NA>
#>    2:                       f                  f                 f
#>    3:                    <NA>               <NA>              <NA>
#>    4:                    <NA>               <NA>              <NA>
#>    5:                    <NA>               <NA>              <NA>
#>   ---                                                             
#> 3064:                    <NA>               <NA>              <NA>
#> 3065:                    <NA>               <NA>              <NA>
#> 3066:                       f                  t                 f
#> 3067:                    <NA>               <NA>              <NA>
#> 3068:                    <NA>               <NA>              <NA>
#>       why.not.vaccine.notyet why.not.vaccine.notoffered why.not.vaccine.norisk
#>                       <fctr>                     <fctr>                 <fctr>
#>    1:                      f                          f                      f
#>    2:                   <NA>                       <NA>                   <NA>
#>    3:                      f                          f                      t
#>    4:                      f                          f                      t
#>    5:                      f                          f                      f
#>   ---                                                                         
#> 3064:                      f                          f                      f
#> 3065:                      t                          f                      f
#> 3066:                   <NA>                       <NA>                   <NA>
#> 3067:                      f                          f                      f
#> 3068:                      f                          f                      f
#>       why.not.vaccine.natural why.not.vaccine.noteffective
#>                        <fctr>                       <fctr>
#>    1:                       f                            f
#>    2:                    <NA>                         <NA>
#>    3:                       t                            f
#>    4:                       f                            f
#>    5:                       f                            t
#>   ---                                                     
#> 3064:                       f                            f
#> 3065:                       f                            f
#> 3066:                    <NA>                         <NA>
#> 3067:                       f                            t
#> 3068:                       f                            f
#>       why.not.vaccine.minor why.not.vaccine.unlikely why.not.vaccine.cause
#>                      <fctr>                   <fctr>                <fctr>
#>    1:                     t                        f                     f
#>    2:                  <NA>                     <NA>                  <NA>
#>    3:                     f                        f                     f
#>    4:                     f                        f                     t
#>    5:                     f                        f                     f
#>   ---                                                                     
#> 3064:                     f                        t                     f
#> 3065:                     f                        f                     f
#> 3066:                  <NA>                     <NA>                  <NA>
#> 3067:                     f                        f                     f
#> 3068:                     f                        f                     f
#>       why.not.vaccine.side.effects why.not.vaccine.dont.like
#>                             <fctr>                    <fctr>
#>    1:                            f                         f
#>    2:                         <NA>                      <NA>
#>    3:                            f                         f
#>    4:                            f                         f
#>    5:                            f                         f
#>   ---                                                       
#> 3064:                            f                         f
#> 3065:                            f                         f
#> 3066:                         <NA>                      <NA>
#> 3067:                            f                         f
#> 3068:                            f                         t
#>       why.not.vaccine.unavailable why.not.vaccine.not.free
#>                            <fctr>                   <fctr>
#>    1:                           f                        f
#>    2:                        <NA>                     <NA>
#>    3:                           f                        f
#>    4:                           f                        f
#>    5:                           f                        f
#>   ---                                                     
#> 3064:                           f                        f
#> 3065:                           f                        f
#> 3066:                        <NA>                     <NA>
#> 3067:                           f                        f
#> 3068:                           f                        f
#>       why.not.vaccine.no.reason why.not.vaccine.doctor why.not.vaccine.other
#>                          <fctr>                 <fctr>                <fctr>
#>    1:                         f                      f                     f
#>    2:                      <NA>                   <NA>                  <NA>
#>    3:                         f                      f                     f
#>    4:                         f                      f                     f
#>    5:                         t                      f                     f
#>   ---                                                                       
#> 3064:                         f                      f                     f
#> 3065:                         f                      f                     f
#> 3066:                      <NA>                   <NA>                  <NA>
#> 3067:                         f                      f                     f
#> 3068:                         f                      f                     f
#>       risk.none risk.asthma risk.diabetes risk.lung risk.heart risk.kidney
#>          <fctr>      <fctr>        <fctr>    <fctr>     <fctr>      <fctr>
#>    1:         t           f             f         f          f           f
#>    2:         t           f             f         f          f           f
#>    3:         t           f             f         f          f           f
#>    4:         t           f             f         f          f           f
#>    5:         t           f             f         f          f           f
#>   ---                                                                     
#> 3064:         t           f             f         f          f           f
#> 3065:         t           f             f         f          f           f
#> 3066:         t           f             f         f          f           f
#> 3067:         t           f             f         f          f           f
#> 3068:         t           f             f         f          f           f
#>       risk.immune pregnant pregnant.trimester        smoke allergy.hayfever
#>            <fctr>   <fctr>              <int>       <fctr>           <fctr>
#>    1:           f     <NA>                 NA           no                f
#>    2:           f     <NA>                 NA           no                f
#>    3:           f     <NA>                 NA           no                f
#>    4:           f     <NA>                 NA           no                f
#>    5:           f     <NA>                 NA           no                f
#>   ---                                                                      
#> 3064:           f     <NA>                 NA           no                f
#> 3065:           f     <NA>                 NA           no                f
#> 3066:           f     <NA>                 NA           no                f
#> 3067:           f      yes                  1           no                f
#> 3068:           f     <NA>                 NA occasionally                f
#>       allergy.dust allergy.animals allergy.other allergy.none diet.none
#>             <fctr>          <fctr>        <fctr>       <fctr>    <fctr>
#>    1:            f               f             f            t         t
#>    2:            f               f             f            t         t
#>    3:            f               f             f            t         t
#>    4:            f               f             f            t         t
#>    5:            f               f             f            t         t
#>   ---                                                                  
#> 3064:            f               t             f            f         t
#> 3065:            f               f             f            t         t
#> 3066:            f               f             f            t         t
#> 3067:            f               f             f            t         t
#> 3068:            f               f             f            t         t
#>       diet.vegetarian diet.vegan diet.low-calorie diet.other pets.none
#>                <fctr>     <fctr>           <fctr>     <fctr>    <fctr>
#>    1:               f          f                f          f         t
#>    2:               f          f                f          f         t
#>    3:               f          f                f          f         t
#>    4:               f          f                f          f         t
#>    5:               f          f                f          f         t
#>   ---                                                                 
#> 3064:               f          f                f          f         t
#> 3065:               f          f                f          f         t
#> 3066:               f          f                f          f         t
#> 3067:               f          f                f          f         t
#> 3068:               f          f                f          f         f
#>       pets.dogs pets.cats pets.birds pets.other howhear.radio.tv
#>          <fctr>    <fctr>     <fctr>     <fctr>           <fctr>
#>    1:         f         f          f          f                f
#>    2:         f         f          f          f                t
#>    3:         f         f          f          f                f
#>    4:         f         f          f          f                f
#>    5:         f         f          f          f                f
#>   ---                                                           
#> 3064:         f         f          f          f                f
#> 3065:         f         f          f          f                f
#> 3066:         f         f          f          f                f
#> 3067:         f         f          f          f                f
#> 3068:         t         f          f          f                f
#>       howhear.paper.magazine howhear.internet howhear.poster
#>                       <fctr>           <fctr>         <fctr>
#>    1:                      f                f              f
#>    2:                      f                f              f
#>    3:                      f                f              f
#>    4:                      f                t              f
#>    5:                      t                t              f
#>   ---                                                       
#> 3064:                      f                t              f
#> 3065:                      f                f              f
#> 3066:                      f                f              f
#> 3067:                      f                t              f
#> 3068:                      f                t              f
#>       howhear.family.friends howhear.other       date  birthdate   age agegroup
#>                       <fctr>        <fctr>     <Date>     <Date> <num>   <fctr>
#>    1:                      f             t 2012-11-11 1989-11-01    23    19-44
#>    2:                      f             f 2012-11-11 1941-12-01    70      65+
#>    3:                      f             t 2012-11-11 2011-06-01     1     0-18
#>    4:                      t             f 2012-11-11 1954-07-01    58    45-64
#>    5:                      f             t 2012-11-11 2002-01-01    10     0-18
#>   ---                                                                          
#> 3064:                      f             f 2013-04-10 2010-05-01     2     0-18
#> 3065:                      t             f 2013-04-10 1968-12-01    43    19-44
#> 3066:                      f             f 2013-04-10 1987-12-01    24    19-44
#> 3067:                      f             f 2013-04-10 1975-07-01    37    19-44
#> 3068:                      f             f 2013-04-10 1940-01-01    72      65+
#>       living.with.children  country settlement.type             region
#>                      <int>   <fctr>           <int>             <fctr>
#>    1:                    1 scotland               3           scotland
#>    2:                    0 scotland               1           scotland
#>    3:                    1  england               5 south_east_england
#>    4:                    0  england               6 south_east_england
#>    5:                    1 scotland               7           scotland
#>   ---                                                                 
#> 3064:                    0  england               5             london
#> 3065:                    1  england               5 south_east_england
#> 3066:                    0  england               5             london
#> 3067:                    1  england               5 south_east_england
#> 3068:                    1  england               5      east_midlands
#>       urban.rural work.country work.settlement.type        work.region
#>            <fctr>       <fctr>                <int>             <fctr>
#>    1:       rural        wales                    2              wales
#>    2:       urban         <NA>                   NA               <NA>
#>    3:       urban         <NA>                   NA               <NA>
#>    4:       rural      england                    5 north_west_england
#>    5:       rural         <NA>                   NA               <NA>
#>   ---                                                                 
#> 3064:       urban         <NA>                   NA               <NA>
#> 3065:       urban         <NA>                   NA               <NA>
#> 3066:       urban     scotland                    1           scotland
#> 3067:       urban      england                    5      west_midlands
#> 3068:       urban         <NA>                   NA               <NA>
#>       work.urban.rural highest.education nb.household nb.household.children
#>                 <fctr>            <fctr>        <num>                 <num>
#>    1:            urban    education.gcse            3                     3
#>    2:             <NA> education.alevels            0                     0
#>    3:             <NA> education.stillin            6                     2
#>    4:            urban     education.msc            2                     0
#>    5:             <NA> education.stillin            4                     1
#>   ---                                                                      
#> 3064:             <NA> education.stillin            0                     0
#> 3065:             <NA>    education.gcse            2                     1
#> 3066:            urban    education.gcse            1                     0
#> 3067:            urban     education.bsc            3                     2
#> 3068:             <NA>    education.gcse            1                     1
#>       contact.id conversational.home.0-4 conversational.home.5-18
#>            <int>                   <int>                    <int>
#>    1:         12                       0                        1
#>    2:        108                       0                        2
#>    3:        173                       1                        1
#>    4:        341                       0                        0
#>    5:        472                       0                        1
#>   ---                                                            
#> 3064:        546                       2                        0
#> 3065:        791                       1                        0
#> 3066:        808                       2                        0
#> 3067:        887                       0                        0
#> 3068:        942                       1                        2
#>       conversational.home.19-44 conversational.home.45-64
#>                           <int>                     <int>
#>    1:                         0                         1
#>    2:                         3                         0
#>    3:                         3                         2
#>    4:                         0                         0
#>    5:                         0                         0
#>   ---                                                    
#> 3064:                         0                         0
#> 3065:                         0                         0
#> 3066:                         1                         0
#> 3067:                         1                         0
#> 3068:                         2                         1
#>       conversational.home.65+ conversational.work.0-4 conversational.work.5-18
#>                         <int>                   <int>                    <int>
#>    1:                       1                       3                        1
#>    2:                       1                       0                        1
#>    3:                       1                       1                        3
#>    4:                       0                       1                        3
#>    5:                       2                       1                        2
#>   ---                                                                         
#> 3064:                       0                       1                        1
#> 3065:                       2                       2                        0
#> 3066:                       0                       0                        1
#> 3067:                       0                       0                        2
#> 3068:                       1                       0                        0
#>       conversational.work.19-44 conversational.work.45-64
#>                           <int>                     <int>
#>    1:                         3                         0
#>    2:                         0                         2
#>    3:                         6                         0
#>    4:                         6                         9
#>    5:                         2                         1
#>   ---                                                    
#> 3064:                         5                         1
#> 3065:                         1                         2
#> 3066:                         0                         2
#> 3067:                         2                         4
#> 3068:                         0                         0
#>       conversational.work.65+ conversational.other.0-4
#>                         <int>                    <int>
#>    1:                       3                        0
#>    2:                       0                        0
#>    3:                       0                        2
#>    4:                       1                        0
#>    5:                       1                        1
#>   ---                                                 
#> 3064:                       2                        1
#> 3065:                       1                        1
#> 3066:                       2                        0
#> 3067:                       0                        2
#> 3068:                       0                        3
#>       conversational.other.5-18 conversational.other.19-44
#>                           <int>                      <int>
#>    1:                         3                          2
#>    2:                         0                          2
#>    3:                         0                          1
#>    4:                         1                          1
#>    5:                         0                          4
#>   ---                                                     
#> 3064:                         1                          2
#> 3065:                         1                          0
#> 3066:                         1                          0
#> 3067:                         1                          2
#> 3068:                         0                          2
#>       conversational.other.45-64 conversational.other.65+ physical.home.0-4
#>                            <int>                    <int>             <int>
#>    1:                          0                        1                 0
#>    2:                          4                        0                 0
#>    3:                          1                        1                 1
#>    4:                          0                        2                 0
#>    5:                          1                        1                 0
#>   ---                                                                      
#> 3064:                          1                        1                 1
#> 3065:                          1                        4                 1
#> 3066:                          1                        0                 0
#> 3067:                          0                        1                 0
#> 3068:                          0                        3                 1
#>       physical.home.5-18 physical.home.19-44 physical.home.45-64
#>                    <int>               <int>               <int>
#>    1:                  1                   0                   1
#>    2:                  0                   2                   0
#>    3:                  1                   2                   1
#>    4:                  0                   0                   0
#>    5:                  0                   0                   0
#>   ---                                                           
#> 3064:                  0                   0                   0
#> 3065:                  0                   0                   0
#> 3066:                  0                   0                   0
#> 3067:                  0                   0                   0
#> 3068:                  1                   2                   0
#>       physical.home.65+ physical.work.0-4 physical.work.5-18
#>                   <int>             <int>              <int>
#>    1:                 0                 2                  1
#>    2:                 1                 0                  0
#>    3:                 0                 0                  0
#>    4:                 0                 1                  0
#>    5:                 1                 0                  0
#>   ---                                                       
#> 3064:                 0                 1                  1
#> 3065:                 1                 1                  0
#> 3066:                 0                 0                  1
#> 3067:                 0                 0                  1
#> 3068:                 0                 0                  0
#>       physical.work.19-44 physical.work.45-64 physical.work.65+
#>                     <int>               <int>             <int>
#>    1:                   3                   0                 2
#>    2:                   0                   1                 0
#>    3:                   4                   0                 0
#>    4:                   2                   3                 0
#>    5:                   0                   0                 0
#>   ---                                                          
#> 3064:                   2                   0                 1
#> 3065:                   1                   1                 1
#> 3066:                   0                   1                 1
#> 3067:                   2                   1                 0
#> 3068:                   0                   0                 0
#>       physical.other.0-4 physical.other.5-18 physical.other.19-44
#>                    <int>               <int>                <int>
#>    1:                  0                   1                    1
#>    2:                  0                   0                    1
#>    3:                  1                   0                    0
#>    4:                  0                   0                    0
#>    5:                  1                   0                    2
#>   ---                                                            
#> 3064:                  1                   0                    0
#> 3065:                  1                   0                    0
#> 3066:                  0                   1                    0
#> 3067:                  0                   0                    0
#> 3068:                  0                   0                    1
#>       physical.other.45-64 physical.other.65+ public.transport
#>                      <int>              <int>           <fctr>
#>    1:                    0                  0 (90,240]_minutes
#>    2:                    3                  0 (90,240]_minutes
#>    3:                    0                  0   (0,30]_minutes
#>    4:                    0                  2  (30,90]_minutes
#>    5:                    1                  0             none
#>   ---                                                         
#> 3064:                    0                  0  (30,90]_minutes
#> 3065:                    0                  3  (30,90]_minutes
#> 3066:                    1                  0  (30,90]_minutes
#> 3067:                    0                  1   (0,30]_minutes
#> 3068:                    0                  0             none
#>       enclosed.indoor.space furthest.travelled conversational
#>                      <fctr>             <fctr>          <int>
#>    1:      (90,240]_minutes        [0,1)_miles             19
#>    2:       (30,90]_minutes     [30,100]_miles             15
#>    3:       (30,90]_minutes       [5,10)_miles             23
#>    4:                  none       [5,10)_miles             24
#>    5:       (30,90]_minutes        [1,5)_miles             17
#>   ---                                                        
#> 3064:                  none      [10,30)_miles             18
#> 3065:                  none        [0,1)_miles             16
#> 3066:                  none      [10,30)_miles             10
#> 3067:        (240,)_minutes       (100,)_miles             15
#> 3068:        (0,30]_minutes     [30,100]_miles             15
#>       conversational.home conversational.work conversational.other
#>                     <int>               <int>                <int>
#>    1:                   3                  10                    6
#>    2:                   6                   3                    6
#>    3:                   8                  10                    5
#>    4:                   0                  20                    4
#>    5:                   3                   7                    7
#>   ---                                                             
#> 3064:                   2                  10                    6
#> 3065:                   3                   6                    7
#> 3066:                   3                   5                    2
#> 3067:                   1                   8                    6
#> 3068:                   7                   0                    8
#>       conversational.0-4 conversational.5-18 conversational.19-44
#>                    <int>               <int>                <int>
#>    1:                  3                   5                    5
#>    2:                  0                   3                    5
#>    3:                  4                   4                   10
#>    4:                  1                   4                    7
#>    5:                  2                   3                    6
#>   ---                                                            
#> 3064:                  4                   2                    7
#> 3065:                  4                   1                    1
#> 3066:                  2                   2                    1
#> 3067:                  2                   3                    5
#> 3068:                  4                   2                    4
#>       conversational.45-64 conversational.65+ physical physical.home
#>                      <int>              <int>    <int>         <int>
#>    1:                    1                  5       12             2
#>    2:                    6                  1        8             3
#>    3:                    3                  2       10             5
#>    4:                    9                  3        8             0
#>    5:                    2                  4        5             1
#>   ---                                                               
#> 3064:                    2                  3        7             1
#> 3065:                    3                  7       10             2
#> 3066:                    3                  2        5             0
#> 3067:                    4                  1        5             0
#> 3068:                    1                  4        5             4
#>       physical.work physical.other physical.0-4 physical.5-18 physical.19-44
#>               <int>          <int>        <int>         <int>          <int>
#>    1:             8              2            2             3              4
#>    2:             1              4            0             0              3
#>    3:             4              1            2             1              6
#>    4:             6              2            1             0              2
#>    5:             0              4            1             0              2
#>   ---                                                                       
#> 3064:             5              1            3             1              2
#> 3065:             4              4            3             0              1
#> 3066:             3              2            0             2              0
#> 3067:             4              1            0             1              2
#> 3068:             0              1            1             1              3
#>       physical.45-64 physical.65+ contact.date contact.day.of.week symptom.id
#>                <int>        <int>       <Date>               <num>      <int>
#>    1:              1            2   2012-11-10                   6         28
#>    2:              4            1   2012-11-10                   6        340
#>    3:              1            0   2012-11-10                   6        598
#>    4:              3            2   2012-11-10                   6       1205
#>    5:              1            1   2012-11-10                   6       1589
#>   ---                                                                        
#> 3064:              0            1   2013-02-19                   2       1824
#> 3065:              1            5   2013-04-09                   2       2558
#> 3066:              2            1   2013-02-18                   1       2616
#> 3067:              1            1   2012-12-20                   4       2890
#> 3068:              0            0   2013-01-26                   6       3031
#>       no.symptoms  fever chills blocked.runny.nose sneezing sore.throat  cough
#>            <fctr> <fctr> <fctr>             <fctr>   <fctr>      <fctr> <fctr>
#>    1:           t      f      f                  f        f           f      f
#>    2:           t      f      f                  f        f           f      f
#>    3:           t      f      f                  f        f           f      f
#>    4:           t      f      f                  f        f           f      f
#>    5:           t      f      f                  f        f           f      f
#>   ---                                                                         
#> 3064:           t      f      f                  f        f           f      f
#> 3065:           t      f      f                  f        f           f      f
#> 3066:           t      f      f                  f        f           f      f
#> 3067:           t      f      f                  f        f           f      f
#> 3068:           t      f      f                  f        f           f      f
#>       shortness.breath headache muscle.and.or.joint.pain chest.pain  tired
#>                 <fctr>   <fctr>                   <fctr>     <fctr> <fctr>
#>    1:                f        f                        f          f      f
#>    2:                f        f                        f          f      f
#>    3:                f        f                        f          f      f
#>    4:                f        f                        f          f      f
#>    5:                f        f                        f          f      f
#>   ---                                                                     
#> 3064:                f        f                        f          f      f
#> 3065:                f        f                        f          f      f
#> 3066:                f        f                        f          f      f
#> 3067:                f        f                        f          f      f
#> 3068:                f        f                        f          f      f
#>       loss.appetite phlegm watery.eyes nausea vomiting diarrhoea stomach.ache
#>              <fctr> <fctr>      <fctr> <fctr>   <fctr>    <fctr>       <fctr>
#>    1:             f      f           f      f        f         f            f
#>    2:             f      f           f      f        f         f            f
#>    3:             f      f           f      f        f         f            f
#>    4:             f      f           f      f        f         f            f
#>    5:             f      f           f      f        f         f            f
#>   ---                                                                        
#> 3064:             f      f           f      f        f         f            f
#> 3065:             f      f           f      f        f         f            f
#> 3066:             f      f           f      f        f         f            f
#> 3067:             f      f           f      f        f         f            f
#> 3068:             f      f           f      f        f         f            f
#>       other.symptoms   same symptoms.start.option symptoms.start.date
#>               <fctr> <fctr>                <fctr>              <Date>
#>    1:              f   <NA>                  <NA>                <NA>
#>    2:              f   <NA>                  <NA>                <NA>
#>    3:              f   <NA>                  <NA>                <NA>
#>    4:              f   <NA>                  <NA>                <NA>
#>    5:              f   <NA>                  <NA>                <NA>
#>   ---                                                                
#> 3064:              f   <NA>                  <NA>                <NA>
#> 3065:              f   <NA>                  <NA>                <NA>
#> 3066:              f   <NA>                  <NA>                <NA>
#> 3067:              f   <NA>                  <NA>                <NA>
#> 3068:              f   <NA>                  <NA>                <NA>
#>       symptoms.end.option symptoms.end.date symptoms.suddenly
#>                    <fctr>            <Date>            <fctr>
#>    1:                <NA>              <NA>              <NA>
#>    2:                <NA>              <NA>              <NA>
#>    3:                <NA>              <NA>              <NA>
#>    4:                <NA>              <NA>              <NA>
#>    5:                <NA>              <NA>              <NA>
#>   ---                                                        
#> 3064:                <NA>              <NA>              <NA>
#> 3065:                <NA>              <NA>              <NA>
#> 3066:                <NA>              <NA>              <NA>
#> 3067:                <NA>              <NA>              <NA>
#> 3068:                <NA>              <NA>              <NA>
#>       fever.start.option fever.start.date fever.suddenly fever.temperature
#>                   <fctr>           <Date>         <fctr>            <fctr>
#>    1:               <NA>             <NA>           <NA>              <NA>
#>    2:               <NA>             <NA>           <NA>              <NA>
#>    3:               <NA>             <NA>           <NA>              <NA>
#>    4:               <NA>             <NA>           <NA>              <NA>
#>    5:               <NA>             <NA>           <NA>              <NA>
#>   ---                                                                     
#> 3064:               <NA>             <NA>           <NA>              <NA>
#> 3065:               <NA>             <NA>           <NA>              <NA>
#> 3066:               <NA>             <NA>           <NA>              <NA>
#> 3067:               <NA>             <NA>           <NA>              <NA>
#> 3068:               <NA>             <NA>           <NA>              <NA>
#>       fever.temperature.value visit.medical.service.no visit.medical.service.gp
#>                        <fctr>                   <fctr>                   <fctr>
#>    1:                    <NA>                     <NA>                     <NA>
#>    2:                    <NA>                     <NA>                     <NA>
#>    3:                    <NA>                     <NA>                     <NA>
#>    4:                    <NA>                     <NA>                     <NA>
#>    5:                    <NA>                     <NA>                     <NA>
#>   ---                                                                          
#> 3064:                    <NA>                     <NA>                     <NA>
#> 3065:                    <NA>                     <NA>                     <NA>
#> 3066:                    <NA>                     <NA>                     <NA>
#> 3067:                    <NA>                     <NA>                     <NA>
#> 3068:                    <NA>                     <NA>                     <NA>
#>       visit.medical.service.ae visit.medical.service.hospital
#>                         <fctr>                         <fctr>
#>    1:                     <NA>                           <NA>
#>    2:                     <NA>                           <NA>
#>    3:                     <NA>                           <NA>
#>    4:                     <NA>                           <NA>
#>    5:                     <NA>                           <NA>
#>   ---                                                        
#> 3064:                     <NA>                           <NA>
#> 3065:                     <NA>                           <NA>
#> 3066:                     <NA>                           <NA>
#> 3067:                     <NA>                           <NA>
#> 3068:                     <NA>                           <NA>
#>       visit.medical.service.other visit.medical.service.appointment
#>                            <fctr>                            <fctr>
#>    1:                        <NA>                              <NA>
#>    2:                        <NA>                              <NA>
#>    3:                        <NA>                              <NA>
#>    4:                        <NA>                              <NA>
#>    5:                        <NA>                              <NA>
#>   ---                                                              
#> 3064:                        <NA>                              <NA>
#> 3065:                        <NA>                              <NA>
#> 3066:                        <NA>                              <NA>
#> 3067:                        <NA>                              <NA>
#> 3068:                        <NA>                              <NA>
#>       visit.medical.service.howsoon.gp.receptionist
#>                                              <fctr>
#>    1:                                          <NA>
#>    2:                                          <NA>
#>    3:                                          <NA>
#>    4:                                          <NA>
#>    5:                                          <NA>
#>   ---                                              
#> 3064:                                          <NA>
#> 3065:                                          <NA>
#> 3066:                                          <NA>
#> 3067:                                          <NA>
#> 3068:                                          <NA>
#>       visit.medical.service.howsoon.gp.doctor.nurse
#>                                              <fctr>
#>    1:                                          <NA>
#>    2:                                          <NA>
#>    3:                                          <NA>
#>    4:                                          <NA>
#>    5:                                          <NA>
#>   ---                                              
#> 3064:                                          <NA>
#> 3065:                                          <NA>
#> 3066:                                          <NA>
#> 3067:                                          <NA>
#> 3068:                                          <NA>
#>       visit.medical.service.howsoon.nhs visit.medical.service.howsoon.other
#>                                  <fctr>                              <fctr>
#>    1:                              <NA>                                <NA>
#>    2:                              <NA>                                <NA>
#>    3:                              <NA>                                <NA>
#>    4:                              <NA>                                <NA>
#>    5:                              <NA>                                <NA>
#>   ---                                                                      
#> 3064:                              <NA>                                <NA>
#> 3065:                              <NA>                                <NA>
#> 3066:                              <NA>                                <NA>
#> 3067:                              <NA>                                <NA>
#> 3068:                              <NA>                                <NA>
#>       contact.medical.service.no contact.medical.service.gp.receptionist
#>                           <fctr>                                  <fctr>
#>    1:                       <NA>                                    <NA>
#>    2:                       <NA>                                    <NA>
#>    3:                       <NA>                                    <NA>
#>    4:                       <NA>                                    <NA>
#>    5:                       <NA>                                    <NA>
#>   ---                                                                   
#> 3064:                       <NA>                                    <NA>
#> 3065:                       <NA>                                    <NA>
#> 3066:                       <NA>                                    <NA>
#> 3067:                       <NA>                                    <NA>
#> 3068:                       <NA>                                    <NA>
#>       contact.medical.service.gp.doctor contact.medical.service.nhs
#>                                  <fctr>                      <fctr>
#>    1:                              <NA>                        <NA>
#>    2:                              <NA>                        <NA>
#>    3:                              <NA>                        <NA>
#>    4:                              <NA>                        <NA>
#>    5:                              <NA>                        <NA>
#>   ---                                                              
#> 3064:                              <NA>                        <NA>
#> 3065:                              <NA>                        <NA>
#> 3066:                              <NA>                        <NA>
#> 3067:                              <NA>                        <NA>
#> 3068:                              <NA>                        <NA>
#>       contact.medical.service.npfs contact.medical.service.other
#>                             <fctr>                        <fctr>
#>    1:                         <NA>                          <NA>
#>    2:                         <NA>                          <NA>
#>    3:                         <NA>                          <NA>
#>    4:                         <NA>                          <NA>
#>    5:                         <NA>                          <NA>
#>   ---                                                           
#> 3064:                         <NA>                          <NA>
#> 3065:                         <NA>                          <NA>
#> 3066:                         <NA>                          <NA>
#> 3067:                         <NA>                          <NA>
#> 3068:                         <NA>                          <NA>
#>       contact.medical.service.howsoon.gp.receptionist
#>                                                <fctr>
#>    1:                                            <NA>
#>    2:                                            <NA>
#>    3:                                            <NA>
#>    4:                                            <NA>
#>    5:                                            <NA>
#>   ---                                                
#> 3064:                                            <NA>
#> 3065:                                            <NA>
#> 3066:                                            <NA>
#> 3067:                                            <NA>
#> 3068:                                            <NA>
#>       contact.medical.service.howsoon.gp.doctor.nurse
#>                                                <fctr>
#>    1:                                            <NA>
#>    2:                                            <NA>
#>    3:                                            <NA>
#>    4:                                            <NA>
#>    5:                                            <NA>
#>   ---                                                
#> 3064:                                            <NA>
#> 3065:                                            <NA>
#> 3066:                                            <NA>
#> 3067:                                            <NA>
#> 3068:                                            <NA>
#>       contact.medical.service.howsoon.nhs contact.medical.service.howsoon.other
#>                                    <fctr>                                <fctr>
#>    1:                                <NA>                                  <NA>
#>    2:                                <NA>                                  <NA>
#>    3:                                <NA>                                  <NA>
#>    4:                                <NA>                                  <NA>
#>    5:                                <NA>                                  <NA>
#>   ---                                                                          
#> 3064:                                <NA>                                  <NA>
#> 3065:                                <NA>                                  <NA>
#> 3066:                                <NA>                                  <NA>
#> 3067:                                <NA>                                  <NA>
#> 3068:                                <NA>                                  <NA>
#>       no.medication medication.painkillers medication.cough
#>              <fctr>                 <fctr>           <fctr>
#>    1:          <NA>                   <NA>             <NA>
#>    2:          <NA>                   <NA>             <NA>
#>    3:          <NA>                   <NA>             <NA>
#>    4:          <NA>                   <NA>             <NA>
#>    5:          <NA>                   <NA>             <NA>
#>   ---                                                      
#> 3064:          <NA>                   <NA>             <NA>
#> 3065:          <NA>                   <NA>             <NA>
#> 3066:          <NA>                   <NA>             <NA>
#> 3067:          <NA>                   <NA>             <NA>
#> 3068:          <NA>                   <NA>             <NA>
#>       medication.antiviral medication.antibiotic medication.other
#>                     <fctr>                <fctr>           <fctr>
#>    1:                 <NA>                  <NA>             <NA>
#>    2:                 <NA>                  <NA>             <NA>
#>    3:                 <NA>                  <NA>             <NA>
#>    4:                 <NA>                  <NA>             <NA>
#>    5:                 <NA>                  <NA>             <NA>
#>   ---                                                            
#> 3064:                 <NA>                  <NA>             <NA>
#> 3065:                 <NA>                  <NA>             <NA>
#> 3066:                 <NA>                  <NA>             <NA>
#> 3067:                 <NA>                  <NA>             <NA>
#> 3068:                 <NA>                  <NA>             <NA>
#>       medication.dontknow medication.howsoon alter.routine still.altered
#>                    <fctr>              <int>        <fctr>        <fctr>
#>    1:                <NA>                 NA          <NA>          <NA>
#>    2:                <NA>                 NA          <NA>          <NA>
#>    3:                <NA>                 NA          <NA>          <NA>
#>    4:                <NA>                 NA          <NA>          <NA>
#>    5:                <NA>                 NA          <NA>          <NA>
#>   ---                                                                   
#> 3064:                <NA>                 NA          <NA>          <NA>
#> 3065:                <NA>                 NA          <NA>          <NA>
#> 3066:                <NA>                 NA          <NA>          <NA>
#> 3067:                <NA>                 NA          <NA>          <NA>
#> 3068:                <NA>                 NA          <NA>          <NA>
#>       howlong.altered what.do.you.think health.score        onset suddenly
#>                <fctr>            <fctr>        <int>       <char>   <fctr>
#>    1:            <NA>              <NA>           86 not.answered     <NA>
#>    2:            <NA>              <NA>           98 not.answered     <NA>
#>    3:            <NA>              <NA>           87 not.answered     <NA>
#>    4:            <NA>              <NA>           88 not.answered     <NA>
#>    5:            <NA>              <NA>           88 not.answered     <NA>
#>   ---                                                                     
#> 3064:            <NA>              <NA>           93 not.answered     <NA>
#> 3065:            <NA>              <NA>           87 not.answered     <NA>
#> 3066:            <NA>              <NA>           81 not.answered     <NA>
#> 3067:            <NA>              <NA>           91 not.answered     <NA>
#> 3068:            <NA>              <NA>           84 not.answered     <NA>
#>       fever.symptoms systemic.symptoms resp.symptoms general.symptoms
#>               <fctr>            <fctr>        <fctr>           <fctr>
#>    1:              f                 f             f                f
#>    2:              f                 f             f                f
#>    3:              f                 f             f                f
#>    4:              f                 f             f                f
#>    5:              f                 f             f                f
#>   ---                                                                
#> 3064:              f                 f             f                f
#> 3065:              f                 f             f                f
#> 3066:              f                 f             f                f
#> 3067:              f                 f             f                f
#> 3068:              f                 f             f                f
#>       ili.symptoms    ili ili.fever ili.self     gi nReports   min.date
#>             <fctr> <fctr>    <fctr>   <fctr> <fctr>    <int>     <Date>
#>    1:            f      f         f        f      f        8 2012-11-05
#>    2:            f      f         f        f      f       21 2012-11-05
#>    3:            f      f         f        f      f       16 2012-11-05
#>    4:            f      f         f        f      f        4 2012-11-05
#>    5:            f      f         f        f      f       19 2012-11-05
#>   ---                                                                  
#> 3064:            f      f         f        f      f        9 2013-02-12
#> 3065:            f      f         f        f      f       22 2012-11-05
#> 3066:            f      f         f        f      f       19 2012-11-18
#> 3067:            f      f         f        f      f       20 2012-11-23
#> 3068:            f      f         f        f      f       15 2012-12-10
#>         max.date day.of.week month
#>           <Date>       <num> <int>
#>    1: 2012-12-22           7    11
#>    2: 2013-03-30           7    11
#>    3: 2013-04-05           7    11
#>    4: 2012-11-26           7    11
#>    5: 2013-03-18           7    11
#>   ---                             
#> 3064: 2013-04-10           3     4
#> 3065: 2013-04-10           3     4
#> 3066: 2013-04-10           3     4
#> 3067: 2013-04-10           3     4
#> 3068: 2013-04-10           3     4
```

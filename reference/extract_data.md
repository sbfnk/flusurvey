# Extract flusurvey data

This extracts flusurvey data for a given number of surveys and years
(all by default).

## Usage

``` r
extract_data(data, surveys = "all", years = "all", join = TRUE, ...)
```

## Arguments

- data:

  either a (named) list of (annual, named) lists of surveys, or (much
  easier) a filename that contains said list.

- surveys:

  the surveys to extract (e.g., "background", "symptom", "contact" or a
  combination thereof), "all" for all of surveys that can be found and
  joinable by `merge_data` (others are dropped with a warning); surveys
  that don't exist in all years are dropped too. The order given does
  not matter. The surveys are always joined in the order background,
  contact, symptom (see
  [`merge_data`](https://sbfnk.github.io/flusurvey/reference/merge_data.md)),
  with one row per report of the last survey in that order.

- years:

  the years to extract, "all" for all years

- join:

  whether to join the data into one big data.table in the end

- ...:

  parameters to be passed to merge_data, especially 'clean' (see
  documentation of
  [`merge_data`](https://sbfnk.github.io/flusurvey/reference/merge_data.md),
  by default all cleaning will be performed, set 'clean' to NULL to get
  the raw data).

## Value

a data.table (if `join` is TRUE) or a list of data.tables (if `join` is
FALSE) with the data

## Author

seb

## Examples

``` r
dt <- extract_data(simulated_data, years = 2013:2014)
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use flag_ill_contacts().
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use flag_ill_contacts().
table(dt$season)
#> 
#> 2012/13 2013/14 
#>    2898    2836 

## without the contact questionnaire
extract_data(simulated_data, years = 2013:2014,
             surveys = c("background", "symptom"))
#> Key: <global_id>
#>       global_id background.id   self gender birthmonth
#>          <char>        <char> <fctr> <fctr>     <char>
#>    1:   sim0001        2014.1   self   male    1966-08
#>    2:   sim0001        2014.1   self   male    1966-08
#>    3:   sim0001        2014.1   self   male    1966-08
#>    4:   sim0001        2014.1   self   male    1966-08
#>    5:   sim0002        2013.2   self   male    1973-02
#>   ---                                                 
#> 5750:   sim0400      2014.313   self   male    1996-04
#> 5751:   sim0400      2014.313   self   male    1996-04
#> 5752:   sim0400      2014.313   self   male    1996-04
#> 5753:   sim0400      2014.313   self   male    1996-04
#> 5754:   sim0400      2014.313   self   male    1996-04
#>                   main.activity work.postcode.option   occupation no.education
#>                          <fctr>               <fctr>       <fctr>       <fctr>
#>    1: paid_employment_full_time                  yes other_manual         <NA>
#>    2: paid_employment_full_time                  yes other_manual         <NA>
#>    3: paid_employment_full_time                  yes other_manual         <NA>
#>    4: paid_employment_full_time                  yes other_manual         <NA>
#>    5: paid_employment_full_time                  yes professional            f
#>   ---                                                                         
#> 5750:                    school                 <NA>         <NA>         <NA>
#> 5751:                    school                 <NA>         <NA>         <NA>
#> 5752:                    school                 <NA>         <NA>         <NA>
#> 5753:                    school                 <NA>         <NA>         <NA>
#> 5754:                    school                 <NA>         <NA>         <NA>
#>       education.gcse education.alevels education.bsc education.msc
#>               <fctr>            <fctr>        <fctr>        <fctr>
#>    1:           <NA>              <NA>          <NA>          <NA>
#>    2:           <NA>              <NA>          <NA>          <NA>
#>    3:           <NA>              <NA>          <NA>          <NA>
#>    4:           <NA>              <NA>          <NA>          <NA>
#>    5:              t                 f             f             f
#>   ---                                                             
#> 5750:           <NA>              <NA>          <NA>          <NA>
#> 5751:           <NA>              <NA>          <NA>          <NA>
#> 5752:           <NA>              <NA>          <NA>          <NA>
#> 5753:           <NA>              <NA>          <NA>          <NA>
#> 5754:           <NA>              <NA>          <NA>          <NA>
#>       education.stillin frequent.contact.children frequent.contact.elderly
#>                  <fctr>                    <fctr>                   <fctr>
#>    1:              <NA>                         f                        f
#>    2:              <NA>                         f                        f
#>    3:              <NA>                         f                        f
#>    4:              <NA>                         f                        f
#>    5:                 f                         f                        t
#>   ---                                                                     
#> 5750:              <NA>                         f                        t
#> 5751:              <NA>                         f                        t
#> 5752:              <NA>                         f                        t
#> 5753:              <NA>                         f                        t
#> 5754:              <NA>                         f                        t
#>       frequent.contact.patients frequent.contact.people frequent.contact.none
#>                          <fctr>                  <fctr>                <fctr>
#>    1:                         f                       f                     t
#>    2:                         f                       f                     t
#>    3:                         f                       f                     t
#>    4:                         f                       f                     t
#>    5:                         f                       f                     f
#>   ---                                                                        
#> 5750:                         f                       f                     f
#> 5751:                         f                       f                     f
#> 5752:                         f                       f                     f
#> 5753:                         f                       f                     f
#> 5754:                         f                       f                     f
#>       household.0.4 nb.household.0.4 household.5.18 nb.household.5.18
#>              <fctr>            <int>         <fctr>             <int>
#>    1:             t                2              f                 0
#>    2:             t                2              f                 0
#>    3:             t                2              f                 0
#>    4:             t                2              f                 0
#>    5:             f                0              f                 0
#>   ---                                                                
#> 5750:             f                0              t                 1
#> 5751:             f                0              t                 1
#> 5752:             f                0              t                 1
#> 5753:             f                0              t                 1
#> 5754:             f                0              t                 1
#>       household.19.44 nb.household.19.44 household.45.64 nb.household.45.64
#>                <fctr>              <int>          <fctr>              <int>
#>    1:               f                  0               t                  1
#>    2:               f                  0               t                  1
#>    3:               f                  0               t                  1
#>    4:               f                  0               t                  1
#>    5:               t                  2               t                  2
#>   ---                                                                      
#> 5750:               t                  1               f                  0
#> 5751:               t                  1               f                  0
#> 5752:               t                  1               f                  0
#> 5753:               t                  1               f                  0
#> 5754:               t                  1               f                  0
#>       household.65+ nb.household.65+ children.school        transport
#>              <fctr>            <int>          <fctr>           <fctr>
#>    1:             f                0               3              car
#>    2:             f                0               3              car
#>    3:             f                0               3              car
#>    4:             f                0               3              car
#>    5:             t                1               1 public_transport
#>   ---                                                                
#> 5750:             f                0            none              car
#> 5751:             f                0            none              car
#> 5752:             f                0            none              car
#> 5753:             f                0            none              car
#> 5754:             f                0            none              car
#>       howlong.transport howoften.flulike vaccine.last.year vaccine.this.year
#>                  <fctr>           <fctr>            <fctr>            <fctr>
#>    1:   [30,90)_minutes   (2,5]_annually               yes                no
#>    2:   [30,90)_minutes   (2,5]_annually               yes                no
#>    3:   [30,90)_minutes   (2,5]_annually               yes                no
#>    4:   [30,90)_minutes   (2,5]_annually               yes                no
#>    5:    (0,30)_minutes   (2,5]_annually               yes                no
#>   ---                                                                       
#> 5750:   [30,90)_minutes   (0,2]_annually               yes                no
#> 5751:   [30,90)_minutes   (0,2]_annually               yes                no
#> 5752:   [30,90)_minutes   (0,2]_annually               yes                no
#> 5753:   [30,90)_minutes   (0,2]_annually               yes                no
#> 5754:   [30,90)_minutes   (0,2]_annually               yes                no
#>       date.vaccine.option date.vaccine why.vaccine.riskgroup
#>                    <fctr>       <char>                <fctr>
#>    1:                <NA>                               <NA>
#>    2:                <NA>                               <NA>
#>    3:                <NA>                               <NA>
#>    4:                <NA>                               <NA>
#>    5:                <NA>                               <NA>
#>   ---                                                       
#> 5750:                <NA>                               <NA>
#> 5751:                <NA>                               <NA>
#> 5752:                <NA>                               <NA>
#> 5753:                <NA>                               <NA>
#> 5754:                <NA>                               <NA>
#>       why.vaccine.protected why.vaccine.protect.others why.vaccine.doctor
#>                      <fctr>                     <fctr>             <fctr>
#>    1:                  <NA>                       <NA>               <NA>
#>    2:                  <NA>                       <NA>               <NA>
#>    3:                  <NA>                       <NA>               <NA>
#>    4:                  <NA>                       <NA>               <NA>
#>    5:                  <NA>                       <NA>               <NA>
#>   ---                                                                    
#> 5750:                  <NA>                       <NA>               <NA>
#> 5751:                  <NA>                       <NA>               <NA>
#> 5752:                  <NA>                       <NA>               <NA>
#> 5753:                  <NA>                       <NA>               <NA>
#> 5754:                  <NA>                       <NA>               <NA>
#>       why.vaccine.work.recommended why.vaccine.convenient why.vaccine.free
#>                             <fctr>                 <fctr>           <fctr>
#>    1:                         <NA>                   <NA>             <NA>
#>    2:                         <NA>                   <NA>             <NA>
#>    3:                         <NA>                   <NA>             <NA>
#>    4:                         <NA>                   <NA>             <NA>
#>    5:                         <NA>                   <NA>             <NA>
#>   ---                                                                     
#> 5750:                         <NA>                   <NA>             <NA>
#> 5751:                         <NA>                   <NA>             <NA>
#> 5752:                         <NA>                   <NA>             <NA>
#> 5753:                         <NA>                   <NA>             <NA>
#> 5754:                         <NA>                   <NA>             <NA>
#>       why.vaccine.nomiss.work why.vaccine.always why.vaccine.other
#>                        <fctr>             <fctr>            <fctr>
#>    1:                    <NA>               <NA>              <NA>
#>    2:                    <NA>               <NA>              <NA>
#>    3:                    <NA>               <NA>              <NA>
#>    4:                    <NA>               <NA>              <NA>
#>    5:                    <NA>               <NA>              <NA>
#>   ---                                                             
#> 5750:                    <NA>               <NA>              <NA>
#> 5751:                    <NA>               <NA>              <NA>
#> 5752:                    <NA>               <NA>              <NA>
#> 5753:                    <NA>               <NA>              <NA>
#> 5754:                    <NA>               <NA>              <NA>
#>       why.not.vaccine.notyet why.not.vaccine.notoffered why.not.vaccine.norisk
#>                       <fctr>                     <fctr>                 <fctr>
#>    1:                      f                          f                      f
#>    2:                      f                          f                      f
#>    3:                      f                          f                      f
#>    4:                      f                          f                      f
#>    5:                      t                          t                      f
#>   ---                                                                         
#> 5750:                      f                          f                      f
#> 5751:                      f                          f                      f
#> 5752:                      f                          f                      f
#> 5753:                      f                          f                      f
#> 5754:                      f                          f                      f
#>       why.not.vaccine.natural why.not.vaccine.noteffective
#>                        <fctr>                       <fctr>
#>    1:                       f                            f
#>    2:                       f                            f
#>    3:                       f                            f
#>    4:                       f                            f
#>    5:                       f                            f
#>   ---                                                     
#> 5750:                       f                            t
#> 5751:                       f                            t
#> 5752:                       f                            t
#> 5753:                       f                            t
#> 5754:                       f                            t
#>       why.not.vaccine.minor why.not.vaccine.unlikely why.not.vaccine.cause
#>                      <fctr>                   <fctr>                <fctr>
#>    1:                     t                        f                     f
#>    2:                     t                        f                     f
#>    3:                     t                        f                     f
#>    4:                     t                        f                     f
#>    5:                     t                        f                     f
#>   ---                                                                     
#> 5750:                     f                        t                     f
#> 5751:                     f                        t                     f
#> 5752:                     f                        t                     f
#> 5753:                     f                        t                     f
#> 5754:                     f                        t                     f
#>       why.not.vaccine.side.effects why.not.vaccine.dont.like
#>                             <fctr>                    <fctr>
#>    1:                            f                         f
#>    2:                            f                         f
#>    3:                            f                         f
#>    4:                            f                         f
#>    5:                            f                         f
#>   ---                                                       
#> 5750:                            f                         f
#> 5751:                            f                         f
#> 5752:                            f                         f
#> 5753:                            f                         f
#> 5754:                            f                         f
#>       why.not.vaccine.unavailable why.not.vaccine.not.free
#>                            <fctr>                   <fctr>
#>    1:                           f                        f
#>    2:                           f                        f
#>    3:                           f                        f
#>    4:                           f                        f
#>    5:                           f                        f
#>   ---                                                     
#> 5750:                           f                        f
#> 5751:                           f                        f
#> 5752:                           f                        f
#> 5753:                           f                        f
#> 5754:                           f                        f
#>       why.not.vaccine.no.reason why.not.vaccine.doctor why.not.vaccine.other
#>                          <fctr>                 <fctr>                <fctr>
#>    1:                         f                      f                     f
#>    2:                         f                      f                     f
#>    3:                         f                      f                     f
#>    4:                         f                      f                     f
#>    5:                         f                      f                     f
#>   ---                                                                       
#> 5750:                         f                      f                     f
#> 5751:                         f                      f                     f
#> 5752:                         f                      f                     f
#> 5753:                         f                      f                     f
#> 5754:                         f                      f                     f
#>       risk.none risk.asthma risk.diabetes risk.lung risk.heart risk.kidney
#>          <fctr>      <fctr>        <fctr>    <fctr>     <fctr>      <fctr>
#>    1:         t           f             f         f          f           f
#>    2:         t           f             f         f          f           f
#>    3:         t           f             f         f          f           f
#>    4:         t           f             f         f          f           f
#>    5:         t           f             f         f          f           f
#>   ---                                                                     
#> 5750:         t           f             f         f          f           f
#> 5751:         t           f             f         f          f           f
#> 5752:         t           f             f         f          f           f
#> 5753:         t           f             f         f          f           f
#> 5754:         t           f             f         f          f           f
#>       risk.immune pregnant pregnant.trimester       smoke allergy.hayfever
#>            <fctr>   <fctr>              <int>      <fctr>           <fctr>
#>    1:           f     <NA>                 NA (,10]_daily                f
#>    2:           f     <NA>                 NA (,10]_daily                f
#>    3:           f     <NA>                 NA (,10]_daily                f
#>    4:           f     <NA>                 NA (,10]_daily                f
#>    5:           f     <NA>                 NA          no                f
#>   ---                                                                     
#> 5750:           f     <NA>                 NA (,10]_daily                f
#> 5751:           f     <NA>                 NA (,10]_daily                f
#> 5752:           f     <NA>                 NA (,10]_daily                f
#> 5753:           f     <NA>                 NA (,10]_daily                f
#> 5754:           f     <NA>                 NA (,10]_daily                f
#>       allergy.dust allergy.animals allergy.other allergy.none diet.none
#>             <fctr>          <fctr>        <fctr>       <fctr>    <fctr>
#>    1:            f               f             f            t         t
#>    2:            f               f             f            t         t
#>    3:            f               f             f            t         t
#>    4:            f               f             f            t         t
#>    5:            f               f             f            t         t
#>   ---                                                                  
#> 5750:            f               f             f            t         t
#> 5751:            f               f             f            t         t
#> 5752:            f               f             f            t         t
#> 5753:            f               f             f            t         t
#> 5754:            f               f             f            t         t
#>       diet.vegetarian diet.vegan diet.low-calorie diet.other pets.none
#>                <fctr>     <fctr>           <fctr>     <fctr>    <fctr>
#>    1:               f          f                f       <NA>         t
#>    2:               f          f                f       <NA>         t
#>    3:               f          f                f       <NA>         t
#>    4:               f          f                f       <NA>         t
#>    5:               f          f                f          f         t
#>   ---                                                                 
#> 5750:               f          f                f       <NA>         f
#> 5751:               f          f                f       <NA>         f
#> 5752:               f          f                f       <NA>         f
#> 5753:               f          f                f       <NA>         f
#> 5754:               f          f                f       <NA>         f
#>       pets.dogs pets.cats pets.birds pets.other howhear.radio.tv
#>          <fctr>    <fctr>     <fctr>     <fctr>           <fctr>
#>    1:         f         f          f          f                t
#>    2:         f         f          f          f                t
#>    3:         f         f          f          f                t
#>    4:         f         f          f          f                t
#>    5:         f         f          f          f                f
#>   ---                                                           
#> 5750:         t         f          f          f                t
#> 5751:         t         f          f          f                t
#> 5752:         t         f          f          f                t
#> 5753:         t         f          f          f                t
#> 5754:         t         f          f          f                t
#>       howhear.paper.magazine howhear.internet howhear.poster
#>                       <fctr>           <fctr>         <fctr>
#>    1:                      f                f              f
#>    2:                      f                f              f
#>    3:                      f                f              f
#>    4:                      f                f              f
#>    5:                      f                t              f
#>   ---                                                       
#> 5750:                      f                f              f
#> 5751:                      f                f              f
#> 5752:                      f                f              f
#> 5753:                      f                f              f
#> 5754:                      f                f              f
#>       howhear.family.friends howhear.other       date  birthdate   age agegroup
#>                       <fctr>        <fctr>     <Date>     <Date> <num>   <fctr>
#>    1:                      f          <NA> 2013-11-18 1966-08-01    47    45-64
#>    2:                      f          <NA> 2013-11-25 1966-08-01    47    45-64
#>    3:                      f          <NA> 2013-12-02 1966-08-01    47    45-64
#>    4:                      f          <NA> 2013-12-09 1966-08-01    47    45-64
#>    5:                      f             t 2012-11-12 1973-02-01    39    19-44
#>   ---                                                                          
#> 5750:                      f          <NA> 2013-12-11 1996-04-01    17     0-18
#> 5751:                      f          <NA> 2013-12-18 1996-04-01    17     0-18
#> 5752:                      f          <NA> 2013-12-25 1996-04-01    17     0-18
#> 5753:                      f          <NA> 2014-01-01 1996-04-01    17     0-18
#> 5754:                      f          <NA> 2014-01-09 1996-04-01    17     0-18
#>       living.with.children country settlement.type             region
#>                      <int>  <fctr>           <int>             <fctr>
#>    1:                    1 england               5      west_midlands
#>    2:                    1 england               5      west_midlands
#>    3:                    1 england               5      west_midlands
#>    4:                    1 england               5      west_midlands
#>    5:                    0 england               5 south_west_england
#>   ---                                                                
#> 5750:                    1 england               5    east_of_england
#> 5751:                    1 england               5    east_of_england
#> 5752:                    1 england               5    east_of_england
#> 5753:                    1 england               5    east_of_england
#> 5754:                    1 england               5    east_of_england
#>       urban.rural work.country work.settlement.type        work.region
#>            <fctr>       <fctr>                <int>             <fctr>
#>    1:       urban      england                    5 north_east_england
#>    2:       urban      england                    5 north_east_england
#>    3:       urban      england                    5 north_east_england
#>    4:       urban      england                    5 north_east_england
#>    5:       urban      england                    6 north_east_england
#>   ---                                                                 
#> 5750:       urban         <NA>                   NA               <NA>
#> 5751:       urban         <NA>                   NA               <NA>
#> 5752:       urban         <NA>                   NA               <NA>
#> 5753:       urban         <NA>                   NA               <NA>
#> 5754:       urban         <NA>                   NA               <NA>
#>       work.urban.rural highest.education nb.household nb.household.children
#>                 <fctr>            <fctr>        <num>                 <num>
#>    1:            urban     education.bsc            3                     2
#>    2:            urban     education.bsc            3                     2
#>    3:            urban     education.bsc            3                     2
#>    4:            urban     education.bsc            3                     2
#>    5:            rural    education.gcse            5                     0
#>   ---                                                                      
#> 5750:             <NA> education.stillin            2                     1
#> 5751:             <NA> education.stillin            2                     1
#> 5752:             <NA> education.stillin            2                     1
#> 5753:             <NA> education.stillin            2                     1
#> 5754:             <NA> education.stillin            2                     1
#>       symptom.id no.symptoms  fever chills blocked.runny.nose sneezing
#>           <char>      <fctr> <fctr> <fctr>             <fctr>   <fctr>
#>    1:     2014.2           t      f      f                  f        f
#>    2:     2014.3           t      f      f                  f        f
#>    3:     2014.4           t      f      f                  f        f
#>    4:     2014.5           t      f      f                  f        f
#>    5:     2013.3           t      f      f                  f        f
#>   ---                                                                 
#> 5750:  2014.3337           t      f      f                  f        f
#> 5751:  2014.3338           t      f      f                  f        f
#> 5752:  2014.3339           t      f      f                  f        f
#> 5753:  2014.3340           t      f      f                  f        f
#> 5754:  2014.3341           t      f      f                  f        f
#>       sore.throat  cough shortness.breath headache muscle.and.or.joint.pain
#>            <fctr> <fctr>           <fctr>   <fctr>                   <fctr>
#>    1:           f      f                f        f                        f
#>    2:           f      f                f        f                        f
#>    3:           f      f                f        f                        f
#>    4:           f      f                f        f                        f
#>    5:           f      f                f        f                        f
#>   ---                                                                      
#> 5750:           f      f                f        f                        f
#> 5751:           f      f                f        f                        f
#> 5752:           f      f                f        f                        f
#> 5753:           f      f                f        f                        f
#> 5754:           f      f                f        f                        f
#>       chest.pain  tired loss.appetite phlegm watery.eyes nausea vomiting
#>           <fctr> <fctr>        <fctr> <fctr>      <fctr> <fctr>   <fctr>
#>    1:          f      f             f      f           f      f        f
#>    2:          f      f             f      f           f      f        f
#>    3:          f      f             f      f           f      f        f
#>    4:          f      f             f      f           f      f        f
#>    5:          f      f             f      f           f      f        f
#>   ---                                                                   
#> 5750:          f      f             f      f           f      f        f
#> 5751:          f      f             f      f           f      f        f
#> 5752:          f      f             f      f           f      f        f
#> 5753:          f      f             f      f           f      f        f
#> 5754:          f      f             f      f           f      f        f
#>       diarrhoea stomach.ache other.symptoms   same symptoms.start.option
#>          <fctr>       <fctr>         <fctr> <fctr>                <fctr>
#>    1:         f            f              f   <NA>                  <NA>
#>    2:         f            f              f   <NA>                  <NA>
#>    3:         f            f              f   <NA>                  <NA>
#>    4:         f            f              f   <NA>                  <NA>
#>    5:         f            f              f   <NA>                  <NA>
#>   ---                                                                   
#> 5750:         f            f              f   <NA>                  <NA>
#> 5751:         f            f              f   <NA>                  <NA>
#> 5752:         f            f              f   <NA>                  <NA>
#> 5753:         f            f              f   <NA>                  <NA>
#> 5754:         f            f              f   <NA>                  <NA>
#>       symptoms.start.date symptoms.end.option symptoms.end.date
#>                    <Date>              <fctr>            <Date>
#>    1:                <NA>                <NA>              <NA>
#>    2:                <NA>                <NA>              <NA>
#>    3:                <NA>                <NA>              <NA>
#>    4:                <NA>                <NA>              <NA>
#>    5:                <NA>                <NA>              <NA>
#>   ---                                                          
#> 5750:                <NA>                <NA>              <NA>
#> 5751:                <NA>                <NA>              <NA>
#> 5752:                <NA>                <NA>              <NA>
#> 5753:                <NA>                <NA>              <NA>
#> 5754:                <NA>                <NA>              <NA>
#>       symptoms.suddenly fever.start.option fever.start.date fever.suddenly
#>                  <fctr>             <fctr>           <Date>         <fctr>
#>    1:              <NA>               <NA>             <NA>           <NA>
#>    2:              <NA>               <NA>             <NA>           <NA>
#>    3:              <NA>               <NA>             <NA>           <NA>
#>    4:              <NA>               <NA>             <NA>           <NA>
#>    5:              <NA>               <NA>             <NA>           <NA>
#>   ---                                                                     
#> 5750:              <NA>               <NA>             <NA>           <NA>
#> 5751:              <NA>               <NA>             <NA>           <NA>
#> 5752:              <NA>               <NA>             <NA>           <NA>
#> 5753:              <NA>               <NA>             <NA>           <NA>
#> 5754:              <NA>               <NA>             <NA>           <NA>
#>       fever.temperature fever.temperature.value visit.medical.service.no
#>                  <fctr>                  <fctr>                   <fctr>
#>    1:              <NA>                    <NA>                     <NA>
#>    2:              <NA>                    <NA>                     <NA>
#>    3:              <NA>                    <NA>                     <NA>
#>    4:              <NA>                    <NA>                     <NA>
#>    5:              <NA>                    <NA>                     <NA>
#>   ---                                                                   
#> 5750:              <NA>                    <NA>                     <NA>
#> 5751:              <NA>                    <NA>                     <NA>
#> 5752:              <NA>                    <NA>                     <NA>
#> 5753:              <NA>                    <NA>                     <NA>
#> 5754:              <NA>                    <NA>                     <NA>
#>       visit.medical.service.gp visit.medical.service.ae
#>                         <fctr>                   <fctr>
#>    1:                     <NA>                     <NA>
#>    2:                     <NA>                     <NA>
#>    3:                     <NA>                     <NA>
#>    4:                     <NA>                     <NA>
#>    5:                     <NA>                     <NA>
#>   ---                                                  
#> 5750:                     <NA>                     <NA>
#> 5751:                     <NA>                     <NA>
#> 5752:                     <NA>                     <NA>
#> 5753:                     <NA>                     <NA>
#> 5754:                     <NA>                     <NA>
#>       visit.medical.service.hospital visit.medical.service.other
#>                               <fctr>                      <fctr>
#>    1:                           <NA>                        <NA>
#>    2:                           <NA>                        <NA>
#>    3:                           <NA>                        <NA>
#>    4:                           <NA>                        <NA>
#>    5:                           <NA>                        <NA>
#>   ---                                                           
#> 5750:                           <NA>                        <NA>
#> 5751:                           <NA>                        <NA>
#> 5752:                           <NA>                        <NA>
#> 5753:                           <NA>                        <NA>
#> 5754:                           <NA>                        <NA>
#>       visit.medical.service.appointment
#>                                  <fctr>
#>    1:                              <NA>
#>    2:                              <NA>
#>    3:                              <NA>
#>    4:                              <NA>
#>    5:                              <NA>
#>   ---                                  
#> 5750:                              <NA>
#> 5751:                              <NA>
#> 5752:                              <NA>
#> 5753:                              <NA>
#> 5754:                              <NA>
#>       visit.medical.service.howsoon.gp.receptionist
#>                                              <fctr>
#>    1:                                          <NA>
#>    2:                                          <NA>
#>    3:                                          <NA>
#>    4:                                          <NA>
#>    5:                                          <NA>
#>   ---                                              
#> 5750:                                          <NA>
#> 5751:                                          <NA>
#> 5752:                                          <NA>
#> 5753:                                          <NA>
#> 5754:                                          <NA>
#>       visit.medical.service.howsoon.gp.doctor.nurse
#>                                              <fctr>
#>    1:                                          <NA>
#>    2:                                          <NA>
#>    3:                                          <NA>
#>    4:                                          <NA>
#>    5:                                          <NA>
#>   ---                                              
#> 5750:                                          <NA>
#> 5751:                                          <NA>
#> 5752:                                          <NA>
#> 5753:                                          <NA>
#> 5754:                                          <NA>
#>       visit.medical.service.howsoon.nhs visit.medical.service.howsoon.other
#>                                  <fctr>                              <fctr>
#>    1:                              <NA>                                <NA>
#>    2:                              <NA>                                <NA>
#>    3:                              <NA>                                <NA>
#>    4:                              <NA>                                <NA>
#>    5:                              <NA>                                <NA>
#>   ---                                                                      
#> 5750:                              <NA>                                <NA>
#> 5751:                              <NA>                                <NA>
#> 5752:                              <NA>                                <NA>
#> 5753:                              <NA>                                <NA>
#> 5754:                              <NA>                                <NA>
#>       contact.medical.service.no contact.medical.service.gp.receptionist
#>                           <fctr>                                  <fctr>
#>    1:                       <NA>                                    <NA>
#>    2:                       <NA>                                    <NA>
#>    3:                       <NA>                                    <NA>
#>    4:                       <NA>                                    <NA>
#>    5:                       <NA>                                    <NA>
#>   ---                                                                   
#> 5750:                       <NA>                                    <NA>
#> 5751:                       <NA>                                    <NA>
#> 5752:                       <NA>                                    <NA>
#> 5753:                       <NA>                                    <NA>
#> 5754:                       <NA>                                    <NA>
#>       contact.medical.service.gp.doctor contact.medical.service.nhs
#>                                  <fctr>                      <fctr>
#>    1:                              <NA>                        <NA>
#>    2:                              <NA>                        <NA>
#>    3:                              <NA>                        <NA>
#>    4:                              <NA>                        <NA>
#>    5:                              <NA>                        <NA>
#>   ---                                                              
#> 5750:                              <NA>                        <NA>
#> 5751:                              <NA>                        <NA>
#> 5752:                              <NA>                        <NA>
#> 5753:                              <NA>                        <NA>
#> 5754:                              <NA>                        <NA>
#>       contact.medical.service.npfs contact.medical.service.other
#>                             <fctr>                        <fctr>
#>    1:                         <NA>                          <NA>
#>    2:                         <NA>                          <NA>
#>    3:                         <NA>                          <NA>
#>    4:                         <NA>                          <NA>
#>    5:                         <NA>                          <NA>
#>   ---                                                           
#> 5750:                         <NA>                          <NA>
#> 5751:                         <NA>                          <NA>
#> 5752:                         <NA>                          <NA>
#> 5753:                         <NA>                          <NA>
#> 5754:                         <NA>                          <NA>
#>       contact.medical.service.howsoon.gp.receptionist
#>                                                <fctr>
#>    1:                                            <NA>
#>    2:                                            <NA>
#>    3:                                            <NA>
#>    4:                                            <NA>
#>    5:                                            <NA>
#>   ---                                                
#> 5750:                                            <NA>
#> 5751:                                            <NA>
#> 5752:                                            <NA>
#> 5753:                                            <NA>
#> 5754:                                            <NA>
#>       contact.medical.service.howsoon.gp.doctor.nurse
#>                                                <fctr>
#>    1:                                            <NA>
#>    2:                                            <NA>
#>    3:                                            <NA>
#>    4:                                            <NA>
#>    5:                                            <NA>
#>   ---                                                
#> 5750:                                            <NA>
#> 5751:                                            <NA>
#> 5752:                                            <NA>
#> 5753:                                            <NA>
#> 5754:                                            <NA>
#>       contact.medical.service.howsoon.nhs contact.medical.service.howsoon.other
#>                                    <fctr>                                <fctr>
#>    1:                                <NA>                                  <NA>
#>    2:                                <NA>                                  <NA>
#>    3:                                <NA>                                  <NA>
#>    4:                                <NA>                                  <NA>
#>    5:                                <NA>                                  <NA>
#>   ---                                                                          
#> 5750:                                <NA>                                  <NA>
#> 5751:                                <NA>                                  <NA>
#> 5752:                                <NA>                                  <NA>
#> 5753:                                <NA>                                  <NA>
#> 5754:                                <NA>                                  <NA>
#>       no.medication medication.painkillers medication.cough
#>              <fctr>                 <fctr>           <fctr>
#>    1:          <NA>                   <NA>             <NA>
#>    2:          <NA>                   <NA>             <NA>
#>    3:          <NA>                   <NA>             <NA>
#>    4:          <NA>                   <NA>             <NA>
#>    5:          <NA>                   <NA>             <NA>
#>   ---                                                      
#> 5750:          <NA>                   <NA>             <NA>
#> 5751:          <NA>                   <NA>             <NA>
#> 5752:          <NA>                   <NA>             <NA>
#> 5753:          <NA>                   <NA>             <NA>
#> 5754:          <NA>                   <NA>             <NA>
#>       medication.antiviral medication.antibiotic medication.other
#>                     <fctr>                <fctr>           <fctr>
#>    1:                 <NA>                  <NA>             <NA>
#>    2:                 <NA>                  <NA>             <NA>
#>    3:                 <NA>                  <NA>             <NA>
#>    4:                 <NA>                  <NA>             <NA>
#>    5:                 <NA>                  <NA>             <NA>
#>   ---                                                            
#> 5750:                 <NA>                  <NA>             <NA>
#> 5751:                 <NA>                  <NA>             <NA>
#> 5752:                 <NA>                  <NA>             <NA>
#> 5753:                 <NA>                  <NA>             <NA>
#> 5754:                 <NA>                  <NA>             <NA>
#>       medication.dontknow medication.howsoon alter.routine still.altered
#>                    <fctr>              <int>        <fctr>        <fctr>
#>    1:                <NA>                 NA          <NA>          <NA>
#>    2:                <NA>                 NA          <NA>          <NA>
#>    3:                <NA>                 NA          <NA>          <NA>
#>    4:                <NA>                 NA          <NA>          <NA>
#>    5:                <NA>                 NA          <NA>          <NA>
#>   ---                                                                   
#> 5750:                <NA>                 NA          <NA>          <NA>
#> 5751:                <NA>                 NA          <NA>          <NA>
#> 5752:                <NA>                 NA          <NA>          <NA>
#> 5753:                <NA>                 NA          <NA>          <NA>
#> 5754:                <NA>                 NA          <NA>          <NA>
#>       howlong.altered what.do.you.think health.score        onset suddenly
#>                <fctr>            <fctr>        <int>       <char>   <fctr>
#>    1:            <NA>              <NA>           87 not.answered     <NA>
#>    2:            <NA>              <NA>           88 not.answered     <NA>
#>    3:            <NA>              <NA>           90 not.answered     <NA>
#>    4:            <NA>              <NA>           86 not.answered     <NA>
#>    5:            <NA>              <NA>           90 not.answered     <NA>
#>   ---                                                                     
#> 5750:            <NA>              <NA>           80 not.answered     <NA>
#> 5751:            <NA>              <NA>           81 not.answered     <NA>
#> 5752:            <NA>              <NA>           85 not.answered     <NA>
#> 5753:            <NA>              <NA>           76 not.answered     <NA>
#> 5754:            <NA>              <NA>           80 not.answered     <NA>
#>       fever.symptoms systemic.symptoms resp.symptoms general.symptoms
#>               <fctr>            <fctr>        <fctr>           <fctr>
#>    1:              f                 f             f                f
#>    2:              f                 f             f                f
#>    3:              f                 f             f                f
#>    4:              f                 f             f                f
#>    5:              f                 f             f                f
#>   ---                                                                
#> 5750:              f                 f             f                f
#> 5751:              f                 f             f                f
#> 5752:              f                 f             f                f
#> 5753:              f                 f             f                f
#> 5754:              f                 f             f                f
#>       ili.symptoms    ili ili.fever ili.self     gi nReports   min.date
#>             <fctr> <fctr>    <fctr>   <fctr> <fctr>    <int>     <Date>
#>    1:            f      f         f        f      f        5 2013-11-11
#>    2:            f      f         f        f      f        5 2013-11-11
#>    3:            f      f         f        f      f        5 2013-11-11
#>    4:            f      f         f        f      f        5 2013-11-11
#>    5:            f      f         f        f      f       16 2012-11-05
#>   ---                                                                  
#> 5750:            f      f         f        f      f        9 2013-11-12
#> 5751:            f      f         f        f      f        9 2013-11-12
#> 5752:            f      f         f        f      f        9 2013-11-12
#> 5753:            f      f         f        f      f        9 2013-11-12
#> 5754:            f      f         f        f      f        9 2013-11-12
#>         max.date day.of.week month  season education
#>           <Date>       <num> <int>  <char>     <int>
#>    1: 2013-12-09           1    11 2013/14         3
#>    2: 2013-12-09           1    11 2013/14         3
#>    3: 2013-12-09           1    12 2013/14         3
#>    4: 2013-12-09           1    12 2013/14         3
#>    5: 2013-03-30           1    11 2012/13        NA
#>   ---                                               
#> 5750: 2014-01-09           3    12 2013/14         5
#> 5751: 2014-01-09           3    12 2013/14         5
#> 5752: 2014-01-09           3    12 2013/14         5
#> 5753: 2014-01-09           3     1 2013/14         5
#> 5754: 2014-01-09           4     1 2013/14         5
#>       why.vaccine.given.at.school howhear.school.work howhear.bsa howhear.who
#>                            <fctr>              <fctr>      <fctr>       <int>
#>    1:                        <NA>                   f           f           2
#>    2:                        <NA>                   f           f           2
#>    3:                        <NA>                   f           f           2
#>    4:                        <NA>                   f           f           2
#>    5:                        <NA>                <NA>        <NA>          NA
#>   ---                                                                        
#> 5750:                        <NA>                   f           f           2
#> 5751:                        <NA>                   f           f           2
#> 5752:                        <NA>                   f           f           2
#> 5753:                        <NA>                   f           f           2
#> 5754:                        <NA>                   f           f           2
#>       activity.vigorous activity.moderate activity.winter participant_id
#>                   <int>             <int>           <int>          <int>
#>    1:                 0                 1               1            321
#>    2:                 0                 1               1            321
#>    3:                 0                 1               1            321
#>    4:                 0                 1               1            321
#>    5:                NA                NA              NA             15
#>   ---                                                                   
#> 5750:                 3                 3               3            327
#> 5751:                 3                 3               3            327
#> 5752:                 3                 3               3            327
#> 5753:                 3                 3               3            327
#> 5754:                 3                 3               3            327
```

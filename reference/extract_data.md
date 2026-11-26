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
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use contact_illness().
#> Warning: The contact and symptom surveys are joined by report date, which pairs each symptom report with the latest earlier contact report however long before it was filed, and (with 'unsuccessful.join') drops symptom reports that have none. To link contacts to illness, extract them separately and use contact_illness().
table(dt$season)
#> 
#> 2012/13 2013/14 
#>    2403    2424 

## without the contact questionnaire
extract_data(simulated_data, years = 2013:2014,
             surveys = c("background", "symptom"))
#> Key: <global_id>
#>       global_id background.id             self gender birthmonth
#>          <char>        <char>           <fctr> <fctr>     <char>
#>    1:   sim0001        2014.1             self   male    1966-08
#>    2:   sim0001        2014.1             self   male    1966-08
#>    3:   sim0001        2014.1             self   male    1966-08
#>    4:   sim0001        2014.1             self   male    1966-08
#>    5:   sim0001        2014.1             self   male    1966-08
#>   ---                                                           
#> 4846:   sim0400      2014.310 household_member   male    1992-06
#> 4847:   sim0400      2014.310 household_member   male    1992-06
#> 4848:   sim0400      2014.310 household_member   male    1992-06
#> 4849:   sim0400      2014.310 household_member   male    1992-06
#> 4850:   sim0400      2014.310 household_member   male    1992-06
#>                   main.activity work.postcode.option     occupation
#>                          <fctr>               <fctr>         <fctr>
#>    1: paid_employment_full_time                  yes   professional
#>    2: paid_employment_full_time                  yes   professional
#>    3: paid_employment_full_time                  yes   professional
#>    4: paid_employment_full_time                  yes   professional
#>    5: paid_employment_full_time                  yes   professional
#>   ---                                                              
#> 4846: paid_employment_full_time                  yes skilled_manual
#> 4847: paid_employment_full_time                  yes skilled_manual
#> 4848: paid_employment_full_time                  yes skilled_manual
#> 4849: paid_employment_full_time                  yes skilled_manual
#> 4850: paid_employment_full_time                  yes skilled_manual
#>       no.education education.gcse education.alevels education.bsc education.msc
#>             <fctr>         <fctr>            <fctr>        <fctr>        <fctr>
#>    1:         <NA>           <NA>              <NA>          <NA>          <NA>
#>    2:         <NA>           <NA>              <NA>          <NA>          <NA>
#>    3:         <NA>           <NA>              <NA>          <NA>          <NA>
#>    4:         <NA>           <NA>              <NA>          <NA>          <NA>
#>    5:         <NA>           <NA>              <NA>          <NA>          <NA>
#>   ---                                                                          
#> 4846:         <NA>           <NA>              <NA>          <NA>          <NA>
#> 4847:         <NA>           <NA>              <NA>          <NA>          <NA>
#> 4848:         <NA>           <NA>              <NA>          <NA>          <NA>
#> 4849:         <NA>           <NA>              <NA>          <NA>          <NA>
#> 4850:         <NA>           <NA>              <NA>          <NA>          <NA>
#>       education.stillin frequent.contact.children frequent.contact.elderly
#>                  <fctr>                    <fctr>                   <fctr>
#>    1:              <NA>                         f                        f
#>    2:              <NA>                         f                        f
#>    3:              <NA>                         f                        f
#>    4:              <NA>                         f                        f
#>    5:              <NA>                         f                        f
#>   ---                                                                     
#> 4846:              <NA>                         f                        f
#> 4847:              <NA>                         f                        f
#> 4848:              <NA>                         f                        f
#> 4849:              <NA>                         f                        f
#> 4850:              <NA>                         f                        f
#>       frequent.contact.patients frequent.contact.people frequent.contact.none
#>                          <fctr>                  <fctr>                <fctr>
#>    1:                         f                       t                     f
#>    2:                         f                       t                     f
#>    3:                         f                       t                     f
#>    4:                         f                       t                     f
#>    5:                         f                       t                     f
#>   ---                                                                        
#> 4846:                         t                       t                     f
#> 4847:                         t                       t                     f
#> 4848:                         t                       t                     f
#> 4849:                         t                       t                     f
#> 4850:                         t                       t                     f
#>       household.0.4 nb.household.0.4 household.5.18 nb.household.5.18
#>              <fctr>            <int>         <fctr>             <int>
#>    1:             t                2              f                 0
#>    2:             t                2              f                 0
#>    3:             t                2              f                 0
#>    4:             t                2              f                 0
#>    5:             t                2              f                 0
#>   ---                                                                
#> 4846:             f                0              t                 1
#> 4847:             f                0              t                 1
#> 4848:             f                0              t                 1
#> 4849:             f                0              t                 1
#> 4850:             f                0              t                 1
#>       household.19.44 nb.household.19.44 household.45.64 nb.household.45.64
#>                <fctr>              <int>          <fctr>              <int>
#>    1:               f                  0               t                  1
#>    2:               f                  0               t                  1
#>    3:               f                  0               t                  1
#>    4:               f                  0               t                  1
#>    5:               f                  0               t                  1
#>   ---                                                                      
#> 4846:               f                  0               t                  1
#> 4847:               f                  0               t                  1
#> 4848:               f                  0               t                  1
#> 4849:               f                  0               t                  1
#> 4850:               f                  0               t                  1
#>       household.65+ nb.household.65+ children.school transport
#>              <fctr>            <int>          <fctr>    <fctr>
#>    1:             f                0               4       car
#>    2:             f                0               4       car
#>    3:             f                0               4       car
#>    4:             f                0               4       car
#>    5:             f                0               4       car
#>   ---                                                         
#> 4846:             f                0            none       car
#> 4847:             f                0            none       car
#> 4848:             f                0            none       car
#> 4849:             f                0            none       car
#> 4850:             f                0            none       car
#>       howlong.transport howoften.flulike vaccine.last.year vaccine.this.year
#>                  <fctr>           <fctr>            <fctr>            <fctr>
#>    1:              none   (2,5]_annually                no                no
#>    2:              none   (2,5]_annually                no                no
#>    3:              none   (2,5]_annually                no                no
#>    4:              none   (2,5]_annually                no                no
#>    5:              none   (2,5]_annually                no                no
#>   ---                                                                       
#> 4846:              none   (0,2]_annually                no                no
#> 4847:              none   (0,2]_annually                no                no
#> 4848:              none   (0,2]_annually                no                no
#> 4849:              none   (0,2]_annually                no                no
#> 4850:              none   (0,2]_annually                no                no
#>       date.vaccine.option date.vaccine why.vaccine.riskgroup
#>                    <fctr>       <char>                <fctr>
#>    1:                <NA>                               <NA>
#>    2:                <NA>                               <NA>
#>    3:                <NA>                               <NA>
#>    4:                <NA>                               <NA>
#>    5:                <NA>                               <NA>
#>   ---                                                       
#> 4846:                <NA>                               <NA>
#> 4847:                <NA>                               <NA>
#> 4848:                <NA>                               <NA>
#> 4849:                <NA>                               <NA>
#> 4850:                <NA>                               <NA>
#>       why.vaccine.protected why.vaccine.protect.others why.vaccine.doctor
#>                      <fctr>                     <fctr>             <fctr>
#>    1:                  <NA>                       <NA>               <NA>
#>    2:                  <NA>                       <NA>               <NA>
#>    3:                  <NA>                       <NA>               <NA>
#>    4:                  <NA>                       <NA>               <NA>
#>    5:                  <NA>                       <NA>               <NA>
#>   ---                                                                    
#> 4846:                  <NA>                       <NA>               <NA>
#> 4847:                  <NA>                       <NA>               <NA>
#> 4848:                  <NA>                       <NA>               <NA>
#> 4849:                  <NA>                       <NA>               <NA>
#> 4850:                  <NA>                       <NA>               <NA>
#>       why.vaccine.work.recommended why.vaccine.convenient why.vaccine.free
#>                             <fctr>                 <fctr>           <fctr>
#>    1:                         <NA>                   <NA>             <NA>
#>    2:                         <NA>                   <NA>             <NA>
#>    3:                         <NA>                   <NA>             <NA>
#>    4:                         <NA>                   <NA>             <NA>
#>    5:                         <NA>                   <NA>             <NA>
#>   ---                                                                     
#> 4846:                         <NA>                   <NA>             <NA>
#> 4847:                         <NA>                   <NA>             <NA>
#> 4848:                         <NA>                   <NA>             <NA>
#> 4849:                         <NA>                   <NA>             <NA>
#> 4850:                         <NA>                   <NA>             <NA>
#>       why.vaccine.nomiss.work why.vaccine.always why.vaccine.other
#>                        <fctr>             <fctr>            <fctr>
#>    1:                    <NA>               <NA>              <NA>
#>    2:                    <NA>               <NA>              <NA>
#>    3:                    <NA>               <NA>              <NA>
#>    4:                    <NA>               <NA>              <NA>
#>    5:                    <NA>               <NA>              <NA>
#>   ---                                                             
#> 4846:                    <NA>               <NA>              <NA>
#> 4847:                    <NA>               <NA>              <NA>
#> 4848:                    <NA>               <NA>              <NA>
#> 4849:                    <NA>               <NA>              <NA>
#> 4850:                    <NA>               <NA>              <NA>
#>       why.not.vaccine.notyet why.not.vaccine.notoffered why.not.vaccine.norisk
#>                       <fctr>                     <fctr>                 <fctr>
#>    1:                      f                          f                      f
#>    2:                      f                          f                      f
#>    3:                      f                          f                      f
#>    4:                      f                          f                      f
#>    5:                      f                          f                      f
#>   ---                                                                         
#> 4846:                      f                          f                      f
#> 4847:                      f                          f                      f
#> 4848:                      f                          f                      f
#> 4849:                      f                          f                      f
#> 4850:                      f                          f                      f
#>       why.not.vaccine.natural why.not.vaccine.noteffective
#>                        <fctr>                       <fctr>
#>    1:                       f                            f
#>    2:                       f                            f
#>    3:                       f                            f
#>    4:                       f                            f
#>    5:                       f                            f
#>   ---                                                     
#> 4846:                       f                            f
#> 4847:                       f                            f
#> 4848:                       f                            f
#> 4849:                       f                            f
#> 4850:                       f                            f
#>       why.not.vaccine.minor why.not.vaccine.unlikely why.not.vaccine.cause
#>                      <fctr>                   <fctr>                <fctr>
#>    1:                     f                        f                     f
#>    2:                     f                        f                     f
#>    3:                     f                        f                     f
#>    4:                     f                        f                     f
#>    5:                     f                        f                     f
#>   ---                                                                     
#> 4846:                     f                        t                     f
#> 4847:                     f                        t                     f
#> 4848:                     f                        t                     f
#> 4849:                     f                        t                     f
#> 4850:                     f                        t                     f
#>       why.not.vaccine.side.effects why.not.vaccine.dont.like
#>                             <fctr>                    <fctr>
#>    1:                            f                         f
#>    2:                            f                         f
#>    3:                            f                         f
#>    4:                            f                         f
#>    5:                            f                         f
#>   ---                                                       
#> 4846:                            f                         f
#> 4847:                            f                         f
#> 4848:                            f                         f
#> 4849:                            f                         f
#> 4850:                            f                         f
#>       why.not.vaccine.unavailable why.not.vaccine.not.free
#>                            <fctr>                   <fctr>
#>    1:                           f                        f
#>    2:                           f                        f
#>    3:                           f                        f
#>    4:                           f                        f
#>    5:                           f                        f
#>   ---                                                     
#> 4846:                           f                        f
#> 4847:                           f                        f
#> 4848:                           f                        f
#> 4849:                           f                        f
#> 4850:                           f                        f
#>       why.not.vaccine.no.reason why.not.vaccine.doctor why.not.vaccine.other
#>                          <fctr>                 <fctr>                <fctr>
#>    1:                         t                      f                     f
#>    2:                         t                      f                     f
#>    3:                         t                      f                     f
#>    4:                         t                      f                     f
#>    5:                         t                      f                     f
#>   ---                                                                       
#> 4846:                         f                      f                     f
#> 4847:                         f                      f                     f
#> 4848:                         f                      f                     f
#> 4849:                         f                      f                     f
#> 4850:                         f                      f                     f
#>       risk.none risk.asthma risk.diabetes risk.lung risk.heart risk.kidney
#>          <fctr>      <fctr>        <fctr>    <fctr>     <fctr>      <fctr>
#>    1:         t           f             f         f          f           f
#>    2:         t           f             f         f          f           f
#>    3:         t           f             f         f          f           f
#>    4:         t           f             f         f          f           f
#>    5:         t           f             f         f          f           f
#>   ---                                                                     
#> 4846:         t           f             f         f          f           f
#> 4847:         t           f             f         f          f           f
#> 4848:         t           f             f         f          f           f
#> 4849:         t           f             f         f          f           f
#> 4850:         t           f             f         f          f           f
#>       risk.immune pregnant pregnant.trimester  smoke allergy.hayfever
#>            <fctr>   <fctr>              <int> <fctr>           <fctr>
#>    1:           f     <NA>                 NA     no                f
#>    2:           f     <NA>                 NA     no                f
#>    3:           f     <NA>                 NA     no                f
#>    4:           f     <NA>                 NA     no                f
#>    5:           f     <NA>                 NA     no                f
#>   ---                                                                
#> 4846:           f     <NA>                 NA     no                f
#> 4847:           f     <NA>                 NA     no                f
#> 4848:           f     <NA>                 NA     no                f
#> 4849:           f     <NA>                 NA     no                f
#> 4850:           f     <NA>                 NA     no                f
#>       allergy.dust allergy.animals allergy.other allergy.none diet.none
#>             <fctr>          <fctr>        <fctr>       <fctr>    <fctr>
#>    1:            f               f             f            t         t
#>    2:            f               f             f            t         t
#>    3:            f               f             f            t         t
#>    4:            f               f             f            t         t
#>    5:            f               f             f            t         t
#>   ---                                                                  
#> 4846:            f               f             f            t         t
#> 4847:            f               f             f            t         t
#> 4848:            f               f             f            t         t
#> 4849:            f               f             f            t         t
#> 4850:            f               f             f            t         t
#>       diet.vegetarian diet.vegan diet.low-calorie diet.other pets.none
#>                <fctr>     <fctr>           <fctr>     <fctr>    <fctr>
#>    1:               f          f                f       <NA>         t
#>    2:               f          f                f       <NA>         t
#>    3:               f          f                f       <NA>         t
#>    4:               f          f                f       <NA>         t
#>    5:               f          f                f       <NA>         t
#>   ---                                                                 
#> 4846:               f          f                f       <NA>         f
#> 4847:               f          f                f       <NA>         f
#> 4848:               f          f                f       <NA>         f
#> 4849:               f          f                f       <NA>         f
#> 4850:               f          f                f       <NA>         f
#>       pets.dogs pets.cats pets.birds pets.other howhear.radio.tv
#>          <fctr>    <fctr>     <fctr>     <fctr>           <fctr>
#>    1:         f         f          f          f                f
#>    2:         f         f          f          f                f
#>    3:         f         f          f          f                f
#>    4:         f         f          f          f                f
#>    5:         f         f          f          f                f
#>   ---                                                           
#> 4846:         t         f          f          f                f
#> 4847:         t         f          f          f                f
#> 4848:         t         f          f          f                f
#> 4849:         t         f          f          f                f
#> 4850:         t         f          f          f                f
#>       howhear.paper.magazine howhear.internet howhear.poster
#>                       <fctr>           <fctr>         <fctr>
#>    1:                      f                f              f
#>    2:                      f                f              f
#>    3:                      f                f              f
#>    4:                      f                f              f
#>    5:                      f                f              f
#>   ---                                                       
#> 4846:                      f                f              f
#> 4847:                      f                f              f
#> 4848:                      f                f              f
#> 4849:                      f                f              f
#> 4850:                      f                f              f
#>       howhear.family.friends howhear.other       date  birthdate   age agegroup
#>                       <fctr>        <fctr>     <Date>     <Date> <num>   <fctr>
#>    1:                      t          <NA> 2013-12-01 1966-08-01    47    45-64
#>    2:                      t          <NA> 2013-12-04 1966-08-01    47    45-64
#>    3:                      t          <NA> 2013-12-11 1966-08-01    47    45-64
#>    4:                      t          <NA> 2013-12-25 1966-08-01    47    45-64
#>    5:                      t          <NA> 2013-12-31 1966-08-01    47    45-64
#>   ---                                                                          
#> 4846:                      t          <NA> 2014-02-13 1992-06-01    21    19-44
#> 4847:                      t          <NA> 2014-02-20 1992-06-01    21    19-44
#> 4848:                      t          <NA> 2014-02-27 1992-06-01    21    19-44
#> 4849:                      t          <NA> 2014-03-16 1992-06-01    21    19-44
#> 4850:                      t          <NA> 2014-03-23 1992-06-01    21    19-44
#>       living.with.children country settlement.type             region
#>                      <int>  <fctr>           <int>             <fctr>
#>    1:                    1 england               5      west_midlands
#>    2:                    1 england               5      west_midlands
#>    3:                    1 england               5      west_midlands
#>    4:                    1 england               5      west_midlands
#>    5:                    1 england               5      west_midlands
#>   ---                                                                
#> 4846:                    1 england               6 south_east_england
#> 4847:                    1 england               6 south_east_england
#> 4848:                    1 england               6 south_east_england
#> 4849:                    1 england               6 south_east_england
#> 4850:                    1 england               6 south_east_england
#>       urban.rural work.country work.settlement.type        work.region
#>            <fctr>       <fctr>                <int>             <fctr>
#>    1:       urban      england                    5 north_east_england
#>    2:       urban      england                    5 north_east_england
#>    3:       urban      england                    5 north_east_england
#>    4:       urban      england                    5 north_east_england
#>    5:       urban      england                    5 north_east_england
#>   ---                                                                 
#> 4846:       rural      england                    5 south_west_england
#> 4847:       rural      england                    5 south_west_england
#> 4848:       rural      england                    5 south_west_england
#> 4849:       rural      england                    5 south_west_england
#> 4850:       rural      england                    5 south_west_england
#>       work.urban.rural highest.education nb.household nb.household.children
#>                 <fctr>            <fctr>        <num>                 <num>
#>    1:            urban     education.bsc            3                     2
#>    2:            urban     education.bsc            3                     2
#>    3:            urban     education.bsc            3                     2
#>    4:            urban     education.bsc            3                     2
#>    5:            urban     education.bsc            3                     2
#>   ---                                                                      
#> 4846:            urban      no.education            2                     1
#> 4847:            urban      no.education            2                     1
#> 4848:            urban      no.education            2                     1
#> 4849:            urban      no.education            2                     1
#> 4850:            urban      no.education            2                     1
#>       symptom.id no.symptoms  fever chills blocked.runny.nose sneezing
#>           <char>      <fctr> <fctr> <fctr>             <fctr>   <fctr>
#>    1:     2014.2           t      f      f                  f        f
#>    2:     2014.3           t      f      f                  f        f
#>    3:     2014.4           t      f      f                  f        f
#>    4:     2014.5           t      f      f                  f        f
#>    5:     2014.6           t      f      f                  f        f
#>   ---                                                                 
#> 4846:  2014.2892           t      f      f                  f        f
#> 4847:  2014.2893           t      f      f                  f        f
#> 4848:  2014.2894           t      f      f                  f        f
#> 4849:  2014.2895           t      f      f                  f        f
#> 4850:  2014.2896           t      f      f                  f        f
#>       sore.throat  cough shortness.breath headache muscle.and.or.joint.pain
#>            <fctr> <fctr>           <fctr>   <fctr>                   <fctr>
#>    1:           f      f                f        f                        f
#>    2:           f      f                f        f                        f
#>    3:           f      f                f        f                        f
#>    4:           f      f                f        f                        f
#>    5:           f      f                f        f                        f
#>   ---                                                                      
#> 4846:           f      f                f        f                        f
#> 4847:           f      f                f        f                        f
#> 4848:           f      f                f        f                        f
#> 4849:           f      f                f        f                        f
#> 4850:           f      f                f        f                        f
#>       chest.pain  tired loss.appetite phlegm watery.eyes nausea vomiting
#>           <fctr> <fctr>        <fctr> <fctr>      <fctr> <fctr>   <fctr>
#>    1:          f      f             f      f           f      f        f
#>    2:          f      f             f      f           f      f        f
#>    3:          f      f             f      f           f      f        f
#>    4:          f      f             f      f           f      f        f
#>    5:          f      f             f      f           f      f        f
#>   ---                                                                   
#> 4846:          f      f             f      f           f      f        f
#> 4847:          f      f             f      f           f      f        f
#> 4848:          f      f             f      f           f      f        f
#> 4849:          f      f             f      f           f      f        f
#> 4850:          f      f             f      f           f      f        f
#>       diarrhoea stomach.ache other.symptoms   same symptoms.start.option
#>          <fctr>       <fctr>         <fctr> <fctr>                <fctr>
#>    1:         f            f              f   <NA>                  <NA>
#>    2:         f            f              f   <NA>                  <NA>
#>    3:         f            f              f   <NA>                  <NA>
#>    4:         f            f              f   <NA>                  <NA>
#>    5:         f            f              f   <NA>                  <NA>
#>   ---                                                                   
#> 4846:         f            f              f   <NA>                  <NA>
#> 4847:         f            f              f   <NA>                  <NA>
#> 4848:         f            f              f   <NA>                  <NA>
#> 4849:         f            f              f   <NA>                  <NA>
#> 4850:         f            f              f   <NA>                  <NA>
#>       symptoms.start.date symptoms.end.option symptoms.end.date
#>                    <Date>              <fctr>            <Date>
#>    1:                <NA>                <NA>              <NA>
#>    2:                <NA>                <NA>              <NA>
#>    3:                <NA>                <NA>              <NA>
#>    4:                <NA>                <NA>              <NA>
#>    5:                <NA>                <NA>              <NA>
#>   ---                                                          
#> 4846:                <NA>                <NA>              <NA>
#> 4847:                <NA>                <NA>              <NA>
#> 4848:                <NA>                <NA>              <NA>
#> 4849:                <NA>                <NA>              <NA>
#> 4850:                <NA>                <NA>              <NA>
#>       symptoms.suddenly fever.start.option fever.start.date fever.suddenly
#>                  <fctr>             <fctr>           <Date>         <fctr>
#>    1:              <NA>               <NA>             <NA>           <NA>
#>    2:              <NA>               <NA>             <NA>           <NA>
#>    3:              <NA>               <NA>             <NA>           <NA>
#>    4:              <NA>               <NA>             <NA>           <NA>
#>    5:              <NA>               <NA>             <NA>           <NA>
#>   ---                                                                     
#> 4846:              <NA>               <NA>             <NA>           <NA>
#> 4847:              <NA>               <NA>             <NA>           <NA>
#> 4848:              <NA>               <NA>             <NA>           <NA>
#> 4849:              <NA>               <NA>             <NA>           <NA>
#> 4850:              <NA>               <NA>             <NA>           <NA>
#>       fever.temperature fever.temperature.value visit.medical.service.no
#>                  <fctr>                  <fctr>                   <fctr>
#>    1:              <NA>                    <NA>                     <NA>
#>    2:              <NA>                    <NA>                     <NA>
#>    3:              <NA>                    <NA>                     <NA>
#>    4:              <NA>                    <NA>                     <NA>
#>    5:              <NA>                    <NA>                     <NA>
#>   ---                                                                   
#> 4846:              <NA>                    <NA>                     <NA>
#> 4847:              <NA>                    <NA>                     <NA>
#> 4848:              <NA>                    <NA>                     <NA>
#> 4849:              <NA>                    <NA>                     <NA>
#> 4850:              <NA>                    <NA>                     <NA>
#>       visit.medical.service.gp visit.medical.service.ae
#>                         <fctr>                   <fctr>
#>    1:                     <NA>                     <NA>
#>    2:                     <NA>                     <NA>
#>    3:                     <NA>                     <NA>
#>    4:                     <NA>                     <NA>
#>    5:                     <NA>                     <NA>
#>   ---                                                  
#> 4846:                     <NA>                     <NA>
#> 4847:                     <NA>                     <NA>
#> 4848:                     <NA>                     <NA>
#> 4849:                     <NA>                     <NA>
#> 4850:                     <NA>                     <NA>
#>       visit.medical.service.hospital visit.medical.service.other
#>                               <fctr>                      <fctr>
#>    1:                           <NA>                        <NA>
#>    2:                           <NA>                        <NA>
#>    3:                           <NA>                        <NA>
#>    4:                           <NA>                        <NA>
#>    5:                           <NA>                        <NA>
#>   ---                                                           
#> 4846:                           <NA>                        <NA>
#> 4847:                           <NA>                        <NA>
#> 4848:                           <NA>                        <NA>
#> 4849:                           <NA>                        <NA>
#> 4850:                           <NA>                        <NA>
#>       visit.medical.service.appointment
#>                                  <fctr>
#>    1:                              <NA>
#>    2:                              <NA>
#>    3:                              <NA>
#>    4:                              <NA>
#>    5:                              <NA>
#>   ---                                  
#> 4846:                              <NA>
#> 4847:                              <NA>
#> 4848:                              <NA>
#> 4849:                              <NA>
#> 4850:                              <NA>
#>       visit.medical.service.howsoon.gp.receptionist
#>                                              <fctr>
#>    1:                                          <NA>
#>    2:                                          <NA>
#>    3:                                          <NA>
#>    4:                                          <NA>
#>    5:                                          <NA>
#>   ---                                              
#> 4846:                                          <NA>
#> 4847:                                          <NA>
#> 4848:                                          <NA>
#> 4849:                                          <NA>
#> 4850:                                          <NA>
#>       visit.medical.service.howsoon.gp.doctor.nurse
#>                                              <fctr>
#>    1:                                          <NA>
#>    2:                                          <NA>
#>    3:                                          <NA>
#>    4:                                          <NA>
#>    5:                                          <NA>
#>   ---                                              
#> 4846:                                          <NA>
#> 4847:                                          <NA>
#> 4848:                                          <NA>
#> 4849:                                          <NA>
#> 4850:                                          <NA>
#>       visit.medical.service.howsoon.nhs visit.medical.service.howsoon.other
#>                                  <fctr>                              <fctr>
#>    1:                              <NA>                                <NA>
#>    2:                              <NA>                                <NA>
#>    3:                              <NA>                                <NA>
#>    4:                              <NA>                                <NA>
#>    5:                              <NA>                                <NA>
#>   ---                                                                      
#> 4846:                              <NA>                                <NA>
#> 4847:                              <NA>                                <NA>
#> 4848:                              <NA>                                <NA>
#> 4849:                              <NA>                                <NA>
#> 4850:                              <NA>                                <NA>
#>       contact.medical.service.no contact.medical.service.gp.receptionist
#>                           <fctr>                                  <fctr>
#>    1:                       <NA>                                    <NA>
#>    2:                       <NA>                                    <NA>
#>    3:                       <NA>                                    <NA>
#>    4:                       <NA>                                    <NA>
#>    5:                       <NA>                                    <NA>
#>   ---                                                                   
#> 4846:                       <NA>                                    <NA>
#> 4847:                       <NA>                                    <NA>
#> 4848:                       <NA>                                    <NA>
#> 4849:                       <NA>                                    <NA>
#> 4850:                       <NA>                                    <NA>
#>       contact.medical.service.gp.doctor contact.medical.service.nhs
#>                                  <fctr>                      <fctr>
#>    1:                              <NA>                        <NA>
#>    2:                              <NA>                        <NA>
#>    3:                              <NA>                        <NA>
#>    4:                              <NA>                        <NA>
#>    5:                              <NA>                        <NA>
#>   ---                                                              
#> 4846:                              <NA>                        <NA>
#> 4847:                              <NA>                        <NA>
#> 4848:                              <NA>                        <NA>
#> 4849:                              <NA>                        <NA>
#> 4850:                              <NA>                        <NA>
#>       contact.medical.service.npfs contact.medical.service.other
#>                             <fctr>                        <fctr>
#>    1:                         <NA>                          <NA>
#>    2:                         <NA>                          <NA>
#>    3:                         <NA>                          <NA>
#>    4:                         <NA>                          <NA>
#>    5:                         <NA>                          <NA>
#>   ---                                                           
#> 4846:                         <NA>                          <NA>
#> 4847:                         <NA>                          <NA>
#> 4848:                         <NA>                          <NA>
#> 4849:                         <NA>                          <NA>
#> 4850:                         <NA>                          <NA>
#>       contact.medical.service.howsoon.gp.receptionist
#>                                                <fctr>
#>    1:                                            <NA>
#>    2:                                            <NA>
#>    3:                                            <NA>
#>    4:                                            <NA>
#>    5:                                            <NA>
#>   ---                                                
#> 4846:                                            <NA>
#> 4847:                                            <NA>
#> 4848:                                            <NA>
#> 4849:                                            <NA>
#> 4850:                                            <NA>
#>       contact.medical.service.howsoon.gp.doctor.nurse
#>                                                <fctr>
#>    1:                                            <NA>
#>    2:                                            <NA>
#>    3:                                            <NA>
#>    4:                                            <NA>
#>    5:                                            <NA>
#>   ---                                                
#> 4846:                                            <NA>
#> 4847:                                            <NA>
#> 4848:                                            <NA>
#> 4849:                                            <NA>
#> 4850:                                            <NA>
#>       contact.medical.service.howsoon.nhs contact.medical.service.howsoon.other
#>                                    <fctr>                                <fctr>
#>    1:                                <NA>                                  <NA>
#>    2:                                <NA>                                  <NA>
#>    3:                                <NA>                                  <NA>
#>    4:                                <NA>                                  <NA>
#>    5:                                <NA>                                  <NA>
#>   ---                                                                          
#> 4846:                                <NA>                                  <NA>
#> 4847:                                <NA>                                  <NA>
#> 4848:                                <NA>                                  <NA>
#> 4849:                                <NA>                                  <NA>
#> 4850:                                <NA>                                  <NA>
#>       no.medication medication.painkillers medication.cough
#>              <fctr>                 <fctr>           <fctr>
#>    1:          <NA>                   <NA>             <NA>
#>    2:          <NA>                   <NA>             <NA>
#>    3:          <NA>                   <NA>             <NA>
#>    4:          <NA>                   <NA>             <NA>
#>    5:          <NA>                   <NA>             <NA>
#>   ---                                                      
#> 4846:          <NA>                   <NA>             <NA>
#> 4847:          <NA>                   <NA>             <NA>
#> 4848:          <NA>                   <NA>             <NA>
#> 4849:          <NA>                   <NA>             <NA>
#> 4850:          <NA>                   <NA>             <NA>
#>       medication.antiviral medication.antibiotic medication.other
#>                     <fctr>                <fctr>           <fctr>
#>    1:                 <NA>                  <NA>             <NA>
#>    2:                 <NA>                  <NA>             <NA>
#>    3:                 <NA>                  <NA>             <NA>
#>    4:                 <NA>                  <NA>             <NA>
#>    5:                 <NA>                  <NA>             <NA>
#>   ---                                                            
#> 4846:                 <NA>                  <NA>             <NA>
#> 4847:                 <NA>                  <NA>             <NA>
#> 4848:                 <NA>                  <NA>             <NA>
#> 4849:                 <NA>                  <NA>             <NA>
#> 4850:                 <NA>                  <NA>             <NA>
#>       medication.dontknow medication.howsoon alter.routine still.altered
#>                    <fctr>              <int>        <fctr>        <fctr>
#>    1:                <NA>                 NA          <NA>          <NA>
#>    2:                <NA>                 NA          <NA>          <NA>
#>    3:                <NA>                 NA          <NA>          <NA>
#>    4:                <NA>                 NA          <NA>          <NA>
#>    5:                <NA>                 NA          <NA>          <NA>
#>   ---                                                                   
#> 4846:                <NA>                 NA          <NA>          <NA>
#> 4847:                <NA>                 NA          <NA>          <NA>
#> 4848:                <NA>                 NA          <NA>          <NA>
#> 4849:                <NA>                 NA          <NA>          <NA>
#> 4850:                <NA>                 NA          <NA>          <NA>
#>       howlong.altered what.do.you.think health.score        onset suddenly
#>                <fctr>            <fctr>        <int>       <char>   <fctr>
#>    1:            <NA>              <NA>           85 not.answered     <NA>
#>    2:            <NA>              <NA>           90 not.answered     <NA>
#>    3:            <NA>              <NA>           86 not.answered     <NA>
#>    4:            <NA>              <NA>           87 not.answered     <NA>
#>    5:            <NA>              <NA>           84 not.answered     <NA>
#>   ---                                                                     
#> 4846:            <NA>              <NA>           95 not.answered     <NA>
#> 4847:            <NA>              <NA>           91 not.answered     <NA>
#> 4848:            <NA>              <NA>           93 not.answered     <NA>
#> 4849:            <NA>              <NA>           91 not.answered     <NA>
#> 4850:            <NA>              <NA>           90 not.answered     <NA>
#>       fever.symptoms systemic.symptoms resp.symptoms general.symptoms
#>               <fctr>            <fctr>        <fctr>           <fctr>
#>    1:              f                 f             f                f
#>    2:              f                 f             f                f
#>    3:              f                 f             f                f
#>    4:              f                 f             f                f
#>    5:              f                 f             f                f
#>   ---                                                                
#> 4846:              f                 f             f                f
#> 4847:              f                 f             f                f
#> 4848:              f                 f             f                f
#> 4849:              f                 f             f                f
#> 4850:              f                 f             f                f
#>       ili.symptoms    ili ili.fever ili.self     gi nReports   min.date
#>             <fctr> <fctr>    <fctr>   <fctr> <fctr>    <int>     <Date>
#>    1:            f      f         f        f      f       11 2013-11-17
#>    2:            f      f         f        f      f       11 2013-11-17
#>    3:            f      f         f        f      f       11 2013-11-17
#>    4:            f      f         f        f      f       11 2013-11-17
#>    5:            f      f         f        f      f       11 2013-11-17
#>   ---                                                                  
#> 4846:            f      f         f        f      f       15 2013-11-11
#> 4847:            f      f         f        f      f       15 2013-11-11
#> 4848:            f      f         f        f      f       15 2013-11-11
#> 4849:            f      f         f        f      f       15 2013-11-11
#> 4850:            f      f         f        f      f       15 2013-11-11
#>         max.date illness.ongoing.at.entry day.of.week month  season education
#>           <Date>                   <lgcl>       <num> <int>  <char>     <int>
#>    1: 2014-02-05                    FALSE           7    12 2013/14         3
#>    2: 2014-02-05                    FALSE           3    12 2013/14         3
#>    3: 2014-02-05                    FALSE           3    12 2013/14         3
#>    4: 2014-02-05                    FALSE           3    12 2013/14         3
#>    5: 2014-02-05                    FALSE           2    12 2013/14         3
#>   ---                                                                        
#> 4846: 2014-03-23                    FALSE           4     2 2013/14         0
#> 4847: 2014-03-23                    FALSE           4     2 2013/14         0
#> 4848: 2014-03-23                    FALSE           4     2 2013/14         0
#> 4849: 2014-03-23                    FALSE           7     3 2013/14         0
#> 4850: 2014-03-23                    FALSE           7     3 2013/14         0
#>       why.vaccine.given.at.school howhear.school.work howhear.bsa howhear.who
#>                            <fctr>              <fctr>      <fctr>       <int>
#>    1:                        <NA>                   f           f           2
#>    2:                        <NA>                   f           f           2
#>    3:                        <NA>                   f           f           2
#>    4:                        <NA>                   f           f           2
#>    5:                        <NA>                   f           f           2
#>   ---                                                                        
#> 4846:                        <NA>                   f           f           2
#> 4847:                        <NA>                   f           f           2
#> 4848:                        <NA>                   f           f           2
#> 4849:                        <NA>                   f           f           2
#> 4850:                        <NA>                   f           f           2
#>       activity.vigorous activity.moderate activity.winter participant_id
#>                   <int>             <int>           <int>          <int>
#>    1:                 0                 3               0            341
#>    2:                 0                 3               0            341
#>    3:                 0                 3               0            341
#>    4:                 0                 3               0            341
#>    5:                 0                 3               0            341
#>   ---                                                                   
#> 4846:                 2                 3               3            298
#> 4847:                 2                 3               3            298
#> 4848:                 2                 3               3            298
#> 4849:                 2                 3               3            298
#> 4850:                 2                 3               3            298
```

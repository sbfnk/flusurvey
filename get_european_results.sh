#!/bin/sh

scp epidb@85.90.70.51:data/epidb_results.custom.uk .

psql -d postgres -c "DROP VIEW epidb_health_status;"
psql -d postgres -c "DROP TABLE epidb_results_intake;"
psql -d postgres -c "DROP TABLE epidb_results_weekly CASCADE;"
pg_restore epidb_results.custom.uk | \
    sed -e 's/^\(.*\)\("Q7b_multi_row1_col1" integer\)\(.*\)$/\1"Q7b" integer\3\
\1\2\3/' \
    -e 's/flusurvey/seb/g' \
    -e 's/^\(.*\)\("Q8_5" boolean NOT NULL\)\(.*\)$/\1\2\3\
\1"Q8b" integer\3/' \
    -e 's/^\(.*\)\("Q11" integer\)\(.*\)$/\1\2\3\
\1"Q12_multi_row1_col1" integer\3\
\1"Q13_multi_row1_col1" integer\3/' \
    -e 's/^\(.*\)\("Q4c" integer\)\(.*\)$/\1\2\3\
\1"Q4d" integer\3/' \
    -e 's/\("Q4d_[0-9]" boolean\) NOT NULL/\1/g' \
    -e 's/^\(.*\)\("Q8b_multi_row4_col1" integer\)\(.*\)$/\1\2\3\
\1"Q8b_multi_row5_col1" integer\3/' | \
    psql -d postgres -f - 

psql -d postgres -f ~/code/flusurvey/add_epidb_key.sql
psql -d postgres -f ~/code/flusurvey/epidb_health_status.sql


#!/bin/sh

scp epidb@85.90.70.27:data/epidb_results.sql .
sed -i 's/ggm/flusurvey/g' epidb_results.sql
psql -d flusurvey -c "DROP VIEW epidb_health_status;"
psql -d flusurvey -c "DROP TABLE epidb_results_intake;"
psql -d flusurvey -c "DROP TABLE epidb_results_weekly;"
psql -d flusurvey -f epidb_results.sql
psql -d flusurvey -f ~/Code/flusurvey/add_epidb_key.sql
psql -d flusurvey -f ~/Code/flusurvey/epidb_health_status.sql


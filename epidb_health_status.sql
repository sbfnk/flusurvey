DROP VIEW epidb_health_status;
CREATE VIEW epidb_health_status AS
    SELECT epidb_results_weekly.id AS epidb_results_weekly_id, CASE true
    WHEN epidb_results_weekly."Q1_0" THEN 'NO-SYMPTOMS'::text WHEN
    ((epidb_results_weekly."Q5" = 0) AND (epidb_results_weekly."Q1_1"
    OR epidb_results_weekly."Q1_11" OR epidb_results_weekly."Q1_8" OR
    epidb_results_weekly."Q1_9") AND (epidb_results_weekly."Q1_5" OR
    epidb_results_weekly."Q1_6" OR epidb_results_weekly."Q1_7")) THEN
    'ILI'::text WHEN ((epidb_results_weekly."Q5" = 1) AND
    (((epidb_results_weekly."Q1_4" OR epidb_results_weekly."Q1_5") OR
    epidb_results_weekly."Q1_6") OR epidb_results_weekly."Q1_7")) THEN
    'COMMON-COLD'::text WHEN ((epidb_results_weekly."Q1_15" OR
    epidb_results_weekly."Q1_16") OR (epidb_results_weekly."Q1_17" AND
    epidb_results_weekly."Q1_18")) THEN 'GASTROINTESTINAL'::text ELSE
    'NON-INFLUENZA'::text END AS status FROM epidb_results_weekly;

ALTER TABLE public.epidb_health_status OWNER TO seb;

DROP VIEW epidb_health_status_notired;
CREATE VIEW epidb_health_status_notired AS
    SELECT epidb_results_weekly.id AS epidb_results_weekly_id, CASE true
    WHEN epidb_results_weekly."Q1_0" THEN 'NO-SYMPTOMS'::text WHEN
    ((epidb_results_weekly."Q5" = 0) AND (epidb_results_weekly."Q1_1"
    OR epidb_results_weekly."Q1_8" OR epidb_results_weekly."Q1_9") AND
    (epidb_results_weekly."Q1_5" OR epidb_results_weekly."Q1_6" OR
    epidb_results_weekly."Q1_7")) THEN
    'ILI'::text WHEN ((epidb_results_weekly."Q5" = 1) AND
    (((epidb_results_weekly."Q1_4" OR epidb_results_weekly."Q1_5") OR
    epidb_results_weekly."Q1_6") OR epidb_results_weekly."Q1_7")) THEN
    'COMMON-COLD'::text WHEN ((epidb_results_weekly."Q1_15" OR
    epidb_results_weekly."Q1_16") OR (epidb_results_weekly."Q1_17" AND
    epidb_results_weekly."Q1_18")) THEN 'GASTROINTESTINAL'::text ELSE
    'NON-INFLUENZA'::text END AS status FROM epidb_results_weekly;

ALTER TABLE public.epidb_health_status_notired OWNER TO seb;

DROP VIEW epidb_health_status_fever;
CREATE VIEW epidb_health_status_fever AS
    SELECT epidb_results_weekly.id AS epidb_results_weekly_id, CASE true
    WHEN epidb_results_weekly."Q1_0" THEN 'NO-SYMPTOMS'::text WHEN
    ((epidb_results_weekly."Q5" = 0) AND epidb_results_weekly."Q1_1"
    AND (epidb_results_weekly."Q1_5" OR epidb_results_weekly."Q1_6" OR
    epidb_results_weekly."Q1_7")) THEN
    'ILI'::text WHEN ((epidb_results_weekly."Q5" = 1) AND
    (((epidb_results_weekly."Q1_4" OR epidb_results_weekly."Q1_5") OR
    epidb_results_weekly."Q1_6") OR epidb_results_weekly."Q1_7")) THEN
    'COMMON-COLD'::text WHEN ((epidb_results_weekly."Q1_15" OR
    epidb_results_weekly."Q1_16") OR (epidb_results_weekly."Q1_17" AND
    epidb_results_weekly."Q1_18")) THEN 'GASTROINTESTINAL'::text ELSE
    'NON-INFLUENZA'::text END AS status FROM epidb_results_weekly;

ALTER TABLE public.epidb_health_status_fever OWNER TO seb;
    

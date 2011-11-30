SELECT year AS year,
       week AS week,
       CASE WHEN a<20 THEN '<20'
       WHEN a>=20 AND a<45 THEN '20..44'
       WHEN a>=45 AND a<65 THEN '45..64'
       WHEN a>=65 THEN '65++'
       END AS agegroup,
       CASE WHEN riskfree='t' THEN 0
       WHEN riskfree='f' THEN 1
       END AS risk,
       CASE WHEN "Q6_0"='t' OR "Q6_1"='t' THEN 1
       ELSE 0
       END AS children,
       CASE WHEN "Q10"=0 THEN 1
       WHEN "Q10"=1 THEN 0
       END AS vaccinated,
       count(*) AS participants, count(ili) AS ili, count(non_ili)
       AS non_ili
  FROM (
SELECT extract(year from age(to_timestamp(I."Q2",'YYYY-MM'))) AS a,
       "Q11_0" AS riskfree,
       "Q6_0", "Q6_1",
       I."Q10",
       NULLIF(S.status = 'ILI', false) AS ili,
       NULLIF(S.status != 'ILI', false) AS non_ili,
       extract(week FROM W.timestamp) AS week,
       extract(year FROM W.timestamp) AS year
  FROM pollster_results_intake AS I,
       pollster_health_status AS S,
        pollster_results_weekly AS W
 WHERE I."Q10"<2
   AND S.pollster_results_weekly_id = W.id
   AND W.global_id = I.global_id AND extract(year from age(to_timestamp(I."Q2",'YYYY-MM'))) > 0
       ) AS statuses
 GROUP BY year,week,agegroup,risk,children,vaccinated
 ORDER BY year,week,agegroup,risk,children,vaccinated;

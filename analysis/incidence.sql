SELECT CASE WHEN a<20 THEN '<20'
            WHEN a>=20 AND a<45 THEN '20..44'
            WHEN a>=45 THEN '45++'
            END AS agegroup,
            count(ili) AS "ILI",
            count(non_ili) AS "No ILI",
            cast(cast(count(ili) AS numeric)/(count(ili)+count(non_ili))
	      AS numeric(2,2)) AS "Incidence",
            cast(cast(count(visited) AS numeric)/(count(ili)) 
              AS numeric(2,2)) AS "Health service visited",
            cast(cast(count(consulted) AS numeric)/(count(ili)) 
              AS numeric(2,2)) AS "Health service consulted" ,
            cast(cast(count(consulted)+count(visited) AS numeric)/(count(ili)) 
              AS numeric(2,2)) AS "Health service all" 
  FROM (
SELECT extract(year from age(to_timestamp(I."Q2",'YYYY-MM'))) AS a,
       NULLIF(S.status = 'ILI', false) AS ili,
       NULLIF(S.status != 'ILI', false) AS non_ili,
       NULLIF((S.status = 'ILI' AND (W."Q7_1" OR W."Q7_2" OR W."Q7_3" OR W."Q7_4" OR W."Q7_5")), 
              false) AS visited,
       NULLIF((S.status = 'ILI' AND (W."Q8_1" OR W."Q8_2" OR W."Q8_3" OR W."Q8_4" OR W."Q8_5")), 
              false) AS consulted
  FROM pollster_results_intake AS I,
       pollster_health_status AS S,
       (SELECT DISTINCT ON (global_id) *
          FROM pollster_results_weekly
         WHERE timestamp BETWEEN 'today'::date-6 AND 'today'::date+1 AND ("Q2" IS NULL OR "Q2" != 0)
         ORDER BY global_id, timestamp DESC) AS W
 WHERE S.pollster_results_weekly_id = W.id
   AND W.global_id = I.global_id AND extract(year from age(to_timestamp(I."Q2",'YYYY-MM'))) > 0
       ) AS statuses
 GROUP BY agegroup ORDER BY agegroup;


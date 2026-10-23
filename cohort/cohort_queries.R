library(dplyr)

# Get every count we'll need for the funnel, in one query
funnel_counts <- dbGetQuery(con, "
  WITH 
  -- Total patients in the records
  all_patients AS (
    SELECT COUNT(*) AS n FROM patients
  ),
  -- Patients with at least one encounter
  patients_with_encounters AS (
    SELECT COUNT(DISTINCT PatientDurableKey) AS n FROM encounters
  ),
  -- Patients ever approached on any of our 3 focus domains (any answer, including declined)
  ever_approached AS (
    SELECT COUNT(DISTINCT PatientDurableKey) AS n
    FROM social_determinants
    WHERE Domain IN ('Financial Resource Strain', 'Housing Stability', 'Transportation Needs')
  ),
  -- Patients who actually answered (not declined/unable)
  answered AS (
    SELECT COUNT(DISTINCT PatientDurableKey) AS n
    FROM social_determinants
    WHERE Domain IN ('Financial Resource Strain', 'Housing Stability', 'Transportation Needs')
      AND AnswerText NOT IN ('Patient declined', 'Patient unable to answer', '*Unknown')
  ),
  -- Approached but only ever declined
  declined_only AS (
    SELECT COUNT(DISTINCT PatientDurableKey) AS n
    FROM social_determinants sd
    WHERE sd.Domain IN ('Financial Resource Strain', 'Housing Stability', 'Transportation Needs')
      AND sd.AnswerText IN ('Patient declined', 'Patient unable to answer', '*Unknown')
      AND sd.PatientDurableKey NOT IN (
        SELECT PatientDurableKey FROM social_determinants
        WHERE Domain IN ('Financial Resource Strain', 'Housing Stability', 'Transportation Needs')
          AND AnswerText NOT IN ('Patient declined', 'Patient unable to answer', '*Unknown')
      )
  ),
  -- Patients positive on at least one domain
  any_positive AS (
    SELECT COUNT(DISTINCT PatientDurableKey) AS n
    FROM social_determinants
    WHERE (Domain = 'Housing Stability' AND AnswerText = 'Yes')
       OR (Domain = 'Transportation Needs' AND AnswerText = 'Yes')
       OR (Domain = 'Financial Resource Strain' AND AnswerText IN ('Yes', 'Already shut off'))
  )
  SELECT
    (SELECT n FROM all_patients) AS total_in_records,
    (SELECT n FROM patients_with_encounters) AS with_encounters,
    (SELECT n FROM ever_approached) AS ever_approached,
    (SELECT n FROM answered) AS approached_and_answered,
    (SELECT n FROM declined_only) AS approached_only_declined,
    (SELECT n FROM any_positive) AS positive_any_domain
")

print(funnel_counts)




domain_counts <- dbGetQuery(con, "
  SELECT
    Domain,
    SUM(CASE WHEN screen = 'Positive' THEN 1 ELSE 0 END) AS n_positive,
    SUM(CASE WHEN screen = 'Negative' THEN 1 ELSE 0 END) AS n_negative
  FROM (
    SELECT 
      PatientDurableKey, 
      Domain,
      CASE 
        WHEN (Domain = 'Housing Stability' AND AnswerText = 'Yes')
          OR (Domain = 'Transportation Needs' AND AnswerText = 'Yes')
          OR (Domain = 'Financial Resource Strain' AND AnswerText IN ('Yes', 'Already shut off'))
        THEN 'Positive'
        WHEN AnswerText = 'No'
        THEN 'Negative'
        ELSE NULL
      END AS screen
    FROM social_determinants
    WHERE Domain IN ('Financial Resource Strain', 'Housing Stability', 'Transportation Needs')
      AND AnswerText NOT IN ('Patient declined', 'Patient unable to answer', '*Unknown')
    GROUP BY PatientDurableKey, Domain, screen
  ) t
  WHERE screen IS NOT NULL
  GROUP BY Domain
")

print(domain_counts)
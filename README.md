# Social Determinants of Health and ER Use as a Substitute for Primary Care

**ASA DataFest 2026, Cal Poly San Luis Obispo · Honorable Mention**

Team: Kenneth Carpenter, Grayson Six, Chloe Schurter, Mira Griffin

Do patients who screen positive for social needs, like housing instability, transportation barriers, or financial strain, use the emergency department instead of primary care? We analyzed four years (2022–2025) of electronic health records from Stormont Vail Health, a Midwest hospital system, covering **947,685 patients**.

## Key findings

- **About 3x the ER visit rate.** After adjusting for age and number of chronic conditions, patients who screened positive for financial resource strain had **3.04 times** the ED visit rate of patients who screened negative (95% CI: 2.88–3.21). That effect was larger than any age group or chronic condition in the model.
- **A larger share of preventable visits.** Positive screeners' ED visits were more often for conditions good primary care should prevent, at every level of primary care engagement, including patients seen by primary care within the last 30 days.

![Adjusted incidence rate ratios](applications/02_adjusted_analysis/adjusted_rate_ratios.png)

## Who we studied

Every result is tied to a clearly defined cohort. The SQL funnel counts unique patients at each step, so the exclusions reconcile exactly: 61,453 approached − 603 who only declined = **60,850 in the study population**.

![Cohort flow diagram](cohort/cohort_diagram.png)

## Interactive dashboards (R Shiny)

Each dashboard lives in its own folder under [`applications/`](applications/), with its app script and screenshots.

| Folder | Question it answers |
|---|---|
| [`01_sdoh_ed_use`](applications/01_sdoh_ed_use/) | Do patients who screen positive for social needs use the ED more? Rates, risk ratios, and significance tests across domains |
| [`02_adjusted_analysis`](applications/02_adjusted_analysis/) | Does the effect survive controls? Stratified rate ratios, a negative binomial forest plot, and a "what this analysis cannot do" section |
| [`03_preventable_visits`](applications/03_preventable_visits/) | How much of the extra ED use is preventable, and which conditions drive it? |
| [`04_care_gaps`](applications/04_care_gaps/) | How long had patients gone without primary care before each ED visit, and does a longer gap predict preventable visits? |
| [`05_roi_calculator`](applications/05_roi_calculator/) | Could investing in community health workers for these patients pay off? Adjustable assumptions, sensitivity analysis, and cited sources |
| [`06_rural_vs_urban`](applications/06_rural_vs_urban/) | How do rural and urban patients' care journeys differ? |
| [`07_cancer_screening`](applications/07_cancer_screening/) | How do cancer screening rates differ by sex and age group? |
| [`08_patient_journey`](applications/08_patient_journey/) | Patient journey viewer: one patient's encounters, diagnoses, and screenings on a timeline (no screenshots, to protect patient privacy) |

### Preventable ED visits

![Preventable visits dashboard](applications/03_preventable_visits/screenshot_1.png)

![Conditions driving the gap](applications/03_preventable_visits/screenshot_2.png)

### Care gaps before the ED

![Care gaps dashboard](applications/04_care_gaps/screenshot_1.png)

![Does the gap predict preventable visits?](applications/04_care_gaps/screenshot_2.png)

### ROI calculator

![ROI calculator](applications/05_roi_calculator/screenshot_1.png)

![Sensitivity analysis and evidence base](applications/05_roi_calculator/screenshot_2.png)

## Methods

**Data pipeline (DuckDB + SQL)**
- Loaded seven related tables (patients, encounters, diagnoses, departments, providers, social determinants, census codes) into a local DuckDB database with indexes on the join keys.
- Built the cohort in one query of CTEs, using `COUNT(DISTINCT PatientDurableKey)` so patients with many visits or answers were counted once.
- Filtered out non-responses ("Patient declined," "Patient unable to answer," "*Unknown") and used a `NOT IN` subquery to identify patients who only ever declined.
- Classified each answer as a positive or negative screen per domain with `CASE WHEN`, since positive answers differ by question (for financial strain, "Already shut off" counts as positive).
- Used a DuckDB window function to compute the days since each patient's last primary care visit before every ED visit.

**Cleaning rules**
- A patient who screened positive in a domain at any visit is treated as positive for that domain.
- Observation windows have a three-month minimum, so patients seen only briefly don't produce inflated visit rates.
- Missing or implausible ages are excluded. Extreme values are capped at the 99th percentile in charts only.

**Modeling**
- Negative binomial regression (`MASS::glm.nb`) on total ED visits, with `log(years observed)` as an offset, adjusting for age band and number of chronic conditions (capped at 5). Negative binomial fits overdispersed count data better than Poisson: most patients have few ED visits, and a small number have many.
- Stratified rate ratios by age band and comorbidity burden, with approximate 95% confidence intervals, to check that the effect holds within each subgroup.
- Preventable visits defined using ambulatory care sensitive conditions (ACSC), based on AHRQ Prevention Quality Indicators.

## Repository layout

```
├── applications/          one folder per Shiny dashboard (app script + screenshots)
├── cohort/                cohort SQL queries and the cohort flow diagram
├── presentation/          competition slides
└── build_database.R       loads the DataFest CSVs into svh.duckdb
```

## Running it

The DataFest data was provided for the competition only, so **no patient data is included in this repository**. With access to the data:

1. Place the DataFest CSV files in the repository root.
2. Run `build_database.R` from the repository root to create `svh.duckdb`.
3. Launch any dashboard from the repository root, for example `shiny::runApp("applications/05_roi_calculator/roi_calculator_app.R")`, or open the file in RStudio and click **Run App**.

R packages: `shiny`, `duckdb`, `DBI`, `dplyr`, `tidyr`, `ggplot2`, `plotly`, `DT`, `MASS`, `broom`.

## Limitations

- This is observational data, so the results show association, not causation.
- Only 61,453 of the 359,760 patients with a visit were ever screened for social needs. Screened patients may differ from those who weren't, so results describe the screened population.
- The ACSC list is an approximation of AHRQ's definitions, and the ROI calculator depends on assumptions drawn from published research rather than hospital cost data.

## Presentation

Our competition slides are in [`presentation/DataFest2026_slides.pdf`](presentation/DataFest2026_slides.pdf).

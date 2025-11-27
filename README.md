# Analysis of Factors Affecting Treatment Duration in Adolescent and Young Adult Substance Use Programs

## Overview

This repository contains an R implementation for analyzing factors that influence length of stay (LOS) in opioid treatment programs among adolescents and young adults (ages 12–20). The project investigates how age at first substance use, treatment modality, employment status, drug use frequency, and route of administration differentially predict treatment duration across developmental subgroups.

Drug overdose deaths have increased dramatically over the past two decades, rising from approximately 20,000 to over 100,000 annually, with a notable acceleration beginning around 2019 (NIDA, 2023). Understanding the factors that influence treatment outcomes in young populations is critical for developing targeted intervention strategies.

## Background

Previous research has established that early age of onset is associated with longer treatment duration and poorer outcomes (McGorry et al., 2011; Poudel & Gautam, 2017). Additionally, mood and anxiety disorders are highly comorbid with substance use disorders (Bauer et al., 2005; Quello et al., 2005), suggesting that patient characteristics at admission may significantly influence treatment trajectories.

This analysis extends the existing literature by examining how predictive factors vary across distinct developmental subgroups within the adolescent and young adult population.

## Research Questions

1. Does age at first substance use significantly predict length of stay in treatment?
2. Which clinical and demographic factors are significant predictors of treatment duration?
3. How do these predictive relationships differ across age subgroups (12–14, 15–17, 18–20)?

## Data Source

**Dataset:** Treatment Episode Data Set—Discharges (TEDS-D) 2020 Public Use File  
**Source:** Substance Abuse and Mental Health Services Administration (SAMHSA), U.S. Department of Health and Human Services  
**Sample:** 1,390,000 discharge records; 76 variables  
**Access:** https://www.datafiles.samhsa.gov/dataset/teds-d-2020-ds0001-teds-d-2020-ds0001

## Methodology

### Sample Selection

To isolate treatment-naive patients with immediate program entry and successful completion, the following inclusion criteria were applied:

| Criterion | Variable | Value |
|-----------|----------|-------|
| No prior treatment episodes | `NOPRIOR` | 0 |
| No wait time for admission | `DAYWAIT` | 0 |
| Treatment completed | `REASON` | 1 |
| Age at admission | `AGE` | ≤3 (12–20 years) |

### Variables

**Response Variable**
- `LOS` — Length of stay in treatment (days), log-transformed to address positive skewness

**Predictor Variables**
| Variable | Description |
|----------|-------------|
| `FRSTUSE1` | Age at first substance use (categorical) |
| `SERVICES_D` | Type of treatment service at discharge |
| `EMPLOY_D` | Employment status at discharge |
| `FREQ1_D` | Frequency of substance use at discharge |
| `ROUTE1` | Primary route of administration |

### Statistical Approach

1. **Distribution Assessment:** Histograms and Q-Q plots to evaluate normality; log transformation applied to LOS
2. **Group Comparisons:** Kruskal-Wallis tests for non-parametric comparison across age-at-first-use groups
3. **Predictor Analysis:** One-way ANOVA with Tukey HSD post-hoc tests for individual predictors
4. **Model Building:** Multi-factor ANOVA with stepwise selection; interaction models for subgroup analyses
5. **Significance Level:** α = 0.05 (two-tailed) for all tests

## Results Summary

### Age at First Use

Significant differences in treatment duration were observed across age-at-first-use groups (H(3) = 332.17, p < 0.01). Pairwise comparisons revealed that younger age at first use was associated with longer treatment stays, with the exception of no significant difference between the 11-and-under and 12–14 groups (H(1) = 1.49, p = 0.22).

### Significant Predictors

The combined model explained approximately 65% of variance in log-transformed LOS (R² ≈ 0.65).

| Predictor | df | F | p |
|-----------|-----|-------|-------|
| Service Type | 7, 3307 | 826.1 | <0.01 |
| Frequency of Use | 2, 3312 | 211.4 | <0.01 |
| Employment Status | 3, 3311 | 148.1 | <0.01 |
| Route of Administration | 4, 3310 | 71.06 | <0.01 |

### Subgroup Models

Predictor significance varied by developmental subgroup:

| Age Group | Significant Main Effects |
|-----------|-------------------------|
| 12–14 | Service type, frequency, employment |
| 15–17 | Service type, frequency, route, employment |
| 18–20 | Service type only (with interaction effects) |

## Repository Structure

```
├── Opioid_research.R        # Primary analysis script
├── README.md                # Project documentation
├── data/
│   └── tedsd_puf_2020.sav   # Raw data (not included; obtain from SAMHSA)
└── output/
    ├── underage.xlsx        # ANOVA results — combined model
    ├── group12-14.xlsx      # ANOVA results — ages 12–14
    ├── group15-17.xlsx      # ANOVA results — ages 15–17
    └── group18-20.xlsx      # ANOVA results — ages 18–20
```

## Dependencies

```r
library(tidyverse)
library(ggplot2)
library(haven)
library(expss)
library(caret)
library(tibble)
library(writexl)
```

## Future Directions

- Compare immediate versus delayed treatment admission
- Extend analysis to adult age groups
- Incorporate longitudinal outcome measures (e.g., relapse rates)

## References

Bauer, M. S., Altshuler, L., Evans, D. R., Beresford, T., Williford, W. O., & Hauger, R. (2005). Prevalence and distinct correlates of anxiety, substance, and combined comorbidity in a multi-site public sector sample with bipolar disorder. *Journal of Affective Disorders, 85*(3), 301–315.

McGorry, P. D., Purcell, R., Goldstone, S., & Amminger, G. P. (2011). Age of onset and timing of treatment for mental and substance use disorders: Implications for preventive intervention strategies and models of care. *Current Opinion in Psychiatry, 24*(4), 301–306.

National Institute on Drug Abuse. (2023). Drug overdose death rates. https://nida.nih.gov/research-topics/trends-statistics/overdose-death-rates

Poudel, A., & Gautam, S. (2017). Age of onset of substance use and psychosocial problems among individuals with substance use disorders. *BMC Psychiatry, 17*, 1–7.

Quello, S. B., Brady, K. T., & Sonne, S. C. (2005). Mood disorders and substance use disorder: A complex comorbidity. *Science & Practice Perspectives, 3*(1), 13.

## Author

Nhi (Sandy) Doan

## Source

SAMHSA. (2020). Treatment Episode Data Set—Discharges (TEDS-D) 2020. Substance Abuse and Mental Health Services Administration. https://www.datafiles.samhsa.gov/dataset/teds-d-2020-ds0001-teds-d-2020-ds0001

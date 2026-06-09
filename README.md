# SamplingCoordination

`SamplingCoordination` is an R package that provides tools for the coordination and management of samples in complex rotative survey designs. It is aimed at statisticians and survey methodologists working on household surveys, labor force surveys, and other social surveys that require controlled sample renewal across time periods.

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("psirusteam/SamplingCoordination")
```

## Overview

The package follows a natural workflow for rotating panel survey design:

1. Generate permanent random numbers for PSU coordination.
2. Define the rotation scheme across periods.
3. Assign PSUs to mini-panels and build the panel matrix.
4. Coordinate samples across periods with controlled overlap.
5. Adjust expansion factors for nonresponse and eligibility.

## Main Functions

### 1. Random number generation

Generate permanent random numbers to coordinate samples across periods.

| Function | Description |
|---|---|
| `generate_random()` | Generates permanent, colocated, Pareto, and PPS random numbers from a vector |
| `generate_random_frame()` | Same as above but works directly with a PSU data frame, with optional stratification |

### 2. Panel design

Define the theoretical rotation structure across periods.

| Function | Description |
|---|---|
| `rotating_panels()` | Generates rotating panels for schemes 4-0-0, 5-0-0, and general A-B-C designs |
| `rotating_panel_222()` | Generates rotating panels following the 2-2-2 scheme (50% overlap between contiguous periods) |

### 3. PSU assignment

Allocate PSUs to mini-panels and build the full period matrix.

| Function | Description |
|---|---|
| `panel_sample_size_nse()` | Allocates sample sizes across mini-panels and SES strata via IPFP |
| `assign_PSUs_to_panels()` | Assigns PSUs to mini-panels sequentially following a theoretical panel sequence |
| `cyclic_panel_adjustment()` | Completes incomplete or missing mini-panels using cyclic assignment |
| `build_panel_matrix()` | Builds the final wide PSU-by-period matrix from assigned and cyclic frames |

### 4. Sample coordination

Select and overlap samples across periods using coordination scores.

| Function | Description |
|---|---|
| `SI_coord()` | Coordinated simple random sampling (negative or positive coordination) |
| `pareto_coord()` | Coordinated Pareto sampling |
| `poisson_coord()` | Coordinated Poisson (PPS) sampling |
| `overlap_sample()` | Selects a coordinated sample between two periods with a target overlap rate |

### 5. Expansion factors

| Function | Description |
|---|---|
| `adjust_fex()` | Adjusts expansion factors: design weight, unknown eligibility, ineligible exclusion, and nonresponse via logistic propensity model |

### 6. Utilities

| Function | Description |
|---|---|
| `redistribution_PSU()` | Redistributes PSU counts across panels |
| `redistribution_PSU_sample()` | Redistributes PSU sample sizes across panels |
| `utils_admissible_scenarios_222()` | Computes admissible scenarios for the 2-2-2 panel |
| `utils_minimal_block_222()` | Generates the minimal repeating block for the 2-2-2 panel |
| `utils_test_contiguous_222()` | Tests overlap properties of a 2-2-2 panel block |
| `utils_create_common_elements_matrix()` | Creates a matrix of common elements between rows |

## Quick Examples

### Generate permanent random numbers for a PSU frame

```r
library(SamplingCoordination)
library(tibble)

frame <- tibble(
  psu       = paste0("PSU", 1:20),
  strata    = rep(c("A", "B"), each = 10),
  dwellings = sample(50:150, 20, replace = TRUE)
)

# Pareto coordination scores by stratum
frame_coord <- generate_random_frame(
  data     = frame,
  id_psu   = psu,
  seed     = 12345,
  method   = "Pareto",
  size_var = dwellings,
  strata   = strata
)
```

### Define a 4-0-0 rotation scheme over 48 quarters

```r
# 12 mini-panels: 4 per month across 3 months
rotation_12 <- cbind(
  rotating_panels(A = 4, B = 0, C = 0, period = 48, value_initial = "A"),
  rotating_panels(A = 4, B = 0, C = 0, period = 48, value_initial = "E"),
  rotating_panels(A = 4, B = 0, C = 0, period = 48, value_initial = "I")
)

# 4 mini-panels: 1 per month, 2 in month 3
rotation_4 <- rotating_panels(A = 4, B = 0, C = 0, period = 48, value_initial = "A")
```

### Allocate sample sizes across mini-panels and SES strata

```r
# monthly_allocation: one row per geographic stratum x month,
# with columns: geo_stratum, month, n_month, n_panels
result_ipfp <- panel_sample_size_nse(
  frame           = psu_frame,
  sample_table    = monthly_allocation,
  geo_column      = "geo_stratum",
  ses_column      = "ses_stratum",
  month_column    = "month",
  n_month_column  = "n_month",
  n_panels_column = "n_panels",
  keep_intermediates = TRUE
)

result_ipfp$ipfp_continuous
result_ipfp$ipfp_rounded
```

### Assign PSUs to mini-panels and build the period matrix

```r
# Assign PSUs
assigned <- assign_PSUs_to_panels(
  DF              = psu_frame,
  stratum_column  = "ses_stratum",
  PSU_column      = "psu_id",
  order_column    = "rank",
  demand_table    = demand,
  panels_sequence = mp_sequence
)

# Complete missing panels cyclically
cyclic <- cyclic_panel_adjustment(
  assigned_frame   = assigned,
  demand_table     = demand,
  panels_by_scheme = list("12_panels" = mp_sequence_12,
                          "4_panels"  = mp_sequence_4),
  scheme_column    = "scheme",
  geo_column       = "geo_stratum",
  stratum_column   = "ses_stratum",
  PSU_column       = "psu_id",
  order_column     = "rank"
)

# Build final matrix
result <- build_panel_matrix(
  assigned_frame   = assigned,
  cyclic_frame     = cyclic,
  rotation_schemes = list("12_panels"   = rotation_12,
                          "4_panels"    = rotation_4,
                          "no_rotation" = NULL),
  sample_table     = monthly_allocation,
  period           = 48,
  geo_column       = "geo_stratum",
  ses_column       = "ses_stratum",
  psu_column       = "psu_id",
  scheme_column    = "scheme"
)

result$panel_matrix
result$verification
```

### Coordinated sample with overlap between two periods

```r
result <- overlap_sample(
  psu_frame   = frame_coord,
  strata      = strata,
  id_psu      = psu,
  n_h         = data.frame(strata = c("A", "B"), n_h = c(4, 4)),
  overlap     = 0.38,
  prev_sample = in_sample_prev,
  method      = "Pareto",
  sort_var    = Xi_Pareto
)
```

## Data

The package includes two example datasets:

- **`Data_PSU`** — 13,420 PSUs with stratum and PSU identifiers.
- **`Data_PSU_aggr`** — 14 strata with PSU counts for population and sample.

## References

Gutierrez, H. A. (2009). *Estrategias de muestreo: diseño de encuestas y estimación de parámetros*. Editorial Universidad Santo Tomás.

## Authors

- José Fernando Zea Castro — <jfzeac@unal.edu.co>
- Hugo Andrés Gutiérrez Rojas (maintainer) — <andres.gutierrez@cepal.org>
- Stalyn Yasid Guerrero Gómez — <syguerrerog@unal.edu.co>
- Yury Vanessa Ochoa Montes — <yury.ochoa@urosario.edu.co>

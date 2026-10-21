# SamplingCoordination <img src="Samplingcoordinationlogo.png" align="right" height="139" alt="Samplingcoordination logo" />

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
4. Coordinate samples across periods and across surveys with controlled overlap.

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
| `basic_rotating_panels()` | Basic block of the 4-0-0 or 5-0-0 scheme |
| `rotating_panels()` | Rotating panels for schemes 4-0-0 (75% overlap between consecutive periods) and 5-0-0 (80%), for any number of periods |
| `rotating_panel_222()` | Generates rotating panels following the 2-2-2 scheme (50% overlap between contiguous periods) |

### 3. PSU assignment

Allocate PSUs to mini-panels and build the full period matrix.

| Function | Description |
|---|---|
| `panel_sample_size_nse()` | Allocates sample sizes across mini-panels and SES strata via IPFP |
| `assign_PSUs_to_panels()` | Assigns PSUs to mini-panels sequentially following a theoretical panel sequence |
| `cyclic_panel_adjustment()` | Completes incomplete or missing mini-panels using cyclic assignment |
| `build_panel_matrix()` | Builds the final wide PSU-by-period matrix from assigned and cyclic frames |
| `panel_sampling()` | Alternative for one stratum: distributes its PSUs across all panels and selects the sample in each panel |

### 4. Sample coordination

#### 4.1 Applied coordination on a PSU frame

Work directly on the PSU frame, by stratum, using ordering numbers already computed (e.g. with `generate_random_frame()`). Use these functions for production surveys.

| Function | Description |
|---|---|
| `select_coordinated_PSUs()` | Negative or positive coordination of a new survey against one or more previous samples, each survey keeping its own ordering number (Pareto, permanent, size...), ascending or descending, by stratum and optionally by period |
| `overlap_sample()` | Rotation within the same survey: keeps a target share of PSUs from the previous period and completes with new PSUs, by stratum |

#### 4.2 Theoretical / illustrative schemes

Classical coordination by shifting one common permanent random number, (&xi; + a) mod 1, on simulated units `1:N`. Useful for teaching and for illustrating the theory of sample coordination.

| Function | Description |
|---|---|
| `SI_coord()` | Coordinated simple random sampling (negative or positive coordination) |
| `pareto_coord()` | Coordinated Pareto sampling (negative or positive coordination) |
| `poisson_coord()` | Coordinated Poisson (PPS) sampling (negative or positive coordination) |

### 5. Utilities

| Function | Description |
|---|---|
| `redistribution_PSU()` | Splits a number of PSUs across panels as evenly as possible (used by `panel_sampling()`) |
| `redistribution_PSU_sample()` | Same split applied to the sample size of a stratum across the panels of a period (used by `panel_sampling()`) |
| `utils_admissible_scenarios_222()` | Computes admissible scenarios for the 2-2-2 panel |
| `utils_minimal_block_222()` | Generates the minimal repeating block for the 2-2-2 panel |
| `utils_test_contiguous_222()` | Tests overlap properties of a 2-2-2 panel block |
| `utils_create_common_elements_matrix()` | Creates a matrix of common elements between rows |

## Quick Examples

### Generate permanent random numbers for a PSU frame

```r
library(SamplingCoordination)
library(tibble)

set.seed(12345)
frame <- tibble(
  psu       = paste0("PSU", 1:20),
  strata    = rep(c("A", "B"), each = 10),
  dwellings = sample(50:150, 20, replace = TRUE)
)

# Pareto coordination scores by stratum (3 PSUs per stratum)
frame_coord <- generate_random_frame(
  data     = frame,
  id_psu   = psu,
  method   = "Pareto",
  size_var = dwellings,
  n_sample = 3,
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

# 4 mini-panels (letters A-D)
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

### Coordinate a new survey against previous samples with different ordering numbers

```r
# frame: one row per PSU with 0/1 flags of previous samples
# and the ordering number of the new survey (e.g. its own Pareto number)
res <- select_coordinated_PSUs(
  frame          = psu_frame,
  n              = sizes_by_stratum_quarter,   # stratum x quarter, column n
  prev_columns   = c("sample_s1", "sample_s2"),
  order_column   = "Xi_Pareto_new",
  decreasing     = FALSE,
  type           = "negative",                 # or "positive"
  rule           = "prioritize",               # or "strict"
  stratum_column = "stratum",
  period_column  = "quarter",
  psu_column     = "psu",
  sample_name    = "sample_new"
)

res$frame          # input frame + overlap_score, rank_sample_new, sample_new, quarter
res$verification   # per stratum: required, selected, overlap with each survey, status
```

### Coordinated sample with overlap between two periods

```r
# Previous-period sample (0/1), here simulated: 4 PSUs per stratum
frame_coord$in_sample_prev <- as.integer(
  ave(frame_coord$Xi_Perman, frame_coord$strata,
      FUN = function(x) rank((x + 0.5) %% 1)) <= 4
)

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

- **`Data_PSU`** — 13,420 rows in 14 strata: 6,710 PSUs, each split into two parts identified by the suffixes `_1` and `_2` of `PSU_id`.
- **`Data_PSU_aggr`** — for each of the 14 strata, the number of PSUs in the population (`N_PSU`, 6,710 in total; a stratum has `2 * N_PSU` rows in `Data_PSU`) and the sample size (`n_PSU`, 915 in total).

## References

- Brewer, K. R. W., Early, L. J. and Joyce, S. F. (1972). Selecting several samples from a single population. *Australian Journal of Statistics*, 14(3), 231–239.
- Deming, W. E. and Stephan, F. F. (1940). On a least squares adjustment of a sampled frequency table when the expected marginal totals are known. *The Annals of Mathematical Statistics*, 11(4), 427–444.
- Gutierrez, H. A. (2009). *Estrategias de muestreo: diseño de encuestas y estimación de parámetros*. Editorial Universidad Santo Tomás.
- Ohlsson, E. (1995). Coordination of samples using permanent random numbers. In B. G. Cox et al. (eds.), *Business Survey Methods*, 153–169. Wiley.
- Ohlsson, E. (1998). Sequential Poisson sampling. *Journal of Official Statistics*, 14(2), 149–162.
- Rosén, B. (1997). On sampling with probability proportional to size. *Journal of Statistical Planning and Inference*, 62(2), 159–191.

## Authors

- José Fernando Zea Castro — <jfzeac@unal.edu.co>
- Hugo Andrés Gutiérrez Rojas (maintainer) — <andres.gutierrez@cepal.org>
- Stalyn Yasid Guerrero Gómez — <syguerrerog@unal.edu.co>
- Yury Vanessa Ochoa Montes — <yury.ochoa@urosario.edu.co>

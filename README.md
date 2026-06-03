# SamplingCoordination


`SamplingCoordination` is an R package that provides tools for the coordination and management of samples in complex rotative survey designs. It is aimed at statisticians and survey methodologists working on household surveys, labor force surveys, and other social surveys that require controlled sample renewal across time periods.

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("psirusteam/SamplingCoordination")
```

## Overview

The package is organized around three main workflows:

**1. Panel design** — define the rotation structure across periods.  
**2. Random number generation** — generate permanent random numbers for coordination.  
**3. Sample coordination** — select and overlap samples across periods using coordination scores.

## Main Functions

### Panel design

| Function | Description |
|---|---|
| `rotating_panels()` | Generates rotating panels for schemes 400, 500, and general A-B-C designs |
| `rotating_panel_222()` | Generates rotating panels following the 2-2-2 scheme (50% overlap) |
| `basic_rotating_panels()` | Generates the basic repeating block of a rotating panel |
| `panel_sampling()` | Assigns PSUs to panels and draws the sample within each panel |

### Random number generation

| Function | Description |
|---|---|
| `generate_random()` | Generates permanent, colocated, Pareto, and PPS random numbers from a vector |
| `generate_random_frame()` | Same as above but works directly with a PSU data frame, with optional stratification |

### Sample coordination

| Function | Description |
|---|---|
| `SI_coord()` | Coordinated simple random sampling (negative or positive coordination) |
| `pareto_coord()` | Coordinated Pareto sampling |
| `poisson_coord()` | Coordinated Poisson (PPS) sampling |
| `overlap_sample()` | Selects a coordinated sample between two periods with a target overlap rate |
| `adjust_fex()` | Adjusts expansion factors: design weight, eligibility, and nonresponse |

### Utilities

| Function | Description |
|---|---|
| `redistribution_PSU()` | Redistributes PSU counts across panels |
| `redistribution_PSU_sample()` | Redistributes PSU sample sizes across panels |
| `utils_admissible_scenarios_222()` | Computes admissible scenarios for the 2-2-2 panel |
| `utils_minimal_block_222()` | Generates the minimal repeating block for the 2-2-2 panel |
| `utils_test_contiguous_222()` | Tests overlap properties of a 2-2-2 panel block |
| `utils_create_common_elements_matrix()` | Creates a matrix of common elements between rows |

## Quick Examples

### Scheme 500 rotating panel (5 consecutive periods, no rest)

```r
library(SamplingCoordination)

rotating_panels(A = 5, B = 0, C = 0, period = 20)
```

### 2-2-2 rotating panel (2 periods on, 2 off, 2 on again)

```r
rotating_panel_222(n_periods = 40)
```

### Generate permanent random numbers for a PSU frame

```r
library(tibble)

frame <- tibble(
  psu       = paste0("PSU", 1:20),
  strata    = rep(c("A", "B"), each = 10),
  dwellings = sample(50:150, 20, replace = TRUE)
)

# Poisson coordination scores by stratum
frame_coord <- generate_random_frame(
  data     = frame,
  id_psu   = psu,
  seed     = 12345,
  method   = "Poisson",
  size_var = dwellings,
  strata   = strata
)
```

### Coordinated sample with overlap between two periods

```r
# Select sample for current period with 38% overlap from previous period
result <- overlap_sample(
  psu_frame   = frame_coord,
  strata      = strata,
  id_psu      = psu,
  n_h         = data.frame(strata = c("A", "B"), n_h = c(4, 4)),
  overlap     = 0.38,
  prev_sample = in_sample_prev,
  method      = "Poisson",
  sort_var    = Xi_Poisson
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

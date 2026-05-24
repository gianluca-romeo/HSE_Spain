# Housing Supply Elasticity in Spain

This repository contains the code used to construct and visualize a proxy for housing supply elasticity (HSE) across Spanish municipalities and autonomous communities.

The proxy is based on the ratio between potentially developable urban land and already built urban surface. The analysis covers the period 1994–2024 and produces spatial maps at both municipal and CCAA level.

### Report

The report of the replication project is available online at:
https://gianluca-romeo.github.io/CSOEM_Replication/
The detailed .pdf report is attached below:
[report.pdf](report.pdf)

---

## Repository structure

```text

.
├── main.R
├── R/
│   ├── functions.R
│   └── prepare_data.R
├── scripts/
│   ├── 01_outlier_analysis.R
│   ├── 02_levels_maps.R
│   └── 03_variations.R
├── outputs/
│   └── figures/
└── data_sample.xlsx
```

---

## Requirements

Main packages used:

```r
library(mapSpain)
library(tidyverse)
library(sf)
library(readxl)
library(rlang)
```

---

## How to run

From the project root:

```r
source("main.R")
```

The script loads the data, computes the HSE proxy, and saves all figures in:

```text
outputs/figures/
```

---

## Data

The full dataset is not included in this repository.  
A sample dataset is provided for demonstration purposes.

---

## Output

The code produces:
- outlier analysis for the 2024 HSE proxy;
- municipal-level HSE maps;
- CCAA-level weighted HSE maps;
- temporal variation maps for HSE, built surface, and total urban surface.

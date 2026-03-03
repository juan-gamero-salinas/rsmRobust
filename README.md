
# rsmRobust <img src="https://img.shields.io/badge/status-CRAN_ready-brightgreen.svg" align="right"/>

<!-- Badges (add/adjust when you publish on GitHub/CRAN) -->

<!-- badges: start -->

<!-- CRAN status: add once on CRAN -->

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/rsmRobust)](https://CRAN.R-project.org/package=rsmRobust) -->

<!-- R-CMD-check: add GH Actions badge once repo exists -->

<!-- badges: end -->

**rsmRobust** provides **robust multiobjective optimization** tools on
top of [`rsm`](https://cran.r-project.org/package=rsm),
[`desirability`](https://cran.r-project.org/package=desirability), and
[`FrF2`](https://cran.r-project.org/package=FrF2):

- **Sequential RSM** wrapper: `sequential_rsm()` (fit + stationary
  point + prediction)
- **Bootstrap CIs for stationary points**:
  `bootstrap_stationary_point()`
- **Robustness visualization**: `plot_robustness_2d()`
- **Screening helper**: `run_fractional_screening()`
- **Noise modeling**: `add_noise_factors()`
- **Two‑response desirability**: `compute_desirability()`

The workflow implements and extends the methodology in:

> Gamero‑Salinas, J., & López‑Fidalgo, J. (2025).  
> *Response Surface Methodology using desirability functions for
> multiobjective optimization to minimize indoor overheating hours and
> maximize useful daylight illuminance*.  
> **Scientific Reports**, 15, 12173. DOI: 10.1038/s41598-025-96376-x

------------------------------------------------------------------------


## Installation

`rsmRobust` is an R package built with a standard R package structure  
(`DESCRIPTION`, `NAMESPACE`, `R/`, `man/`).

To install it from source, you will need the **devtools** package:

```r
install.packages("devtools")
```

---

### 1️⃣ Clone or Download the Repository

#### Option A — Clone with Git

```bash
git clone https://github.com/<your-username>/rsmRobust.git
cd rsmRobust
```

#### Option B — Download Manually

1. Click **Code → Download ZIP**
2. Unzip the folder
3. Open the project in **RStudio**

---

### 2️⃣ Build and Install the Package

Make sure you are inside the package root directory  
(the folder containing the `DESCRIPTION` file), then run:

```r
# Generate documentation from roxygen2 comments
devtools::document()

# Install the package locally
devtools::install(upgrade = "never")

# (Optional) Run package checks
devtools::check(vignettes = FALSE)
```

---

## What These Commands Do

### `devtools::document()`

- Creates `.Rd` documentation files  
- Updates the `NAMESPACE` file  
- Generates documentation from roxygen2 comments  
- You **do not** need to manually edit `DESCRIPTION`, `NAMESPACE`, or files in `R/`

---

### `devtools::install()`

- Installs the package in your local R library  
- Works as if installing from CRAN  


---

## 3️⃣ Load the Package

After installation:

```r
library(rsmRobust)
```

---

## System Requirements

- **R ≥ 4.1.0**
- Dependencies such as `rsm`, `desirability`, and `FrF2`  
  are installed automatically via the `Imports:` field in the `DESCRIPTION` file.



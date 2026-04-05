# CFA Sample Size Simulator

Determining adequate sample size for CFA is one of the least understood decisions in educational and social science research. Most of the time, researchers use Rules-of-thumb that are often inadequate. In reality, sample size requirements can range from as few as 30 to over 460, depending on model structure, loading strength, and factor correlations. This Shiny app makes model-specific Monte Carlo sample size planning accessible without requiring  custom simulation R- scripts.

It implements Monte Carlo power analysis for Confirmatory Factor Analysis (CFA) using **simsem** and **lavaan**,based from Wolf et al. (2013), with targeted bias checks for high-risk parameter combinations.

---

## File Structure

```
cfasim/
├── app.R        
├── global.R     — Libraries, parallel backend, model builders,
│                  bias flag detection, simulation worker
├── ui.R         — Shiny UI 
├── server.R     — Reactive server logic (async parallel, bias-aware)
└── README.md    
```

---

## Installation & Setup

### Option 1 — Clone the repository (recommended)

Make sure you have [Git](https://git-scm.com/) installed, then run in your
terminal:
```bash
git clone https://github.com/hcencabo/cfasim.git
cd cfasim
```

### Option 2 — Download as ZIP

1. Download ZIP
2. Extract the ZIP file to a folder of your choice

Install the following packages

```r
install.packages(c(
  "shiny", "simsem", "lavaan", "ggplot2", "DT",
  "shinycssloaders", "shinyjs",
  "future", "promises", "furrr"
))
```

---

## Running the App

```r
shiny::runApp("path/to/cfasim")
```

---

## Bias Checking — When and Why

Standard adequacy criteria (power ≥ .80, RMSEA < .05, CFI > .95) are
applied for all runs. Three additional bias checks are activated
automatically based on detected parameter combinations:

### Flag 1 — Factor correlation at boundary values
**Triggered when:** `factor_cor ≥ .50` OR `factor_cor ≤ .10` AND
`n_factors > 1`

Correlation estimates are prone to bias at these extremes, particularly
at smaller N. This matters most when discriminant or convergent validity
is the research aim. The check computes relative bias on all
factor-to-factor correlation parameters:

```
bias = |mean_estimated_cor − true_cor| / |true_cor|
```

Rejects N if bias > 5% on any factor correlation.

### Flag 2 — Weak loadings (≤ .40)
**Triggered when:** `loading_value ≤ .40`

Wolf et al. (2013) did not evaluate loadings below .50. At these values
and small N, estimates are pulled toward zero (attenuation). The check
computes relative bias on all factor loading parameters:

```
bias = |mean_estimated_loading − true_loading| / |true_loading|
```

Rejects N if bias > 5% on any loading.

### Flag 3 — Identification boundary (3 indicators per factor)
**Triggered when:** any factor has exactly 3 indicators

Three-indicator factors are overidentified by only 1 df. They are
structurally fragile at small N, producing Heywood cases and
non-convergence at higher rates than larger indicator sets. The check
raises the convergence threshold from descriptive reporting to a hard
gate: **convergence rate must be ≥ 98%**.

---

## Important Note on Replications and Bias

Bias estimates computed from simulation are themselves subject to
Monte Carlo error. At 200–500 replications, a true bias of 4.8% may
appear as 6% or 3% by chance. The app displays a warning when bias
checks are active and nRep < 1000. For publication-level bias
assessment, use **1000 replications**.

---

## Adequacy Criteria Summary

| Criterion | Applies | Threshold |
|---|---|---|
| Min loading power | Always | ≥ .80 |
| Mean RMSEA | Always | < .05 |
| Mean CFI | Always | > .95 |
| Convergence rate | Flag 3 (3-indicator) | ≥ 98% |
| Loading bias | Flag 2 (loading ≤ .40) | < 5% |
| Factor correlation bias | Flag 1 (cor ≥ .50 or ≤ .10) | < 5% |

**Recommended N** = smallest N where ALL applicable criteria are met.

---

## Citation

If you use this app in your research, please cite it as:

Encabo, H. C. (2026). *CFA Sample Size Simulator: A Monte Carlo power
analysis tool for confirmatory factor analysis* [R Shiny application].
GitHub. https://github.com/hcencabo/cfasim

Please also cite the underlying method:

Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013). Sample size requirements for structural equation models: An evaluation of power, bias, and solution propriety. *Educational and Psychological Measurement*, *73*(6), 913–934. https://doi.org/10.1177/0013164413495237



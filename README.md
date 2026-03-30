# CFA Sample Size Simulator 

Monte Carlo power analysis for Confirmatory Factor Analysis (CFA) using
**simsem** and **lavaan**, implementing the Wolf et al. (2013) method.

---

## File Structure

```
cfa_samplesize_app/
├── app.R        — Entry point (sources the 3 files below)
├── global.R     — Libraries, parallel backend setup, helper functions
├── ui.R         — Shiny UI definition (Apple-style light theme)
├── server.R     — Reactive server logic (async parallel simulation)
└── README.md    — This file
```

---

## Installation

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
shiny::runApp("path/to/cfa_samplesize_app")
```

Or open `app.R` in RStudio and click **Run App**.

---

## Performance Design

### Parallel execution
All candidate N values are dispatched **simultaneously** to background R
processes using `future` + `furrr::future_map_dfr()`. On a 4-core machine,
7 candidate N values finish in roughly the time one used to take.

Workers used = `detectCores(logical=FALSE) - 1` (one core reserved for the
Shiny UI process).

### Non-blocking UI
The parallel block runs inside `future_promise({})`. The Shiny event loop
is never blocked — the browser stays interactive while workers compute.
Results arrive via a `then()` callback that writes to `reactiveValues` once.

### Zero in-loop overhead
The old design wrote to `reactiveValues` and called `incProgress()` on
every iteration, forcing serial execution and triggering a re-render per
step. The new design makes **one** state update when all workers finish.

### Deterministic seeds
Each worker seeds as `user_seed + N`, giving different but fully
reproducible random streams regardless of the order workers finish.

---

## Adequacy Criteria (Wolf et al., 2013)

| Criterion | Threshold |
|---|---|
| Minimum loading power | ≥ .80 |
| Mean RMSEA | < .05 |
| Mean CFI | > .95 |

**Recommended N** = smallest N where **all three** criteria are met simultaneously.

---

## Reference

Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013).
Sample size requirements for structural equation models: An evaluation of
power, bias, and solution propriety.
*Educational and Psychological Measurement, 73*(6), 913–934.

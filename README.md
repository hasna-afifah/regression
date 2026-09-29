# Trend Analysis Tool — R Shiny App

An interactive web application for regression and trend analysis, built with R Shiny. Upload your own data, explore distributions, fit models (linear, multiple, polynomial, GLM), check diagnostics, and visualise results — all step by step.

## Features

| Step | What it does |
|------|-------------|
| **1. Upload Data** | Load `.xlsx`, `.xls`, or `.csv` files; choose sheet; preview data |
| **2. Explore** | Histograms, density, boxplots, scatter plots, correlation heatmap |
| **3. Model Setup** | Choose model type, response/predictors, interactions, scaling |
| **4. Results** | Coefficient table, R², AIC/BIC, VIF for multicollinearity |
| **5. Diagnostics** | Q-Q plot, Shapiro-Wilk test, residuals vs fitted, scale-location |
| **6. Visualise** | Regression line with confidence bands, downloadable plot |

### Supported model types

- **Simple linear regression** (`lm`)
- **Multiple linear regression** (`lm` with multiple predictors)
- **Polynomial regression** (quadratic, cubic, up to degree 5)
- **GLM** — Gaussian, Poisson, Binomial, Gamma, Inverse Gaussian families

---

## Quick start (run locally)

### Prerequisites

Install R (≥ 4.1) and RStudio. Then install required packages:

```r
install.packages(c(
  "shiny", "bslib", "readxl", "tidyverse", "car",
  "performance", "DT", "plotly", "broom", "MASS"
))
```

### Run

```r
# In RStudio, open any of the three .R files, then click "Run App"
# OR from the R console:
shiny::runApp("path/to/trend-analysis-app")
```

## How it works — a walkthrough

### 1. Upload your data

Accepted formats: Excel (`.xlsx`, `.xls`) or CSV. Each column = one variable.
The app auto-detects sheet names for Excel files.

### 2. Explore your data

- Check distributions of individual variables (histogram, density, boxplot).
- Look at bivariate scatter plots with a LOESS smoother.
- Inspect the correlation matrix to spot collinearity before modelling.

### 3. Set up your model

- Pick the response (Y) and one or more predictors (X).
- For **GLM**, choose the family (Poisson for counts, Binomial for 0/1, etc.) and link function.
- Optionally **scale** predictors (recommended when they are on different measurement scales).
- Optionally include **two-way interactions**.

### 4. Interpret results

- Coefficient table with estimates, standard errors, t/z-values, p-values, and confidence intervals.
- Fit statistics: R², adjusted R², F-test (for `lm`), deviance and AIC (for `glm`).
- **VIF** table appears automatically when you have 2+ predictors (VIF > 5 = concern).

### 5. Check diagnostics

- **Q-Q plot**: points should follow the dashed line if residuals are normal.
- **Shapiro-Wilk test**: formal normality test (p < 0.05 = residuals not normal).
- **Residuals vs Fitted**: look for random scatter (no patterns = good).
- **Scale-Location**: checks homoscedasticity (constant variance).

### 6. Visualise

- Regression line overlaid on data with confidence bands.
- Adjust confidence level (80%–99%).
- Download the plot as PNG.

---

## Tips

- For **logistic regression**: make sure your response is 0/1, choose GLM → Binomial.
- For **Poisson regression** (count data): choose GLM → Poisson.
- For **Gaussian curves** (e.g. species abundance vs pH): use Polynomial Regression with degree 2.
- Always check diagnostics before trusting results.

---

## Credits

Tool developer: **Hasna Afifah** with Claude assistancy

Built for the Ecological Methods course workflow (Wageningen University).
Powered by R, Shiny, ggplot2, plotly, bslib.

# =============================================================================
# Ecological Statistics Toolkit - app.R
# (extends the original "Trend Analysis Tool": regression, correlation, GLM)
#
#   Guide          - which test? decision chart + method chooser
#   1. Data        - upload .xlsx / .csv or load an example forest dataset
#   2. Explore     - distributions, scatter plots, correlation matrix
#   3. Transform   - normality checks and transformations to normality
#   4. Compare     - t-test family, ANOVA, non-parametric alternatives
#   5. Correlation - Pearson / Spearman / Kendall, chi-square, Fisher
#   6. Regression  - LM, polynomial, GLM (Poisson, neg. binomial, binomial...),
#                    optional random factors (mixed models, lme4)
#   7. Multivariate- PCA, NMDS + PERMANOVA, cluster analysis
#
# The code for each tab lives in R/mod_*.R (sourced automatically by Shiny).
#
# Tool developer : Hasna Afifah with Claude assistance
# =============================================================================

# --- Packages -----------------------------------------------------------------
library(shiny)
library(bslib)
library(readxl)
library(tidyverse)
library(car)
library(DT)
library(plotly)
library(broom)
# MASS (negative binomial GLM), lme4 / lmerTest (random factors) and vegan
# (NMDS, PERMANOVA) are called with pkg:: so they do not mask dplyr::select.

# When the app is started with source("app.R") instead of runApp(), the R/
# folder is not sourced automatically - do it here.
if (!exists("app_theme")) {
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
}

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  title    = "Ecological Statistics Toolkit",
  theme    = app_theme,
  fillable = FALSE,
  header   = tags$head(tags$style(HTML(
    ".navbar .nav-link.active, .navbar .nav-link:hover { color: #ffffff !important; }
     .navbar .nav-link.active { border-bottom-color: #ffffff !important; font-weight: 600; }"))),

  nav_panel("Guide",          icon = icon("signs-post"),   guideUI("guide")),
  nav_panel("1. Data",        icon = icon("upload"),       dataUI("data")),
  nav_panel("2. Explore",     icon = icon("chart-bar"),    exploreUI("explore")),
  nav_panel("3. Transform",   icon = icon("wand-magic-sparkles"), transformUI("transform")),
  nav_panel("4. Compare groups", icon = icon("scale-balanced"), groupsUI("groups")),
  nav_panel("5. Correlation", icon = icon("link"),         associationUI("assoc")),
  nav_panel("6. Regression",  icon = icon("chart-line"),   regressionUI("reg")),
  nav_panel("7. Multivariate", icon = icon("diagram-project"), multivariateUI("multi")),

  nav_panel(
    title = "About",
    icon  = icon("info-circle"),
    card(
      card_header("How to Use This App"),
      tags$div(
        class = "p-3",
        tags$h5("A glimpse"),
        tags$ol(
          start = 0,
          tags$li(tags$strong("Guide"), " - not sure which test to use? Answer two or three questions or read the decision chart."),
          tags$li(tags$strong("Upload Data"), " - load your .xlsx or .csv file, or an example forest dataset."),
          tags$li(tags$strong("Explore"), " - histograms, scatter plots, correlations."),
          tags$li(tags$strong("Transform"), " - check normality (Shapiro-Wilk, Q-Q plot) and transform skewed variables (log, square root, logit, Box-Cox, Yeo-Johnson...)."),
          tags$li(tags$strong("Compare groups"), " - t-test family, ANOVA with post-hoc tests, and non-parametric alternatives."),
          tags$li(tags$strong("Correlation"), " - Pearson vs Spearman vs Kendall; chi-square and Fisher's exact test for categorical variables."),
          tags$li(tags$strong("Regression"), " - four sub-tabs that keep the original workflow: ",
                  tags$strong("Model Setup"), " (choose model type, response, predictors, random factors), ",
                  tags$strong("Results"), " (coefficients, R-squared, AIC, VIF), ",
                  tags$strong("Diagnostics"), " (Q-Q, Shapiro-Wilk, residual plots), ",
                  tags$strong("Visualise"), " (regression line with confidence bands)."),
          tags$li(tags$strong("Multivariate"), " - PCA, NMDS with PERMANOVA, cluster analysis.")
        ),
        hr(),
        tags$h5("Supported models"),
        tags$ul(
          tags$li("Simple & multiple linear regression (lm)"),
          tags$li("Polynomial regression (degree 2-5)"),
          tags$li("GLM: Gaussian, Poisson, Negative binomial, Binomial, Gamma, Inverse Gaussian"),
          tags$li("Mixed models with random factors: LMM (lmer) and GLMM (glmer) with random intercepts")
        ),
        tags$h5("Supported tests"),
        tags$ul(
          tags$li("One-sample, two-sample (Welch / Student) and paired t-tests"),
          tags$li("One-way ANOVA + Tukey HSD, Welch's ANOVA, two-way ANOVA"),
          tags$li("Wilcoxon signed-rank, Mann-Whitney U, Kruskal-Wallis"),
          tags$li("Pearson, Spearman and Kendall correlation"),
          tags$li("Chi-square (independence, goodness of fit), Fisher's exact test"),
          tags$li("PCA, NMDS, PERMANOVA, hierarchical clustering")
        ),
        hr(),
        tags$h4("How it works - a walkthrough"),

        tags$h5("1. Upload your data"),
        tags$p("Accepted formats: Excel (.xlsx, .xls) or CSV. Each column should be ",
               "one variable. The app auto-detects sheet names for Excel files."),

        tags$h5("2. Explore your data"),
        tags$ul(
          tags$li("Check distributions of individual variables (histogram, density, boxplot)."),
          tags$li("Look at bivariate scatter plots with a LOESS smoother."),
          tags$li("Inspect the correlation matrix to spot collinearity before modelling.")
        ),

        tags$h5("3. Set up your model (Regression > Model setup)"),
        tags$ul(
          tags$li("Pick the response (Y) and one or more predictors (X)."),
          tags$li("For GLM, choose the family (Poisson for counts, Binomial for 0/1, etc.) ",
                  "and link function."),
          tags$li("Optionally scale predictors (recommended when they are on different ",
                  "measurement scales)."),
          tags$li("Optionally include two-way interactions."),
          tags$li("Optionally add ", tags$strong("random factors"), " (site, block, plot ID, year) when ",
                  "observations are grouped or repeated. The app then fits a mixed model ",
                  "with a random intercept for each factor.")
        ),

        tags$h5("4. Interpret results (Regression > Results)"),
        tags$ul(
          tags$li("Coefficient table with estimates, standard errors, t/z-values, p-values, ",
                  "and confidence intervals."),
          tags$li("Fit statistics: R-squared, adjusted R-squared, F-test (for lm), ",
                  "deviance and AIC (for glm), marginal and conditional R-squared (for mixed models)."),
          tags$li("VIF table appears automatically when you have 2+ predictors ",
                  "(VIF > 5 = concern)."),
          tags$li("With random factors: a variance-components table shows how much of the ",
                  "variation lies between groups (intraclass correlation).")
        ),

        tags$h5("5. Check diagnostics (Regression > Diagnostics)"),
        tags$ul(
          tags$li(tags$strong("Q-Q plot: "), "points should follow the dashed line ",
                  "if residuals are normal."),
          tags$li(tags$strong("Shapiro-Wilk test: "), "formal normality test ",
                  "(p < 0.05 = residuals not normal)."),
          tags$li(tags$strong("Residuals vs Fitted: "), "look for random scatter ",
                  "(no patterns = good)."),
          tags$li(tags$strong("Scale-Location: "), "checks homoscedasticity ",
                  "(constant variance).")
        ),

        tags$h5("6. Visualise (Regression > Visualise)"),
        tags$ul(
          tags$li("Regression line overlaid on data with confidence bands."),
          tags$li("Adjust confidence level (80% - 99%)."),
          tags$li("Download the plot as PNG.")
        ),

        hr(),
        tags$h5("Tips"),
        tags$ul(
          tags$li("Start by exploring data types and distributions to get a feel ",
                  "for which analysis is appropriate."),
          tags$li("If your data is not normally distributed, consider applying a ",
                  "transformation (e.g. log, square root, logit) before fitting ",
                  "a linear model. Do not proceed until the assumption of normality ",
                  "is reasonably met. The Transform tab does this and adds the new column to your data."),
          tags$li("For ", tags$strong("logistic regression"), ": ensure the response ",
                  "variable is binary (0/1), then choose GLM -> Binomial."),
          tags$li("For ", tags$strong("Poisson regression"), " (count data): ",
                  "choose GLM -> Poisson. If the dispersion statistic is well above 1, switch to Negative binomial."),
          tags$li("For ", tags$strong("Gaussian response curves"),
                  " (e.g. species abundance vs pH): ",
                  "use Polynomial Regression with degree 2."),
          tags$li("Scale your predictors when variables are measured on different units."),
          tags$li("For ", tags$strong("grouped or repeated data"), " (plots within sites, the same plots ",
                  "re-measured): add the grouping variable as a random factor instead of ignoring it."),
          tags$li("Normality matters for the ", tags$em("residuals"), " of a model or the values within each group - ",
                  "not for the pooled raw variable."),
          tags$li("Report an effect size (difference in means, r, R-squared, Cohen's d) next to every p-value."),
          tags$li("Always check the diagnostic plots before trusting your results.")
        ),

        tags$h5("Learning material"),
        tags$p("A step-by-step teaching module with forest-ecology examples is in the ",
               tags$code("module/"), " folder of the repository (R Markdown + HTML). ",
               "The example datasets are simulated; the script that creates them is ",
               tags$code("data-raw/make_example_data.R"), "."),

        hr(),
        tags$p(
          style = "text-align: center; color: #666; font-size: 0.9em;",
          "Credits: ", tags$strong("Hasna Afifah"),
          " with Claude assistance"
        ),
        tags$p(
          style = "text-align: center; font-size: 0.9em;",
          tags$a(href = "https://github.com/hasna-afifah/regression",
                 target = "_blank",
                 icon("github"), " github.com/hasna-afifah/regression")
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # Working dataset shared by all tabs. Tabs that create new columns
  # (Transform, PCA scores, clusters) write back to rv$data.
  rv <- reactiveValues(data = NULL, name = NULL)

  guideServer("guide")
  dataServer("data", rv)
  exploreServer("explore", rv)
  transformServer("transform", rv)
  groupsServer("groups", rv)
  associationServer("assoc", rv)
  regressionServer("reg", rv)
  multivariateServer("multi", rv)
}

# =============================================================================
# RUN
# =============================================================================
shinyApp(ui = ui, server = server)

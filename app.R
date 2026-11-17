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
#                    ROC / AUC for logistic models,
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
          tags$li(tags$strong("Regression"), " - sub-tabs that keep the original workflow: ",
                  tags$strong("Model Setup"), " (choose model type, response, predictors, random factors), ",
                  tags$strong("Results"), " (coefficients, R-squared, AIC, VIF), ",
                  tags$strong("Diagnostics"), " (Q-Q, Shapiro-Wilk, residual plots), ",
                  tags$strong("ROC / AUC"), " (appears only for logistic regression), ",
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
        tags$h4("How it works — a walkthrough"),

        tags$h5("0. Not sure which test to use? (Guide)"),
        tags$p("Answer two or three questions in the ", tags$strong("Method chooser"),
               " or study the decision chart — the app recommends a method, tells you ",
               "where to find it, and shows the R code and an ecological example."),

        tags$h5("1. Upload your data (Data)"),
        tags$p("Accepted formats: Excel (.xlsx, .xls) or CSV. Each column should be ",
               "one variable. The app auto-detects sheet names for Excel files. ",
               "You can also click ", tags$strong("Load example data"),
               " to try everything with a simulated forest dataset."),

        tags$h5("2. Explore your data (Explore)"),
        tags$ul(
          tags$li("Check distributions of individual variables (histogram, density, boxplot), ",
                  "optionally split by a grouping variable."),
          tags$li("Look at bivariate scatter plots with a LOESS smoother."),
          tags$li("Inspect the correlation matrix to spot collinearity before modelling.")
        ),

        tags$h5("3. Check and fix normality (Transform)"),
        tags$ul(
          tags$li("Run a Shapiro-Wilk test and Q-Q plot on any numeric column."),
          tags$li("Try 10+ transformations (log, ln(x+1), square root, cube root, reciprocal, ",
                  "square, arcsine-sqrt, logit, Box-Cox, Yeo-Johnson, rank-based inverse normal) ",
                  "— the app ranks them all and lets you add the best one to your data."),
          tags$li("See the before/after Q-Q plot side by side.")
        ),

        tags$h5("4. Compare groups (Compare groups)"),
        tags$ul(
          tags$li(tags$strong("One-sample"), ", ", tags$strong("two-sample"),
                  " (Welch / Student) and ", tags$strong("paired t-tests"), "."),
          tags$li(tags$strong("One-way ANOVA"), " + Tukey HSD, ",
                  tags$strong("Welch's ANOVA"), ", and ", tags$strong("two-way ANOVA"), "."),
          tags$li("Non-parametric alternatives: ", tags$strong("Wilcoxon signed-rank"), ", ",
                  tags$strong("Mann-Whitney U"), ", ", tags$strong("Kruskal-Wallis"),
                  " with pairwise comparisons."),
          tags$li("Automatic assumption checks (normality, equal variances) and ",
                  tags$strong("effect sizes"), " (Cohen's d, eta-squared).")
        ),

        tags$h5("5. Measure correlation (Correlation)"),
        tags$ul(
          tags$li(tags$strong("Pearson"), ", ", tags$strong("Spearman"), ", and ",
                  tags$strong("Kendall"), " correlation side by side, with significance ",
                  "and confidence intervals."),
          tags$li("Full correlation matrix with significance stars."),
          tags$li(tags$strong("Chi-square"), " tests of independence and goodness of fit, ",
                  tags$strong("Fisher's exact test"), ", and ", tags$strong("Cramér's V"),
                  " for categorical variables.")
        ),

        tags$h5("6. Fit a regression model (Regression)"),
        tags$p("Four sub-tabs walk you through the modelling workflow ",
               "(a fifth, ROC / AUC, appears when you fit a logistic regression):"),
        tags$ul(
          tags$li(tags$strong("Model setup"), " — pick response (Y) and predictor(s) (X); ",
                  "choose Linear, Polynomial, or GLM (Gaussian, Poisson, Negative binomial, ",
                  "Binomial, Gamma, Inverse Gaussian); optionally ", tags$strong("scale"),
                  " predictors, include ", tags$strong("interactions"), ", or add ",
                  tags$strong("random factors"), " (site, block, year) for a mixed model."),
          tags$li(tags$strong("Results"), " — coefficient table (estimates, SE, t/z, p, CI); ",
                  "fit statistics (R², adj. R², F-test, AIC, deviance, marginal/conditional R²); ",
                  tags$strong("VIF"), " for collinearity; variance components and ICC for mixed models."),
          tags$li(tags$strong("Diagnostics"), " — Q-Q plot, Shapiro-Wilk test, Residuals vs Fitted, ",
                  "Scale-Location. Warnings appear when an assumption is violated."),
          tags$li(tags$strong("ROC / AUC"), " — logistic regression (GLM → Binomial) only: ROC curve, ",
                  "AUC with 95% CI, 5-fold cross-validated AUC, and a confusion matrix with sensitivity, ",
                  "specificity, TSS and kappa at any threshold (one click sets the optimal threshold)."),
          tags$li(tags$strong("Visualise"), " — regression line with confidence bands (80%–99%); ",
                  "download as PNG.")
        ),
        tags$div(
          class = "alert alert-info py-2 px-3", style = "font-size: 0.88rem;",
          tags$strong("Regression tips"), tags$br(),
          tags$ul(class = "mb-0 mt-1",
            tags$li("For ", tags$strong("logistic regression"), ": make sure your response is 0/1, ",
                    "choose GLM → Binomial, then check the ", tags$strong("ROC / AUC"),
                    " sub-tab (AUC > 0.7 acceptable, > 0.8 excellent)."),
            tags$li("For ", tags$strong("Poisson regression"), " (count data): ",
                    "choose GLM → Poisson. If the dispersion statistic is well above 1, switch to Negative binomial."),
            tags$li("For ", tags$strong("Gaussian response curves"),
                    " (e.g. species abundance vs pH): ",
                    "use Polynomial Regression with degree 2."),
            tags$li("For ", tags$strong("positive right-skewed responses"), " (biomass, DBH): ",
                    "use a Gamma GLM with log link, or log-transform the response and fit a linear model."),
            tags$li("For ", tags$strong("proportions"), " (canopy cover %): logit-transform first, ",
                    "or use a binomial GLM if the proportion comes from counts."),
            tags$li(tags$strong("Scale predictors"), " when variables are measured on different units."),
            tags$li("For ", tags$strong("grouped or repeated data"), " (plots within sites, re-measured plots): ",
                    "add the grouping variable as a ", tags$strong("random factor"), " instead of ignoring it."),
            tags$li("Always check the ", tags$strong("diagnostic plots"), " before trusting regression results.")
          )
        ),

        tags$h5("7. Multivariate analysis (Multivariate)"),
        tags$ul(
          tags$li(tags$strong("PCA"), ": biplot, scree plot, loadings table; export PC scores to your data."),
          tags$li(tags$strong("NMDS"), " (Bray-Curtis): ordination plot with group ellipses; ",
                  tags$strong("PERMANOVA"), " and betadisper test for compositional differences."),
          tags$li(tags$strong("Hierarchical cluster analysis"), ": dendrogram with group labels; ",
                  "Euclidean or Bray-Curtis distance.")
        ),

        hr(),
        tags$h4("Tips"),

        tags$h5("General"),
        tags$ul(
          tags$li(tags$strong("Start with the Guide"), " if you are not sure which analysis to use."),
          tags$li("Explore data types and distributions first — it tells you which method ",
                  "is appropriate."),
          tags$li(tags$strong("Normality matters for the residuals"), " (or the values within each group), ",
                  "not for the pooled raw variable."),
          tags$li("Report ", tags$strong("effect sizes"), " (difference in means, r, R², Cohen's d, eta-squared) ",
                  "alongside every p-value."),
          tags$li("Always check ", tags$strong("assumptions and diagnostics"), " before trusting your results.")
        ),

        tags$h5("Data & exploration"),
        tags$ul(
          tags$li("Check the ", tags$strong("raw distributions"), " before jumping into analysis; ",
                  "a histogram or boxplot can reveal outliers, skew, or unexpected gaps."),
          tags$li("Use the ", tags$strong("correlation matrix"), " early to spot collinearity — ",
                  "highly correlated predictors can inflate standard errors in regression.")
        ),

        tags$h5("Transformation"),
        tags$ul(
          tags$li("Right-skewed positive data (biomass, DBH): try ", tags$strong("log"), " or ",
                  tags$strong("square root"), "."),
          tags$li("Counts (stems, seedlings): ", tags$strong("square root"), " or ",
                  tags$strong("ln(x + 1)"), " — but a Poisson / negative binomial GLM ",
                  "is usually better than transforming."),
          tags$li("Proportions or percentages (canopy cover): ", tags$strong("logit"), " or ",
                  tags$strong("arcsine-sqrt"), " — or a binomial GLM if the proportion comes from counts."),
          tags$li("Left-skewed data: try ", tags$strong("squaring"), "."),
          tags$li("Not sure? Let ", tags$strong("Box-Cox"), " (x > 0) or ",
                  tags$strong("Yeo-Johnson"), " pick the power for you; the ranking table ",
                  "compares all options at a glance."),
          tags$li("Re-check the model diagnostics ", tags$strong("after"), " transforming — ",
                  "the Q-Q plot of the original raw variable is not what matters.")
        ),

        tags$h5("Comparing groups"),
        tags$ul(
          tags$li("Parametric tests (t-test, ANOVA) compare ", tags$strong("means"),
                  " and assume normal residuals; non-parametric tests (Wilcoxon, Mann-Whitney, ",
                  "Kruskal-Wallis) work on ", tags$strong("ranks"), " — use them when normality ",
                  "fails and transformation doesn't help."),
          tags$li("For ", tags$strong("two independent groups"), ", Welch's t-test is the safer default ",
                  "because it does not assume equal variances."),
          tags$li("For ", tags$strong("paired data"), " (same plots measured twice), the assumption is ",
                  "normality of the ", tags$em("differences"), ", not of the raw values."),
          tags$li("With ", tags$strong("unequal variances"), " across three or more groups, use ",
                  "Welch's ANOVA instead of the classic one-way ANOVA."),
          tags$li("If residuals of a two-way ANOVA are not normal, transform the response or ",
                  "model the data with a GLM in the Regression tab (factors can be used as predictors).")
        ),

        tags$h5("Correlation"),
        tags$ul(
          tags$li(tags$strong("Pearson"), " measures how close the points are to a straight line — it needs ",
                  "roughly normal variables and is sensitive to outliers."),
          tags$li(tags$strong("Spearman"), " is Pearson on the ranks: it detects any monotonic relationship ",
                  "and is robust to outliers and skew."),
          tags$li("If ", tags$strong("Spearman is clearly larger"), " than Pearson, the relationship is ",
                  "probably curved or influenced by outliers."),
          tags$li("Correlation is symmetric and says nothing about ", tags$strong("cause"),
                  " — to predict Y from X use the Regression tab."),
          tags$li("For ", tags$strong("chi-square tests"), ", expected counts should be ≥ 5 in most cells; ",
                  "otherwise rely on ", tags$strong("Fisher's exact test"), " or merge rare categories.")
        ),

        tags$h5("Multivariate"),
        tags$ul(
          tags$li(tags$strong("PCA"), " is for continuous environmental variables (soil, stand structure); ",
                  "for species abundance tables (many zeros, non-linear) use ", tags$strong("NMDS"), " instead."),
          tags$li("Standardise (scale) PCA variables when their ", tags$strong("units differ"),
                  "; the app does this by default."),
          tags$li("Export ", tags$strong("PC scores"), " and use them as predictors in regression to avoid ",
                  "collinearity among the original variables."),
          tags$li("NMDS ", tags$strong("stress"), " < 0.05 = excellent, < 0.1 = good, < 0.2 = usable, ",
                  "> 0.2 = poor — consider more dimensions or removing outlier plots."),
          tags$li("If ", tags$strong("betadisper"), " is significant, a significant PERMANOVA may partly ",
                  "reflect differences in within-group spread, not only in average composition."),
          tags$li("For cluster analysis, use ", tags$strong("Euclidean distance on standardised variables"),
                  " for environmental data and ", tags$strong("Bray-Curtis"), " for species abundance data.")
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

# =============================================================================
# mod_guide.R - "Which test?" : decision chart + interactive method chooser
# =============================================================================

guideUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Method chooser"),
        radioButtons(ns("goal"), "1. What do you want to do?",
                     choices = c(
                       "Compare groups"                              = "compare",
                       "Measure the correlation between two variables" = "relate",
                       "Predict / explain a response (regression)"   = "predict",
                       "Summarise many variables at once"            = "multi")),
        uiOutput(ns("questions"))
      ),
      card(
        card_header("Recommendation"),
        uiOutput(ns("recommendation"))
      )
    ),
    card(
      card_header("Decision chart: when to use which method"),
      tags$div(style = "overflow-x: auto;",
               tags$img(src = "method_decision_chart.svg",
                        alt = "Decision chart for choosing a statistical method",
                        style = "width: 100%; min-width: 900px; height: auto;")),
      tags$small(class = "text-muted",
                 "The chart is also saved as www/method_decision_chart.svg / .png for slides and handouts.")
    )
  )
}

# One recommendation = method, where in the app, why, R code, ecological example
rec <- function(method, tab, why, code, example, alt = NULL) {
  list(method = method, tab = tab, why = why, code = code, example = example, alt = alt)
}

guideServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$questions <- renderUI({
      switch(
        input$goal,
        compare = tagList(
          radioButtons(ns("n_groups"), "2. How many groups?",
                       choices = c("One group vs a reference value" = "1",
                                   "Two groups" = "2",
                                   "Three or more groups" = "3")),
          conditionalPanel(
            "input.n_groups == '2'", ns = ns,
            radioButtons(ns("paired"), "Are the two sets of measurements paired?",
                         choices = c("No - different plots / individuals" = "no",
                                     "Yes - same plots measured twice" = "yes"))),
          conditionalPanel(
            "input.n_groups == '3'", ns = ns,
            radioButtons(ns("factors"), "How many grouping factors?",
                         choices = c("One (e.g. forest type)" = "1",
                                     "Two (e.g. forest type and fire history)" = "2"))),
          radioButtons(ns("normal"), "3. Are the data (residuals) roughly normal?",
                       choices = c("Yes" = "yes",
                                   "No, even after transformation" = "no",
                                   "I don't know yet" = "unknown"))
        ),
        relate = tagList(
          radioButtons(ns("var_types"), "2. What kind of variables?",
                       choices = c("Both numeric" = "nn",
                                   "Both categorical" = "cc",
                                   "One numeric, one categorical" = "nc")),
          conditionalPanel(
            "input.var_types == 'nn'", ns = ns,
            radioButtons(ns("linear"), "3. Is the relationship linear and are both variables roughly normal?",
                         choices = c("Yes" = "yes",
                                     "No (curved, skewed, outliers, ordinal scores)" = "no",
                                     "I don't know yet" = "unknown")))
        ),
        predict = tagList(
          radioButtons(
            ns("resp_type"), "2. What kind of response variable (Y)?",
            choices = c("Continuous (height, soil pH, basal area)"        = "cont",
                        "Count (stems, seedlings, species richness)"      = "count",
                        "Binary (presence/absence, alive/dead)"           = "binary",
                        "Positive and right-skewed (biomass, DBH)"        = "skew",
                        "Proportion or percentage (canopy cover)"         = "prop")),
          radioButtons(
            ns("grouped"), "3. Are the observations grouped or repeated?",
            choices = c("No - every plot is independent" = "no",
                        "Yes - plots within sites / blocks, or the same plots measured repeatedly" = "yes"))
        ),
        multi = radioButtons(
          ns("multi_type"), "2. What kind of data?",
          choices = c("Several continuous variables (soil, stand structure)" = "env",
                      "Species abundances per plot (community data)"     = "comm",
                      "I want to group similar plots"                    = "cluster"))
      )
    })

    recommendation <- reactive({
      goal <- input$goal
      if (goal == "compare") {
        req(input$n_groups, input$normal)
        if (input$normal == "unknown")
          return(rec("First check normality", "3. Transform",
                     "Parametric tests assume normal residuals. Look at the histogram and Q-Q plot and run Shapiro-Wilk; if the variable is skewed, try a transformation.",
                     "shapiro.test(x); qqnorm(x); qqline(x)",
                     "Above-ground biomass is right-skewed; ln(biomass) is close to normal."))
        par <- input$normal == "yes"
        if (input$n_groups == "1") {
          if (par) rec("One-sample t-test", "4. Compare groups",
                       "Compares the mean of one sample with a known or target value.",
                       "t.test(x, mu = 5.5)",
                       "Is mean soil pH in the plots different from the optimum of 5.5?",
                       "Not normal: one-sample Wilcoxon signed-rank test.")
          else rec("One-sample Wilcoxon signed-rank test", "4. Compare groups",
                   "Rank-based alternative to the one-sample t-test.",
                   "wilcox.test(x, mu = 5.5)",
                   "Is median seedling density different from the restoration target?")
        } else if (input$n_groups == "2") {
          req(input$paired)
          if (input$paired == "yes") {
            if (par) rec("Paired t-test", "4. Compare groups",
                         "The same units are measured twice, so the test works on the within-plot differences (which should be roughly normal).",
                         "t.test(wet, dry, paired = TRUE)",
                         "Is soil moisture in the same plots higher in the wet than in the dry season?",
                         "Differences not normal: Wilcoxon signed-rank test.")
            else rec("Wilcoxon signed-rank test (paired)", "4. Compare groups",
                     "Rank-based alternative to the paired t-test.",
                     "wilcox.test(wet, dry, paired = TRUE)",
                     "Seedling counts in the same quadrats before and after thinning.")
          } else {
            if (par) rec("Two-sample t-test (Welch)", "4. Compare groups",
                         "Compares the means of two independent groups. Welch's version does not require equal variances and is the safe default.",
                         "t.test(y ~ group, data = d)",
                         "Is basal area higher in primary than in logged forest?",
                         "Not normal: Mann-Whitney U test.")
            else rec("Mann-Whitney U test (Wilcoxon rank-sum)", "4. Compare groups",
                     "Rank-based comparison of two independent groups; robust to skew and outliers.",
                     "wilcox.test(y ~ group, data = d)",
                     "Do burned and unburned plots differ in seedling counts?")
          }
        } else {
          req(input$factors)
          if (input$factors == "2")
            return(rec("Two-way ANOVA", "4. Compare groups",
                       "Tests the effect of two factors and their interaction (does the effect of one factor depend on the other?). If residuals are not normal, transform the response or use a GLM with both factors as predictors.",
                       "car::Anova(aov(y ~ A * B, data = d), type = 2)",
                       "Does basal area depend on forest type, fire history, and their combination?"))
          if (par) rec("One-way ANOVA + Tukey HSD", "4. Compare groups",
                       "Tests whether at least one group mean differs; Tukey's post-hoc test then shows which pairs differ. Use Welch's ANOVA if variances are unequal.",
                       "fit <- aov(y ~ group, data = d); summary(fit); TukeyHSD(fit)",
                       "Does basal area differ among primary, logged, agroforest and plantation plots?",
                       "Not normal: Kruskal-Wallis test.")
          else rec("Kruskal-Wallis test + pairwise Wilcoxon", "4. Compare groups",
                   "Rank-based alternative to one-way ANOVA.",
                   "kruskal.test(y ~ group, data = d); pairwise.wilcox.test(d$y, d$group, p.adjust.method = 'holm')",
                   "Do seedling counts differ among four forest types?")
        }
      } else if (goal == "relate") {
        req(input$var_types)
        if (input$var_types == "cc")
          return(rec("Chi-square test of independence", "5. Correlation > Chi-square",
                     "Tests whether two categorical variables are related, using a table of counts. Use Fisher's exact test when expected counts are below 5.",
                     "chisq.test(table(d$A, d$B))",
                     "Is fire history (burned / unburned) related to forest type?"))
        if (input$var_types == "nc")
          return(rec("That is a group comparison", "4. Compare groups",
                     "With one numeric and one categorical variable the question becomes: does the numeric variable differ between the groups? Choose 'Compare groups' above.",
                     "t.test(y ~ group) / aov(y ~ group)",
                     "Does canopy cover differ between forest types?"))
        req(input$linear)
        if (input$linear == "unknown")
          return(rec("Plot the data first", "2. Explore, then 5. Correlation",
                     "Make a scatter plot. A straight cloud of points and normal variables -> Pearson. A curved but consistently rising or falling pattern, skew or outliers -> Spearman.",
                     "plot(x, y); shapiro.test(x); shapiro.test(y)",
                     "Biomass against basal area rises in a curve: Spearman, or Pearson after log-transforming biomass."))
        if (input$linear == "yes") rec("Pearson correlation", "5. Correlation > Correlation",
                                       "Measures the strength of a linear relationship between two roughly normal variables (r from -1 to +1).",
                                       "cor.test(x, y, method = 'pearson')",
                                       "Is soil carbon correlated with soil nitrogen?",
                                       "Skewed, curved or with outliers: Spearman.")
        else rec("Spearman rank correlation", "5. Correlation > Correlation",
                 "Correlation of the ranks: detects any monotonic relationship and is robust to skew and outliers. Kendall's tau is an alternative for small samples with many ties.",
                 "cor.test(x, y, method = 'spearman')",
                 "Is above-ground biomass (skewed) related to basal area?")
      } else if (goal == "predict") {
        req(input$resp_type)
        switch(
          input$resp_type,
          cont = rec("Linear regression (LM)", "6. Regression > Linear / Multiple / Polynomial",
                     "Models a continuous response as a straight-line (or polynomial) function of one or more predictors. Check that residuals are normal with constant variance.",
                     "lm(y ~ x1 + x2, data = d)",
                     "How does litter depth change with canopy cover? Hump-shaped responses (understory cover vs canopy) need a polynomial term."),
          count = rec("Poisson GLM (negative binomial if overdispersed)", "6. Regression > GLM > Poisson",
                      "Counts are non-negative integers whose variance grows with the mean, so a log-link GLM fits better than a linear model. If the dispersion statistic is well above 1, switch to negative binomial.",
                      "glm(y ~ x, family = poisson, data = d)   # MASS::glm.nb(y ~ x, data = d)",
                      "Does tree species richness decline with elevation and differ among forest types?"),
          binary = rec("Binomial GLM (logistic regression)", "6. Regression > GLM > Binomial",
                       "Models the probability of a 0/1 outcome on the logit scale.",
                       "glm(y ~ x, family = binomial, data = d)",
                       "Does the probability of hornbill presence increase with basal area?"),
          skew = rec("Gamma GLM with log link, or log-transform + LM", "6. Regression > GLM > Gamma (or 3. Transform first)",
                     "Positive right-skewed responses violate the normal-residual assumption. Either model them directly with a Gamma GLM or log-transform the response and fit a linear model.",
                     "glm(y ~ x, family = Gamma(link = 'log'), data = d)   # or lm(log(y) ~ x)",
                     "How does above-ground biomass increase with basal area?"),
          prop = rec("Logit-transform + LM (or binomial GLM for k-out-of-n counts)", "3. Transform, then 6. Regression",
                     "Percentages are bounded between 0 and 100, so residuals are rarely normal near the limits. Logit-transform continuous proportions; if the proportion comes from counts (e.g. 12 of 40 seedlings survived), use a binomial GLM.",
                     "lm(qlogis(p) ~ x, data = d)   # glm(cbind(k, n - k) ~ x, family = binomial)",
                     "How does understory cover (%) respond to canopy openness?")
        )
      } else {
        req(input$multi_type)
        switch(
          input$multi_type,
          env = rec("Principal Component Analysis (PCA)", "7. Multivariate > PCA",
                    "Combines many correlated continuous variables into a few uncorrelated axes that capture most of the variation. Standardise the variables when their units differ.",
                    "prcomp(d[, vars], scale. = TRUE)",
                    "Summarise soil pH, C, N, litter depth, canopy cover and basal area into one or two environmental gradients."),
          comm = rec("NMDS (Bray-Curtis) + PERMANOVA", "7. Multivariate > NMDS + PERMANOVA",
                     "Species abundance tables contain many zeros and are not linear, so PCA is a poor choice. NMDS maps plots by compositional dissimilarity; PERMANOVA tests whether composition differs between groups.",
                     "vegan::metaMDS(comm, distance = 'bray'); vegan::adonis2(comm ~ group, data = env)",
                     "Does tree species composition differ among primary, logged, agroforest and plantation plots?"),
          cluster = rec("Hierarchical cluster analysis", "7. Multivariate > Cluster analysis",
                        "Groups plots by similarity and shows the result as a dendrogram. Use Euclidean distance on standardised environmental variables, or Bray-Curtis for species data.",
                        "hclust(dist(scale(d[, vars])), method = 'ward.D2')",
                        "Which plots form natural vegetation types based on their species composition?")
        )
      }
    })

    output$recommendation <- renderUI({
      r <- recommendation()
      tagList(
        tags$h4(style = paste0("color:", eco_green, ";"), icon("check-circle"), " ", r$method),
        tags$p(r$why),
        tags$p(tags$b("Where in this app: "), tags$span(class = "badge bg-success", r$tab)),
        tags$p(tags$b("Ecological example: "), tags$em(r$example)),
        tags$p(tags$b("In R:")),
        tags$pre(class = "bg-light p-2 border rounded", style = "white-space: pre-wrap;", r$code),
        if (!is.null(r$alt)) tags$p(class = "text-muted", tags$b("If the assumption fails: "), r$alt),
        if (input$goal == "predict" && identical(input$grouped, "yes")) tags$div(
          class = "alert alert-warning py-2 px-3", style = "font-size: 0.9rem;",
          tags$b("Add a random factor (mixed model). "),
          "Grouped or repeated observations are not independent. In 6. Regression choose the grouping variable ",
          "(site, block, plot ID, year) under 'Random factor(s)': the app then fits a mixed model with a random intercept, ",
          "e.g. ", tags$code("lme4::lmer(y ~ x + (1 | site))"), " or ", tags$code("lme4::glmer(y ~ x + (1 | site), family = poisson)"), ".")
      )
    })
  })
}

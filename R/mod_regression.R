# =============================================================================
# mod_regression.R - Step 6: regression (LM, polynomial, GLM) with optional
#                    random factors (mixed models: LMM / GLMM via lme4)
# =============================================================================

regressionUI <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("tabs"),
    title = "Regression",

    # -- Model setup -----------------------------------------------------------
    nav_panel(
      "Model setup", value = "setup",
      layout_columns(
        col_widths = c(4, 8),
        tags$div(
          selectInput(ns("model_type"), "Analysis type",
                      choices = c(
                        "Simple Linear Regression"       = "lm_simple",
                        "Multiple Linear Regression"     = "lm_multiple",
                        "Polynomial Regression"          = "lm_poly",
                        "GLM (Generalised Linear Model)" = "glm"
                      )),
          selectInput(ns("resp_var"), "Response variable (Y)", choices = NULL),
          selectizeInput(ns("pred_vars"), "Predictor variable(s) (X)",
                         choices = NULL, multiple = TRUE),
          selectizeInput(ns("rand_vars"), "Random factor(s) - optional",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "none (ordinary fixed-effects model)")),
          tags$small(class = "text-muted d-block mb-3",
                     "Grouping variables such as site, block, transect, year or observer. ",
                     "Observations in the same group are not independent; choosing a random ",
                     "factor adds a random intercept (1 | factor) and fits a mixed model ",
                     "(LMM / GLMM, package lme4)."),
          conditionalPanel(
            condition = "input.model_type == 'glm'", ns = ns,
            selectInput(ns("glm_family"), "GLM family", choices = family_choices,
                        selected = "poisson"),
            conditionalPanel(
              condition = "input.glm_family != 'negbin'", ns = ns,
              selectInput(ns("glm_link"), "Link function",
                          choices = c("default", "log", "logit", "probit",
                                      "inverse", "identity"))
            )
          ),
          conditionalPanel(
            condition = "input.model_type == 'lm_poly'", ns = ns,
            sliderInput(ns("poly_degree"), "Polynomial degree", 2, 5, 2, step = 1)
          ),
          conditionalPanel(
            condition = "input.model_type != 'lm_poly' && input.model_type != 'lm_simple'", ns = ns,
            checkboxInput(ns("include_interaction"), "Include 2-way interactions", FALSE)
          ),
          checkboxInput(ns("scale_predictors"), "Standardise numeric predictors (mean 0, SD 1)", FALSE),
          hr(),
          actionButton(ns("fit_model"), "Fit Model",
                       class = "btn-success btn-lg w-100", icon = icon("play"))
        ),
        tagList(
          h6("Model formula"),
          verbatimTextOutput(ns("formula_preview")),
          h6("Variable summary"),
          DTOutput(ns("var_summary_table")),
          help_note(
            tags$b("Choosing the model: "),
            "continuous response -> linear model; counts (stems, species) -> Poisson, ",
            "or negative binomial if overdispersed; presence/absence -> binomial; ",
            "positive right-skewed (biomass) -> Gamma with log link, or log-transform ",
            "the response and use a linear model. Categorical predictors (e.g. forest ",
            "type) are allowed and are dummy-coded automatically."),
          help_note(
            tags$b("Fixed or random? "),
            "Use a ", tags$b("fixed"), " predictor for the effects you want to estimate and ",
            "compare (forest type, elevation). Use a ", tags$b("random factor"), " for grouping ",
            "that is a sample from a larger population and creates non-independence ",
            "(sites, blocks, repeated plots) - you account for its variation without ",
            "estimating each level. Aim for at least 5 levels in a random factor.")
        )
      )
    ),

    # -- Results ---------------------------------------------------------------
    nav_panel(
      "Results", value = "results",
      uiOutput(ns("no_model_1")),
      verbatimTextOutput(ns("model_summary")),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Coefficient table"), DTOutput(ns("coef_table"))),
        card(card_header("Model fit statistics"),
             DTOutput(ns("fit_stats_table")),
             uiOutput(ns("dispersion_note")))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Term-level tests (Type II ANOVA / likelihood-ratio)"),
             DTOutput(ns("anova_table")),
             tags$small(class = "text-muted",
                        "One test per predictor - useful for categorical predictors with several levels.")),
        card(card_header("Multicollinearity check (VIF)"),
             DTOutput(ns("vif_table")),
             tags$small(class = "text-muted",
                        "VIF > 5 = potential concern. VIF > 10 = serious collinearity."))
      ),
      uiOutput(ns("random_card"))
    ),

    # -- Diagnostics -----------------------------------------------------------
    nav_panel(
      "Diagnostics", value = "diagnostics",
      uiOutput(ns("diag_note")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Q-Q Plot of Residuals"),
             plotlyOutput(ns("qq_plot"), height = "360px"),
             verbatimTextOutput(ns("shapiro_result"))),
        card(card_header("Residuals vs Fitted"),
             plotlyOutput(ns("resid_fitted"), height = "360px")),
        card(card_header("Residuals vs Predictor"),
             plotlyOutput(ns("resid_predictor"), height = "360px")),
        card(card_header("Scale-Location"),
             plotlyOutput(ns("scale_location"), height = "360px"))
      )
    ),

    # -- ROC / AUC (binary models) -----------------------------------------------
    nav_panel(
      "ROC / AUC", value = "roc",
      uiOutput(ns("roc_note")),
      uiOutput(ns("roc_body"))
    ),

    # -- Visualise -------------------------------------------------------------
    nav_panel(
      "Visualise", value = "visualise",
      layout_columns(
        col_widths = c(3, 9),
        tags$div(
          checkboxInput(ns("show_ci"), "Show confidence interval", TRUE),
          sliderInput(ns("ci_level"), "Confidence level", 0.80, 0.99, 0.95, 0.01),
          checkboxInput(ns("show_points"), "Show data points", TRUE),
          selectInput(ns("vis_x_var"), "X-axis variable", choices = NULL),
          tags$small(class = "text-muted",
                     "With several predictors the line shows the effect of the chosen ",
                     "variable while the others are held constant (see the plot title), ",
                     "so it will not pass through the middle of all points."),
          hr(),
          downloadButton(ns("download_plot"), "Download Plot",
                         class = "btn-outline-success w-100")
        ),
        plotlyOutput(ns("regression_plot"), height = "500px")
      )
    )
  )
}

regressionServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    m <- reactiveValues(model = NULL, data = NULL, raw = NULL, scaling = NULL, spec = NULL)

    observe({
      df <- rv$data
      req(df)
      cols <- names(df)
      updateSelectInput(session, "resp_var", choices = cols,
                        selected = if (isTRUE(isolate(input$resp_var) %in% cols)) isolate(input$resp_var) else NULL)
      updateSelectizeInput(session, "pred_vars", choices = cols,
                           selected = intersect(isolate(input$pred_vars), cols))
      rand_ch <- union(cat_cols(df), cols[!vapply(df, is.numeric, logical(1))])
      updateSelectizeInput(session, "rand_vars", choices = rand_ch,
                           selected = intersect(isolate(input$rand_vars), rand_ch))
    })

    # a new dataset invalidates the fitted model
    observeEvent(rv$name, { m$model <- NULL })

    # -- Formula (backtick-safe) -----------------------------------------------
    model_formula_text <- reactive({
      req(input$resp_var, input$pred_vars)
      y  <- bt(input$resp_var)
      xs <- setdiff(input$pred_vars, input$resp_var)
      req(length(xs) >= 1)
      if (input$model_type == "lm_poly") {
        x1 <- bt(xs[1])
        terms <- paste(c(x1, sprintf("I(%s^%d)", x1, 2:input$poly_degree)), collapse = " + ")
      } else {
        if (input$model_type == "lm_simple") xs <- xs[1]
        terms <- paste(bt(xs), collapse = " + ")
        if (isTRUE(input$include_interaction) && length(xs) >= 2 && input$model_type != "lm_simple") {
          combos <- combn(bt(xs), 2, FUN = function(pair) paste(pair, collapse = ":"))
          terms <- paste(c(terms, combos), collapse = " + ")
        }
      }
      rands <- setdiff(input$rand_vars, c(input$resp_var, xs))
      if (length(rands)) terms <- paste(c(terms, sprintf("(1 | %s)", bt(rands))), collapse = " + ")
      paste(y, "~", terms)
    })

    output$formula_preview <- renderPrint({
      f <- tryCatch(model_formula_text(), error = function(e) NULL)
      if (is.null(f)) {
        cat("Select a response and at least one predictor.")
        return(invisible())
      }
      cat(f, "\n")
      if (input$model_type == "glm") cat("family =", input$glm_family, "\n")
      if (input$model_type %in% c("lm_simple", "lm_poly") && length(input$pred_vars) > 1)
        cat("(only the first predictor is used for this analysis type)\n")
      if (isTRUE(input$scale_predictors)) cat("(numeric predictors standardised before fitting)\n")
      if (length(setdiff(input$rand_vars, c(input$resp_var, input$pred_vars))))
        cat("(1 | factor) = random intercept -> mixed model fitted with lme4\n")
    })

    output$var_summary_table <- renderDT({
      req(rv$data, input$pred_vars, input$resp_var)
      vars <- unique(c(input$resp_var, input$pred_vars))
      df   <- as.data.frame(rv$data, check.names = FALSE)
      req(all(vars %in% names(df)))
      df <- df[, vars, drop = FALSE]
      summ <- data.frame(
        Variable = vars,
        Role     = c("Response", rep("Predictor", length(vars) - 1)),
        Type     = vapply(df, function(x) if (is.numeric(x)) "numeric" else "categorical", ""),
        N        = vapply(df, function(x) sum(!is.na(x)), 0L),
        Missing  = vapply(df, function(x) sum(is.na(x)), 0L),
        Mean     = vapply(df, function(x) if (is.numeric(x)) round(mean(x, na.rm = TRUE), 3) else NA_real_, 0),
        SD       = vapply(df, function(x) if (is.numeric(x)) round(sd(x, na.rm = TRUE), 3) else NA_real_, 0)
      )
      simple_dt(summ)
    })

    # -- Fit model ---------------------------------------------------------------
    observeEvent(input$fit_model, {
      f_txt <- tryCatch(model_formula_text(), error = function(e) NULL)
      if (is.null(rv$data) || is.null(f_txt)) {
        showNotification("Load data and select a response and predictor(s) first.", type = "error")
        return()
      }
      type  <- input$model_type
      resp  <- input$resp_var
      preds <- setdiff(input$pred_vars, resp)
      if (type %in% c("lm_simple", "lm_poly")) preds <- preds[1]
      fam   <- if (type == "glm") input$glm_family else "gaussian"

      rands <- setdiff(input$rand_vars, c(resp, preds))
      mixed <- length(rands) > 0
      if (mixed && !requireNamespace("lme4", quietly = TRUE)) {
        showNotification("Random factors need the 'lme4' package: install.packages('lme4')",
                         type = "error", duration = 10)
        return()
      }

      df <- as.data.frame(rv$data, check.names = FALSE)
      df <- df[complete.cases(df[, c(resp, preds, rands), drop = FALSE]),
               c(resp, preds, rands), drop = FALSE]
      msg <- NULL
      lv01 <- c("0", "1")
      for (r in rands) {
        df[[r]] <- factor(df[[r]])
        if (nlevels(df[[r]]) < 2 || nlevels(df[[r]]) >= nrow(df)) {
          showNotification(sprintf("'%s' cannot be a random factor: it needs at least 2 groups and several observations per group.", r),
                           type = "error", duration = 10)
          return()
        }
        if (nlevels(df[[r]]) < 5)
          showNotification(sprintf("'%s' has only %d levels - variance of a random factor is poorly estimated with fewer than 5. Consider using it as a fixed predictor.",
                                   r, nlevels(df[[r]])), type = "warning", duration = 10)
      }

      # response
      if (!is.numeric(df[[resp]])) {
        lv <- levels(factor(df[[resp]]))
        if (fam == "binomial" && length(lv) == 2) {
          df[[resp]] <- as.numeric(factor(df[[resp]])) - 1
          lv01 <- lv
          msg <- sprintf("Response coded as 0 = '%s', 1 = '%s'.", lv[1], lv[2])
        } else {
          showNotification("The response must be numeric (or have exactly two categories for a binomial GLM).",
                           type = "error", duration = 8)
          return()
        }
      }
      # predictors
      for (v in preds) if (!is.numeric(df[[v]])) df[[v]] <- factor(df[[v]])
      if (type == "lm_poly" && !is.numeric(df[[preds[1]]])) {
        showNotification("Polynomial regression needs a numeric predictor.", type = "error")
        return()
      }
      raw <- df
      scaling <- list()
      if (isTRUE(input$scale_predictors)) {
        for (v in preds) if (is.numeric(df[[v]])) {
          scaling[[v]] <- c(mean = mean(df[[v]]), sd = sd(df[[v]]))
          df[[v]] <- (df[[v]] - scaling[[v]]["mean"]) / scaling[[v]]["sd"]
        }
      }

      f <- as.formula(f_txt)
      do_fit <- function() {
        if (mixed) {
          lmm <- function() if (requireNamespace("lmerTest", quietly = TRUE))
            lmerTest::lmer(f, data = df) else lme4::lmer(f, data = df)
          lnk <- input$glm_link
          if (type != "glm" || (fam == "gaussian" && (is.null(lnk) || lnk %in% c("default", "identity")))) {
            lmm()
          } else if (fam == "negbin") {
            lme4::glmer.nb(f, data = df)
          } else {
            fam_obj <- if (is.null(lnk) || lnk == "default") do.call(fam, list()) else
              do.call(fam, list(link = lnk))
            lme4::glmer(f, data = df, family = fam_obj)
          }
        } else if (type != "glm") {
          lm(f, data = df)
        } else if (fam == "negbin") {
          MASS::glm.nb(f, data = df)
        } else {
          lnk <- input$glm_link
          fam_obj <- if (is.null(lnk) || lnk == "default") do.call(fam, list()) else
            do.call(fam, list(link = lnk))
          glm(f, data = df, family = fam_obj)
        }
      }
      warns <- character()
      fit <- tryCatch(
        withCallingHandlers(do_fit(), warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }),
        error = function(e) {
          showNotification(paste("Model error:", conditionMessage(e)), type = "error", duration = 10)
          NULL
        })
      if (length(warns))
        showNotification(paste("Warning while fitting:", unique(warns)[1]),
                         type = "warning", duration = 10)
      if (is.null(fit)) return()

      m$model   <- fit
      m$data    <- df
      m$raw     <- raw
      m$scaling <- scaling
      m$spec    <- list(resp = resp, preds = preds, type = type, family = fam,
                        formula = f_txt, is_glm = type == "glm",
                        rands = rands, mixed = mixed, lv01 = lv01)
      num_preds <- preds[vapply(raw[preds], is.numeric, logical(1))]
      updateSelectInput(session, "vis_x_var", choices = preds,
                        selected = if (length(num_preds)) num_preds[1] else preds[1])
      showNotification(paste("Model fitted successfully!", msg,
                             if (type == "glm" && fam == "binomial") "See the ROC / AUC tab for model discrimination." else ""),
                       type = "message")
      nav_select("tabs", "results", session = session)
    })

    need_model <- function() validate(need(!is.null(m$model),
      "No model yet - choose variables in 'Model setup' and click Fit Model."))

    output$no_model_1 <- renderUI({
      if (is.null(m$model)) help_note("No model yet - choose variables in 'Model setup' and click Fit Model.")
    })

    # -- Results -----------------------------------------------------------------
    output$model_summary <- renderPrint({
      req(m$model)
      summary(m$model)
    })

    output$coef_table <- renderDT({
      need_model()
      if (isTRUE(m$spec$mixed)) {
        cs <- as.data.frame(coef(summary(m$model)))
        stat_col <- grep("value$", names(cs), value = TRUE)[1]
        p_col    <- grep("^Pr", names(cs), value = TRUE)
        ci <- tryCatch(suppressMessages(confint(m$model, method = "Wald", parm = "beta_")),
                       error = function(e) matrix(NA_real_, nrow(cs), 2))
        tbl <- data.frame(
          term      = rownames(cs),
          estimate  = cs$Estimate,
          std.error = cs$`Std. Error`,
          statistic = cs[[stat_col]],
          # lme4::lmer gives no p-values; fall back to a normal approximation
          p.value   = if (length(p_col)) cs[[p_col]] else 2 * pnorm(-abs(cs[[stat_col]])),
          conf.low  = ci[, 1], conf.high = ci[, 2])
      } else {
        tbl <- tryCatch(suppressMessages(suppressWarnings(broom::tidy(m$model, conf.int = TRUE))),
                        error = function(e) broom::tidy(m$model))
      }
      tbl <- round_df(as.data.frame(tbl), 5)
      simple_dt(tbl) |>
        formatStyle("p.value",
                    backgroundColor = styleInterval(
                      c(0.001, 0.01, 0.05),
                      c("#c6efce", "#c6efce", "#ffeb9c", "#ffc7ce")))
    })

    dispersion <- reactive({
      req(m$model, m$spec$family %in% c("poisson", "negbin"))
      sum(residuals(m$model, type = "pearson")^2) / df.residual(m$model)
    })

    output$fit_stats_table <- renderDT({
      need_model()
      mod <- m$model
      s   <- summary(mod)
      if (isTRUE(m$spec$mixed)) {
        stats_list <- list(
          "Model"   = if (lme4::isGLMM(mod)) "Generalised linear mixed model (GLMM)" else
            "Linear mixed model (LMM, REML)",
          "AIC"     = round(AIC(mod), 2),
          "BIC"     = round(BIC(mod), 2),
          "logLik"  = round(as.numeric(logLik(mod)), 2),
          "N"       = nobs(mod)
        )
        for (r in m$spec$rands)
          stats_list[[paste0("Groups in ", r)]] <- nlevels(m$data[[r]])
        if (!lme4::isGLMM(mod)) {
          vc  <- as.data.frame(lme4::VarCorr(mod))
          v_r <- sum(vc$vcov[vc$grp != "Residual"]); v_e <- vc$vcov[vc$grp == "Residual"]
          v_f <- var(as.numeric(lme4::getME(mod, "X") %*% lme4::fixef(mod)))
          stats_list[["Marginal R-sq (fixed effects only)"]]    <- round(v_f / (v_f + v_r + v_e), 4)
          stats_list[["Conditional R-sq (fixed + random)"]]     <- round((v_f + v_r) / (v_f + v_r + v_e), 4)
          stats_list[["Residual SD"]] <- round(sqrt(v_e), 4)
        }
        if (m$spec$family %in% c("poisson", "negbin"))
          stats_list[["Dispersion (Pearson chi-sq / df)"]] <- round(dispersion(), 3)
      } else if (!inherits(mod, "glm")) {
        stats_list <- list(
          "R-squared"         = round(s$r.squared, 4),
          "Adjusted R-sq"     = round(s$adj.r.squared, 4),
          "F-statistic"       = round(s$fstatistic[1], 3),
          "p-value (F)"       = fmt_p(pf(s$fstatistic[1], s$fstatistic[2],
                                         s$fstatistic[3], lower.tail = FALSE)),
          "Residual SE"       = round(s$sigma, 4),
          "df (residual)"     = s$df[2],
          "AIC"               = round(AIC(mod), 2),
          "BIC"               = round(BIC(mod), 2),
          "N"                 = nobs(mod)
        )
      } else {
        stats_list <- list(
          "Null deviance"      = round(mod$null.deviance, 3),
          "Residual deviance"  = round(mod$deviance, 3),
          "Deviance explained (pseudo R-sq)" = round(1 - mod$deviance / mod$null.deviance, 4),
          "AIC"                = round(AIC(mod), 2),
          "BIC"                = round(BIC(mod), 2),
          "df (residual)"      = mod$df.residual,
          "N"                  = nobs(mod)
        )
        if (m$spec$family %in% c("poisson", "negbin"))
          stats_list[["Dispersion (Pearson chi-sq / df)"]] <- round(dispersion(), 3)
        if (m$spec$family == "negbin")
          stats_list[["Theta (negative binomial)"]] <- round(mod$theta, 3)
      }
      if (isTRUE(is_binary())) {
        y <- m$data[[m$spec$resp]]
        if (all(y %in% c(0, 1)) && length(unique(y)) == 2)
          stats_list[["AUC (see the ROC / AUC tab)"]] <- round(auc_value(y, as.numeric(fitted(mod))), 3)
      }
      simple_dt(data.frame(Metric = names(stats_list),
                           Value  = as.character(unlist(stats_list))))
    })

    output$dispersion_note <- renderUI({
      req(m$model)
      if (!identical(m$spec$family, "poisson")) return(NULL)
      d <- dispersion()
      if (d > 1.5) {
        tags$div(class = "alert alert-warning py-2 px-3 mt-2 mb-0", style = "font-size: 0.88rem;",
                 sprintf("Dispersion = %.2f (> 1.5): the counts are overdispersed, so Poisson standard errors and p-values are too optimistic. Refit with the Negative binomial family%s.", d,
                         if (isTRUE(m$spec$mixed)) "" else ", or add a random factor if the plots are grouped"))
      } else {
        help_note(sprintf("Dispersion = %.2f: close to 1, so the Poisson assumption (variance = mean) looks reasonable.", d))
      }
    })

    output$anova_table <- renderDT({
      need_model()
      at <- tryCatch(as.data.frame(car::Anova(m$model, type = 2)), error = function(e) NULL)
      validate(need(!is.null(at), "Not available for this model."))
      at <- cbind(Term = rownames(at), round_df(at, 4))
      simple_dt(at)
    })

    output$vif_table <- renderDT({
      need_model()
      validate(need(length(m$spec$preds) >= 2 && m$spec$type != "lm_poly",
                    "VIF needs a model with two or more predictors."))
      v <- tryCatch(car::vif(m$model), error = function(e) NULL)
      validate(need(!is.null(v), "VIF not available for this model (e.g. aliased terms)."))
      if (is.matrix(v)) {
        tbl <- data.frame(Term = rownames(v), VIF = round(v[, ncol(v)]^2, 3))
        names(tbl)[2] <- "VIF (GVIF^(1/Df), squared)"
      } else {
        tbl <- data.frame(Term = names(v), VIF = round(v, 3))
      }
      simple_dt(tbl) |>
        formatStyle(names(tbl)[2],
                    backgroundColor = styleInterval(c(5, 10), c("#c6efce", "#ffeb9c", "#ffc7ce")))
    })

    output$random_card <- renderUI({
      req(m$model, isTRUE(m$spec$mixed))
      ns <- session$ns
      card(card_header("Random effects (variance components)"),
           DTOutput(ns("random_table")),
           uiOutput(ns("random_note")))
    })

    output$random_table <- renderDT({
      req(m$model, isTRUE(m$spec$mixed))
      vc <- as.data.frame(lme4::VarCorr(m$model))
      tbl <- data.frame(Group = vc$grp, Effect = ifelse(is.na(vc$var1), "", vc$var1),
                        Variance = vc$vcov, SD = vc$sdcor)
      if (!lme4::isGLMM(m$model)) tbl$`Share of variance` <- tbl$Variance / sum(tbl$Variance)
      simple_dt(round_df(tbl, 4))
    })

    output$random_note <- renderUI({
      req(m$model, isTRUE(m$spec$mixed))
      mod <- m$model
      if (isTRUE(lme4::isSingular(mod)))
        return(tags$div(class = "alert alert-warning py-2 px-3 mt-2 mb-0", style = "font-size: 0.88rem;",
                        "Singular fit: a random-effect variance is estimated as (near) zero. The random factor explains no extra variation here - the ordinary model without it is adequate."))
      if (lme4::isGLMM(mod))
        return(help_note("Variances are on the link scale (log for counts, logit for presence/absence). A variance clearly above zero means the groups differ more than expected by chance, so ignoring the grouping would overstate your confidence."))
      vc  <- as.data.frame(lme4::VarCorr(mod))
      icc <- vc$vcov[vc$grp != "Residual"] / sum(vc$vcov)
      help_note(tags$b("In words: "),
                paste(sprintf("%.0f%% of the variation left after the fixed effects lies between the levels of %s (intraclass correlation = %.2f).",
                              100 * icc, vc$grp[vc$grp != "Residual"], icc), collapse = " "),
                " The fixed-effect estimates and prediction plot describe an average group (random effects set to zero).")
    })

    # -- Diagnostics -------------------------------------------------------------
    output$diag_note <- renderUI({
      if (is.null(m$model)) return(help_note("No model yet - choose variables in 'Model setup' and click Fit Model."))
      if (isTRUE(m$spec$mixed) && !lme4::isGLMM(m$model)) {
        help_note("Mixed model: residuals are conditional on the random effects (what is left after both the fixed effects and the group effects). They should be normal with constant spread, exactly as in an ordinary linear model.")
      } else if (m$spec$is_glm && m$spec$family != "gaussian") {
        help_note("GLM: the plots use deviance residuals. They are not expected to be exactly normal for Poisson / binomial models, so focus on Residuals vs Fitted (no trend, no funnel) and on the dispersion statistic in Results rather than on Shapiro-Wilk.")
      }
    })

    output$qq_plot <- renderPlotly({
      need_model()
      resids  <- residuals(m$model)
      qq_vals <- qqnorm(resids, plot.it = FALSE)
      qq_data <- data.frame(theoretical = qq_vals$x, sample = qq_vals$y)
      p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
        geom_point(colour = eco_green, alpha = 0.7) +
        geom_abline(slope = sd(resids), intercept = mean(resids),
                    colour = eco_red, linetype = "dashed") +
        labs(title = "Normal Q-Q Plot",
             x = "Theoretical Quantiles", y = "Sample Quantiles") +
        theme_minimal()
      ggplotly(p)
    })

    output$shapiro_result <- renderPrint({
      req(m$model)
      sw <- safe_shapiro(residuals(m$model))
      cat("Shapiro-Wilk normality test of residuals\n")
      if (is.na(sw$p.value)) {
        cat("Not available.\n")
        return(invisible())
      }
      cat("W =", round(sw$statistic, 4), "  p-value =", fmt_p(sw$p.value), "\n")
      if (sw$p.value < 0.05) {
        cat("=> Residuals deviate significantly from normality (p < 0.05).\n")
        if (!m$spec$is_glm) cat("   Try transforming the response (Transform tab) or a GLM.\n")
      } else {
        cat("=> No significant departure from normality (p >= 0.05).\n")
      }
    })

    output$resid_fitted <- renderPlotly({
      need_model()
      df <- data.frame(fitted = fitted(m$model), residuals = residuals(m$model))
      p <- ggplot(df, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6, colour = eco_green) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = eco_red) +
        geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                    colour = eco_orange, linewidth = 0.7) +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        theme_minimal()
      ggplotly(p)
    })

    output$resid_predictor <- renderPlotly({
      need_model()
      pred1   <- m$spec$preds[1]
      plot_df <- data.frame(x = m$raw[[pred1]], residuals = residuals(m$model))
      p <- ggplot(plot_df, aes(x = x, y = residuals))
      if (is.numeric(plot_df$x)) {
        p <- p + geom_point(alpha = 0.6, colour = eco_green) +
          geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                      colour = eco_orange, linewidth = 0.7)
      } else {
        p <- p + geom_boxplot(fill = eco_green, alpha = 0.5)
      }
      p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = eco_red) +
        labs(title = paste("Residuals vs", pred1), x = pred1, y = "Residuals") +
        theme_minimal()
      ggplotly(p)
    })

    output$scale_location <- renderPlotly({
      need_model()
      std_res <- if (isTRUE(m$spec$mixed)) {
        r <- residuals(m$model, type = "pearson"); r / sd(r)
      } else rstandard(m$model)
      df <- data.frame(fitted = fitted(m$model),
                       sqrt_abs_resid = sqrt(abs(std_res)))
      df <- df[is.finite(df$sqrt_abs_resid), ]
      p <- ggplot(df, aes(x = fitted, y = sqrt_abs_resid)) +
        geom_point(alpha = 0.6, colour = eco_green) +
        geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                    colour = eco_orange, linewidth = 0.7) +
        labs(title = "Scale-Location", x = "Fitted Values",
             y = "sqrt(|Std. Residuals|)") +
        theme_minimal()
      ggplotly(p)
    })

    # -- ROC / AUC -----------------------------------------------------------------
    is_binary <- reactive({
      !is.null(m$model) && isTRUE(m$spec$is_glm) && identical(m$spec$family, "binomial")
    })

    # The ROC / AUC sub-tab only exists for logistic (binomial) models: it is
    # hidden at start-up and whenever the fitted model is not binomial.
    observe({
      if (isTRUE(is_binary())) nav_show("tabs", target = "roc", session = session)
      else nav_hide("tabs", target = "roc", session = session)
    })

    output$roc_note <- renderUI(NULL)

    output$roc_body <- renderUI({
      req(is_binary())
      ns <- session$ns
      tagList(
        if (isTRUE(m$spec$mixed))
          radioButtons(ns("roc_re"), "Predictions used for the ROC curve",
                       choices = c("Including random effects (conditional)" = "cond",
                                   "Fixed effects only (population level - for new sites)" = "fixed"),
                       inline = TRUE),
        layout_columns(
          col_widths = c(7, 5),
          card(card_header("ROC curve"),
               plotlyOutput(ns("roc_plot"), height = "520px"),
               downloadButton(ns("download_roc"), "Download ROC plot",
                              class = "btn-outline-success")),
          tags$div(
            card(card_header("Discrimination (AUC)"), fill = FALSE,
                 DTOutput(ns("auc_table"), fill = FALSE),
                 uiOutput(ns("auc_interp"))),
            card(card_header("Classification at a probability threshold"), fill = FALSE,
                 sliderInput(ns("roc_thr"), sprintf("Threshold: predict '%s' when probability >=", m$spec$lv01[2]),
                             min = 0.01, max = 0.99, value = 0.5, step = 0.01),
                 actionLink(ns("roc_opt"), "Use the optimal threshold (maximum TSS / Youden's J)"),
                 DTOutput(ns("conf_mat"), fill = FALSE),
                 DTOutput(ns("class_table"), fill = FALSE))
          )
        )
      )
    })

    roc_data <- reactive({
      req(is_binary())
      y <- m$data[[m$spec$resp]]
      validate(need(all(y %in% c(0, 1)), "The response must be coded 0/1."),
               need(length(unique(y)) == 2, "The response needs both 0s and 1s."))
      fixed_only <- isTRUE(m$spec$mixed) && identical(input$roc_re, "fixed")
      p <- if (fixed_only) as.numeric(predict(m$model, re.form = NA, type = "response")) else
        as.numeric(fitted(m$model))
      a  <- auc_value(y, p)
      n1 <- sum(y == 1); n0 <- sum(y == 0)
      roc <- roc_curve(y, p)
      list(y = y, p = p, roc = roc, auc = a, ci = auc_ci(a, n1, n0),
           n1 = n1, n0 = n0, opt = roc_optimal(roc), fixed_only = fixed_only)
    })

    # cross-validation refits the model k times, so compute it once per model
    cv_result <- reactive({
      req(is_binary())
      if (isTRUE(m$spec$mixed)) return(NA_real_)
      cv_auc(m$model, m$data, m$spec$resp, k = 5)
    })

    observeEvent(m$model, {
      if (isTRUE(is_binary())) updateSliderInput(session, "roc_thr", value = 0.5)
    })

    observeEvent(input$roc_opt, {
      r <- roc_data()
      updateSliderInput(session, "roc_thr", value = round(r$opt$threshold, 2))
    })

    roc_gg <- reactive({
      r   <- roc_data()
      thr <- if (is.null(input$roc_thr)) 0.5 else input$roc_thr
      cs  <- class_stats(r$y, r$p, thr)
      cur <- data.frame(fpr = 1 - cs$specificity, tpr = cs$sensitivity)
      opt <- data.frame(fpr = r$opt$fpr, tpr = r$opt$tpr)
      ggplot(r$roc, aes(x = fpr, y = tpr)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
        geom_area(fill = eco_green, alpha = 0.15) +
        geom_path(colour = eco_green, linewidth = 1.1) +
        geom_point(data = opt, colour = eco_orange, size = 4, shape = 18) +
        geom_point(data = cur, colour = eco_red, size = 3.5) +
        annotate("text", x = 0.62, y = 0.12, hjust = 0, size = 4.2,
                 label = sprintf("AUC = %.3f\n95%% CI %.3f - %.3f",
                                 r$auc, r$ci[["lower"]], r$ci[["upper"]])) +
        scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
        labs(title = sprintf("ROC curve: %s", m$spec$resp),
             subtitle = sprintf("Red dot = threshold %.2f; orange diamond = optimal threshold %.2f; dashed line = no better than chance",
                                thr, r$opt$threshold),
             x = "False positive rate (1 - specificity)",
             y = "True positive rate (sensitivity)") +
        theme_minimal()
    })

    output$roc_plot <- renderPlotly({
      p <- roc_gg()
      layout(ggplotly(p),
             title = list(text = paste0(p$labels$title, "<br><sup>", p$labels$subtitle, "</sup>")),
             margin = list(t = 70))
    })

    output$download_roc <- downloadHandler(
      filename = function() paste0("roc_curve_", Sys.Date(), ".png"),
      content  = function(file) ggsave(file, plot = roc_gg() + coord_equal(), width = 7, height = 7.4, dpi = 150, bg = "white")
    )

    output$auc_table <- renderDT({
      r  <- roc_data()
      cv <- cv_result()
      rows <- list(
        c("AUC (on the data used to fit the model)", sprintf("%.3f", r$auc)),
        c("95% confidence interval", sprintf("%.3f - %.3f", r$ci[["lower"]], r$ci[["upper"]])),
        c("5-fold cross-validated AUC",
          if (isTRUE(m$spec$mixed)) "not computed for mixed models" else
            if (is.na(cv)) "not available (too few 0s or 1s, or a factor level missing from a fold)" else
              sprintf("%.3f", cv)),
        c("Optimal threshold (max TSS)", sprintf("%.3f", r$opt$threshold)),
        c("Presences (1) / absences (0)", sprintf("%d / %d", r$n1, r$n0))
      )
      if (isTRUE(m$spec$mixed)) {
        p_fix  <- as.numeric(predict(m$model, re.form = NA, type = "response"))
        p_cond <- as.numeric(fitted(m$model))
        rows <- append(rows, list(
          c("AUC with random effects", sprintf("%.3f", auc_value(r$y, p_cond))),
          c("AUC fixed effects only",  sprintf("%.3f", auc_value(r$y, p_fix)))), after = 2)
      }
      tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      names(tbl) <- c("Measure", "Value")
      simple_dt(tbl)
    })

    output$auc_interp <- renderUI({
      r  <- roc_data()
      cv <- cv_result()
      tagList(
        help_note(
          tags$b("In words: "),
          sprintf("AUC = %.2f - discrimination is %s. If you pick at random one plot where %s = %s and one where it is %s, the model gives the first a higher predicted probability %.0f%% of the time (50%% = coin toss).",
                  r$auc, auc_label(r$auc), m$spec$resp, m$spec$lv01[2], m$spec$lv01[1], 100 * r$auc),
          if (!is.na(cv)) sprintf(" Cross-validated AUC (%.2f) estimates performance on new plots%s.",
                                  cv, if (r$auc - cv > 0.05) " - the clearly lower value suggests the model is over-fitted" else "")),
        tags$small(class = "text-muted",
                   "Guide (Hosmer & Lemeshow): 0.5 = no discrimination, 0.7-0.8 acceptable, 0.8-0.9 excellent, > 0.9 outstanding. ",
                   "AUC measures how well the model ranks presences above absences, not whether the predicted probabilities are well calibrated. ",
                   "With pseudo-absences or very rare species, report TSS and sensitivity as well.")
      )
    })

    output$conf_mat <- renderDT({
      r  <- roc_data()
      cs <- class_stats(r$y, r$p, input$roc_thr)
      l0 <- m$spec$lv01[1]; l1 <- m$spec$lv01[2]
      tbl <- data.frame(` ` = c(paste("Predicted", l1), paste("Predicted", l0)),
                        a = c(cs$TP, cs$FN), b = c(cs$FP, cs$TN), check.names = FALSE)
      names(tbl)[2:3] <- c(paste("Observed", l1), paste("Observed", l0))
      simple_dt(tbl)
    })

    output$class_table <- renderDT({
      r  <- roc_data()
      cs <- class_stats(r$y, r$p, input$roc_thr)
      tbl <- data.frame(
        Measure = c(sprintf("Sensitivity (%s correctly predicted)", m$spec$lv01[2]),
                    sprintf("Specificity (%s correctly predicted)", m$spec$lv01[1]),
                    "Accuracy (all correct)",
                    "TSS (sensitivity + specificity - 1)",
                    "Cohen's kappa"),
        Value = c(cs$sensitivity, cs$specificity, cs$accuracy, cs$TSS, cs$kappa))
      simple_dt(round_df(tbl, 3))
    })

    # -- Visualise ---------------------------------------------------------------
    # Predicted response along one predictor; the other predictors are held at
    # their mean (numeric) or most common level (categorical).
    reg_plot <- reactive({
      need_model()
      spec  <- m$spec
      raw   <- m$raw
      x_var <- input$vis_x_var
      req(x_var %in% spec$preds)
      mod   <- m$model
      level <- input$ci_level
      others <- setdiff(spec$preds, x_var)

      if (is.numeric(raw[[x_var]])) {
        newdata <- data.frame(x = seq(min(raw[[x_var]]), max(raw[[x_var]]), length.out = 200))
      } else {
        newdata <- data.frame(x = factor(levels(raw[[x_var]]), levels = levels(raw[[x_var]])))
      }
      names(newdata) <- x_var
      held <- character()
      for (v in others) {
        if (is.numeric(raw[[v]])) {
          newdata[[v]] <- mean(raw[[v]])
          held <- c(held, sprintf("%s = %.3g (mean)", v, mean(raw[[v]])))
        } else {
          top <- names(which.max(table(raw[[v]])))
          newdata[[v]] <- factor(top, levels = levels(raw[[v]]))
          held <- c(held, sprintf("%s = %s", v, top))
        }
      }
      plot_x <- newdata[[x_var]]
      for (v in names(m$scaling)) {
        newdata[[v]] <- (newdata[[v]] - m$scaling[[v]]["mean"]) / m$scaling[[v]]["sd"]
      }

      if (isTRUE(spec$mixed)) {
        # population-level prediction (random effects = 0) with Wald confidence band
        tt   <- delete.response(terms(lme4::nobars(formula(mod))))
        X    <- model.matrix(tt, newdata)
        beta <- lme4::fixef(mod)
        X    <- X[, names(beta), drop = FALSE]
        eta  <- as.numeric(X %*% beta)
        se   <- sqrt(rowSums((X %*% as.matrix(vcov(mod))) * X))
        z    <- qnorm(1 - (1 - level) / 2)
        inv  <- if (lme4::isGLMM(mod)) family(mod)$linkinv else identity
        pred <- data.frame(x = plot_x, fit = inv(eta), lwr = inv(eta - z * se), upr = inv(eta + z * se))
        lo <- pmin(pred$lwr, pred$upr); hi <- pmax(pred$lwr, pred$upr)
        pred$lwr <- lo; pred$upr <- hi
      } else if (inherits(mod, "glm")) {
        pr  <- predict(mod, newdata = newdata, type = "link", se.fit = TRUE)
        z   <- qnorm(1 - (1 - level) / 2)
        inv <- family(mod)$linkinv
        pred <- data.frame(x = plot_x, fit = inv(pr$fit),
                           lwr = inv(pr$fit - z * pr$se.fit), upr = inv(pr$fit + z * pr$se.fit))
        # for decreasing links (e.g. inverse) the bounds swap
        lo <- pmin(pred$lwr, pred$upr); hi <- pmax(pred$lwr, pred$upr)
        pred$lwr <- lo; pred$upr <- hi
      } else {
        pr <- predict(mod, newdata = newdata, interval = "confidence", level = level)
        pred <- data.frame(x = plot_x, fit = pr[, "fit"], lwr = pr[, "lwr"], upr = pr[, "upr"])
      }
      obs <- data.frame(x = raw[[x_var]], y = raw[[spec$resp]])

      p <- ggplot()
      if (is.numeric(plot_x)) {
        if (input$show_ci)
          p <- p + geom_ribbon(data = pred, aes(x = x, ymin = lwr, ymax = upr),
                               alpha = 0.18, fill = eco_red)
        if (input$show_points)
          p <- p + geom_point(data = obs, aes(x = x, y = y), alpha = 0.6, colour = eco_green)
        p <- p + geom_line(data = pred, aes(x = x, y = fit), colour = eco_red, linewidth = 1)
      } else {
        if (input$show_points)
          p <- p + geom_jitter(data = obs, aes(x = x, y = y), alpha = 0.5,
                               colour = eco_green, width = 0.15, height = 0)
        if (input$show_ci)
          p <- p + geom_errorbar(data = pred, aes(x = x, ymin = lwr, ymax = upr),
                                 width = 0.2, colour = eco_red)
        p <- p + geom_point(data = pred, aes(x = x, y = fit), colour = eco_red, size = 3.5)
      }
      sub <- if (length(others))
        paste("Prediction line with other predictors held at:", paste(held, collapse = ", ")) else NULL
      if (isTRUE(spec$mixed))
        sub <- paste(c(sub, sprintf("Population-level prediction (average %s)", paste(spec$rands, collapse = ", "))),
                     collapse = "; ")
      p + labs(title = paste("Fitted model:", spec$resp, "~", x_var), subtitle = sub,
               x = x_var, y = spec$resp) +
        theme_minimal()
    })

    output$regression_plot <- renderPlotly({
      p   <- reg_plot()
      sub <- p$labels$subtitle
      out <- ggplotly(p)
      # plotly drops ggplot subtitles, so add it back under the title
      if (!is.null(sub))
        out <- layout(out, title = list(text = paste0(p$labels$title, "<br><sup>", sub, "</sup>")),
                      margin = list(t = 70))
      out
    })

    output$download_plot <- downloadHandler(
      filename = function() paste0("regression_plot_", Sys.Date(), ".png"),
      content  = function(file) {
        ggsave(file, plot = reg_plot(), width = 10, height = 6, dpi = 150, bg = "white")
      }
    )
  })
}

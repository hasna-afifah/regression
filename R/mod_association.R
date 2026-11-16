# =============================================================================
# mod_association.R - Step 5 "Correlation": Pearson / Spearman / Kendall
#                             and chi-square tests for categorical data
# =============================================================================

associationUI <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    title = "Correlation between variables",

    # ---- Correlation ---------------------------------------------------------
    nav_panel(
      "Correlation (two numeric variables)",
      layout_columns(
        col_widths = c(3, 9),
        tags$div(
          selectInput(ns("cx"), "Variable X", choices = NULL),
          selectInput(ns("cy"), "Variable Y", choices = NULL),
          radioButtons(ns("cmethod"), "Method",
                       choices = c("Pearson (linear, normal data)" = "pearson",
                                   "Spearman (rank-based, monotonic)" = "spearman",
                                   "Kendall (rank-based, small n / many ties)" = "kendall")),
          hr(),
          tags$small(
            class = "text-muted",
            tags$b("Pearson"), " measures how close the points are to a straight line; ",
            "it needs roughly normal variables and is sensitive to outliers.", tags$br(),
            tags$b("Spearman"), " is Pearson on the ranks: it detects any consistently ",
            "increasing or decreasing (monotonic) relationship and is robust to ",
            "outliers and skew.", tags$br(),
            "Correlation is symmetric and says nothing about cause - to predict Y ",
            "from X use the Regression tab.")
        ),
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Raw values (what Pearson sees)"),
                 plotOutput(ns("c_scatter"), height = "300px")),
            card(card_header("Ranks (what Spearman sees)"),
                 plotOutput(ns("c_rank"), height = "300px"))
          ),
          layout_columns(
            col_widths = c(7, 5),
            card(card_header("Pearson vs Spearman vs Kendall"),
                 DTOutput(ns("c_table")),
                 uiOutput(ns("c_interp"))),
            card(card_header("Normality of each variable"),
                 DTOutput(ns("c_norm")),
                 uiOutput(ns("c_advice")))
          ),
          card(card_header(textOutput(ns("c_mat_title"), inline = TRUE)),
               plotOutput(ns("c_matrix"), height = "420px"))
        )
      )
    ),

    # ---- Chi-square ----------------------------------------------------------
    nav_panel(
      "Chi-square (categorical variables)",
      layout_columns(
        col_widths = c(3, 9),
        tags$div(
          radioButtons(ns("qtype"), "Test",
                       choices = c("Independence (two categorical variables)" = "indep",
                                   "Goodness of fit (one variable vs expected)" = "gof")),
          selectInput(ns("qa"), "Variable A (rows)", choices = NULL),
          conditionalPanel(
            "input.qtype == 'indep'", ns = ns,
            selectInput(ns("qb"), "Variable B (columns)", choices = NULL)
          ),
          conditionalPanel(
            "input.qtype == 'gof'", ns = ns,
            textInput(ns("qprobs"), "Expected proportions (comma-separated, blank = equal)",
                      value = ""),
            uiOutput(ns("q_levels_hint"))
          ),
          hr(),
          tags$small(
            class = "text-muted",
            "Chi-square tests work on ", tags$b("counts of independent observations"),
            " (e.g. number of plots that are burned / unburned in each forest type), ",
            "not on means or percentages. Expected counts should be >= 5 in most ",
            "cells; otherwise rely on Fisher's exact test.")
        ),
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Plot"), plotOutput(ns("q_plot"), height = "330px")),
            card(card_header("Test result"),
                 verbatimTextOutput(ns("q_result")),
                 uiOutput(ns("q_interp")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Observed counts"), DTOutput(ns("q_obs"))),
            card(card_header("Expected counts (if the two variables are unrelated)"), DTOutput(ns("q_exp")))
          ),
          card(card_header("Standardised residuals (|value| > 2 = cell contributes strongly)"),
               DTOutput(ns("q_resid")))
        )
      )
    )
  )
}

associationServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      df <- rv$data
      req(df)
      nums <- num_cols(df); cats <- cat_cols(df)
      keep <- function(cur, ch, default = ch[1]) if (isTRUE(cur %in% ch)) cur else default
      updateSelectInput(session, "cx", choices = nums, selected = keep(isolate(input$cx), nums))
      updateSelectInput(session, "cy", choices = nums,
                        selected = keep(isolate(input$cy), nums, nums[min(2, length(nums))]))
      updateSelectInput(session, "qa", choices = cats, selected = keep(isolate(input$qa), cats))
      updateSelectInput(session, "qb", choices = cats,
                        selected = keep(isolate(input$qb), cats, cats[min(2, length(cats))]))
    })

    # ---- Correlation ---------------------------------------------------------
    cdat <- reactive({
      req(rv$data, input$cx, input$cy)
      df <- as.data.frame(rv$data, check.names = FALSE)
      validate(need(all(c(input$cx, input$cy) %in% num_cols(df)), "Choose two numeric variables."),
               need(input$cx != input$cy, "Choose two different variables."))
      d <- data.frame(x = df[[input$cx]], y = df[[input$cy]])
      d <- d[is.finite(d$x) & is.finite(d$y), ]
      validate(need(nrow(d) >= 4, "Need at least 4 complete observations."))
      d
    })

    ctests <- reactive({
      d <- cdat()
      lapply(c(pearson = "pearson", spearman = "spearman", kendall = "kendall"), function(m)
        suppressWarnings(cor.test(d$x, d$y, method = m, exact = FALSE)))
    })

    output$c_scatter <- renderPlot({
      d <- cdat()
      ggplot(d, aes(x, y)) +
        geom_point(colour = eco_green, alpha = 0.65, size = 2) +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = eco_red, linewidth = 0.9) +
        labs(x = input$cx, y = input$cy,
             subtitle = sprintf("Pearson r = %.3f", ctests()$pearson$estimate)) +
        eco_theme()
    })

    output$c_rank <- renderPlot({
      d <- cdat()
      d$rx <- rank(d$x); d$ry <- rank(d$y)
      ggplot(d, aes(rx, ry)) +
        geom_point(colour = eco_blue, alpha = 0.65, size = 2) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = eco_red, linewidth = 0.9) +
        labs(x = paste("Rank of", input$cx), y = paste("Rank of", input$cy),
             subtitle = sprintf("Spearman rho = %.3f", ctests()$spearman$estimate)) +
        eco_theme()
    })

    output$c_table <- renderDT({
      ts <- ctests()
      tbl <- data.frame(
        Method      = c("Pearson r", "Spearman rho", "Kendall tau"),
        Coefficient = vapply(ts, function(t) unname(t$estimate), 0),
        p           = vapply(ts, function(t) t$p.value, 0),
        `95% CI`    = c(sprintf("%.3f to %.3f", ts$pearson$conf.int[1], ts$pearson$conf.int[2]), "", ""),
        N           = nrow(cdat()),
        check.names = FALSE)
      sel <- match(input$cmethod, names(ts))
      simple_dt(round_df(tbl, 4), selection = "none") |>
        formatStyle("Method", target = "row",
                    fontWeight = styleEqual(tbl$Method[sel], "bold"),
                    backgroundColor = styleEqual(tbl$Method[sel], "#e8f5e9"))
    })

    output$c_interp <- renderUI({
      t <- ctests()[[input$cmethod]]
      r <- unname(t$estimate)
      strength <- if (abs(r) < 0.1) "negligible" else if (abs(r) < 0.3) "weak" else
        if (abs(r) < 0.5) "moderate" else if (abs(r) < 0.7) "strong" else "very strong"
      nm <- c(pearson = "Pearson r", spearman = "Spearman rho", kendall = "Kendall tau")[[input$cmethod]]
      extra <- if (input$cmethod == "pearson")
        sprintf(" r-squared = %.2f: the two variables share %.0f%% of their variance.", r^2, 100 * r^2) else ""
      help_note(tags$b("In words: "),
                sprintf("%s = %.3f - a %s %s correlation between %s and %s, which is %s (%s).%s",
                        nm, r, strength, if (r >= 0) "positive" else "negative",
                        input$cx, input$cy, sig_word(t$p.value), fmt_peq(t$p.value), extra))
    })

    output$c_norm <- renderDT({
      d <- cdat()
      sx <- safe_shapiro(d$x); sy <- safe_shapiro(d$y)
      tbl <- data.frame(Variable = c(input$cx, input$cy),
                        Skewness = c(skewness(d$x), skewness(d$y)),
                        `Shapiro W` = c(unname(sx$statistic), unname(sy$statistic)),
                        `Shapiro p` = c(sx$p.value, sy$p.value), check.names = FALSE)
      simple_dt(round_df(tbl, 4)) |>
        formatStyle("Shapiro p", backgroundColor = styleInterval(0.05, c("#ffc7ce", "#c6efce")))
    })

    output$c_advice <- renderUI({
      d  <- cdat()
      ps <- c(safe_shapiro(d$x)$p.value, safe_shapiro(d$y)$p.value)
      ts <- ctests()
      gap <- abs(ts$spearman$estimate) - abs(ts$pearson$estimate)
      msg <- if (all(ps >= 0.05, na.rm = TRUE)) {
        "Both variables look normal: Pearson is appropriate (check the scatter is roughly linear)."
      } else {
        "At least one variable is not normal: prefer Spearman, or transform the skewed variable (Transform tab) and re-check."
      }
      if (gap > 0.1) msg <- paste(msg, "Spearman is clearly larger than Pearson, which points to a curved (monotonic but non-linear) relationship or outliers.")
      tags$div(class = "alert alert-warning py-2 px-3 mt-2 mb-0", style = "font-size: 0.88rem;", msg)
    })

    output$c_mat_title <- renderText({
      paste0("Correlation matrix of all numeric variables (",
             c(pearson = "Pearson", spearman = "Spearman", kendall = "Kendall")[[input$cmethod]],
             "); x = not significant at 0.05")
    })

    output$c_matrix <- renderPlot({
      req(rv$data)
      df <- as.data.frame(rv$data, check.names = FALSE)
      nd <- df[, num_cols(df), drop = FALSE]
      nd <- nd[, vapply(nd, function(x) length(unique(x[is.finite(x)])) > 2, logical(1)), drop = FALSE]
      validate(need(ncol(nd) >= 2, "Need at least two numeric columns."),
               need(ncol(nd) <= 25, "Too many numeric columns (> 25) to draw a readable matrix."))
      m <- input$cmethod
      vars <- names(nd)
      grid <- expand.grid(Var1 = vars, Var2 = vars, stringsAsFactors = FALSE)
      grid$r <- NA_real_; grid$p <- NA_real_
      for (i in seq_len(nrow(grid))) {
        if (grid$Var1[i] == grid$Var2[i]) { grid$r[i] <- 1; grid$p[i] <- 0; next }
        ct <- tryCatch(suppressWarnings(cor.test(nd[[grid$Var1[i]]], nd[[grid$Var2[i]]],
                                                 method = m, exact = FALSE)),
                       error = function(e) NULL)
        if (!is.null(ct)) { grid$r[i] <- unname(ct$estimate); grid$p[i] <- ct$p.value }
      }
      grid$Var1 <- factor(grid$Var1, levels = vars)
      grid$Var2 <- factor(grid$Var2, levels = rev(vars))
      grid$lab <- ifelse(is.na(grid$r), "", ifelse(grid$p >= 0.05, "x", sprintf("%.2f", grid$r)))
      ggplot(grid, aes(Var1, Var2, fill = r)) +
        geom_tile(colour = "white") +
        geom_text(aes(label = lab), size = if (length(vars) > 12) 2.6 else 3.4) +
        scale_fill_gradient2(low = eco_red, mid = "white", high = eco_green,
                             midpoint = 0, limits = c(-1, 1), name = "Coefficient") +
        labs(x = NULL, y = NULL) +
        eco_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
    })

    # ---- Chi-square ----------------------------------------------------------
    output$q_levels_hint <- renderUI({
      req(rv$data, input$qa %in% names(rv$data))
      lv <- levels(factor(rv$data[[input$qa]]))
      tags$small(class = "text-muted", "Order of categories: ", paste(lv, collapse = ", "))
    })

    qres <- reactive({
      req(rv$data, input$qa, input$qtype)
      df <- as.data.frame(rv$data, check.names = FALSE)
      validate(need(input$qa %in% names(df), "Choose a categorical variable."))
      a <- factor(df[[input$qa]])

      if (input$qtype == "gof") {
        tab <- table(a)
        validate(need(length(tab) >= 2, "The variable needs at least 2 categories."))
        txt <- trimws(input$qprobs)
        if (nzchar(txt)) {
          pr <- suppressWarnings(as.numeric(strsplit(txt, "[,;]")[[1]]))
          validate(need(!anyNA(pr) && length(pr) == length(tab) && all(pr > 0),
                        sprintf("Enter %d positive numbers separated by commas (one per category).", length(tab))))
          pr <- pr / sum(pr)
        } else {
          pr <- rep(1 / length(tab), length(tab))
        }
        ht <- suppressWarnings(chisq.test(tab, p = pr))
        ht$data.name <- input$qa
        obs <- data.frame(Category = names(tab), Observed = as.integer(tab))
        exp <- data.frame(Category = names(tab), `Expected proportion` = pr,
                          Expected = as.numeric(ht$expected), check.names = FALSE)
        res <- data.frame(Category = names(tab), `Std. residual` = as.numeric(ht$stdres), check.names = FALSE)
        low <- sum(ht$expected < 5)
        interp <- sprintf(
          "The observed frequencies of %s %s the expected proportions (chi-squared = %.2f, df = %d, %s).",
          input$qa, if (ht$p.value < 0.05) "differ significantly from" else "are consistent with",
          ht$statistic, ht$parameter, fmt_peq(ht$p.value))
        pd <- data.frame(Category = factor(names(tab), levels = names(tab)),
                         Observed = as.numeric(tab), Expected = as.numeric(ht$expected))
        plt <- ggplot(pd, aes(x = Category)) +
          geom_col(aes(y = Observed), fill = eco_green, alpha = 0.8) +
          geom_point(aes(y = Expected), colour = eco_red, size = 4, shape = 18) +
          labs(x = input$qa, y = "Count", caption = "Bars = observed, red diamonds = expected") +
          eco_theme()
        return(list(print = list(ht), obs = obs, exp = exp, res = res, interp = interp,
                    low = low, plot = plt))
      }

      validate(need(input$qb %in% names(df), "Choose a second categorical variable."),
               need(input$qb != input$qa, "Choose two different variables."))
      b   <- factor(df[[input$qb]])
      tab <- table(a, b)
      tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
      validate(need(nrow(tab) >= 2 && ncol(tab) >= 2, "Each variable needs at least 2 categories with data."))
      ht <- suppressWarnings(chisq.test(tab, correct = (nrow(tab) == 2 && ncol(tab) == 2)))
      ht$data.name <- paste(input$qa, "x", input$qb)
      low <- sum(ht$expected < 5)
      prints <- list(ht)
      fp <- NA
      if (low > 0 || (nrow(tab) == 2 && ncol(tab) == 2)) {
        ft <- tryCatch(fisher.test(tab), error = function(e)
          tryCatch(fisher.test(tab, simulate.p.value = TRUE, B = 10000), error = function(e) NULL))
        if (!is.null(ft)) { ft$data.name <- ht$data.name; prints[[2]] <- ft; fp <- ft$p.value }
      }
      v <- cramers_v(tab)
      to_df <- function(m, digits = NULL) {
        x <- as.data.frame.matrix(m)
        if (!is.null(digits)) x <- round(x, digits)
        cbind(setNames(data.frame(rownames(x)), input$qa), x)
      }
      p_use <- if (low > 0 && !is.na(fp)) fp else ht$p.value
      interp <- sprintf(
        "%s and %s are %s (chi-squared = %.2f, df = %d, %s%s). Cramer's V = %.2f (%s relationship).",
        input$qa, input$qb,
        if (p_use < 0.05) "significantly related: the proportions differ between groups" else "not significantly related",
        ht$statistic, ht$parameter, fmt_peq(ht$p.value),
        if (!is.na(fp)) paste0("; Fisher's exact ", fmt_peq(fp)) else "",
        v, if (v < 0.1) "negligible" else if (v < 0.3) "weak" else if (v < 0.5) "moderate" else "strong")
      pd <- as.data.frame(tab)
      names(pd) <- c("A", "B", "n")
      plt <- ggplot(pd, aes(x = A, y = n, fill = B)) +
        geom_col(position = "fill", alpha = 0.85, colour = "white") +
        scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
        scale_fill_manual(values = rep(eco_palette, length.out = nlevels(pd$B)), name = input$qb) +
        labs(x = input$qa, y = "Share of observations",
             caption = "Similar bars = no relationship") +
        eco_theme() + theme(legend.position = "bottom")
      list(print = prints, obs = to_df(tab), exp = to_df(ht$expected, 2),
           res = to_df(ht$stdres, 2), interp = interp, low = low, plot = plt, v = v)
    })

    output$q_plot   <- renderPlot(qres()$plot)
    output$q_result <- renderPrint({
      r <- qres()
      for (p in r$print) print(p)
      if (!is.null(r$v)) cat(sprintf("Cramer's V = %.3f\n", r$v))
    })
    output$q_interp <- renderUI({
      r <- qres()
      tagList(
        help_note(tags$b("In words: "), r$interp),
        if (r$low > 0) tags$div(
          class = "alert alert-warning py-2 px-3 mb-0", style = "font-size: 0.88rem;",
          sprintf("%d cell(s) have an expected count below 5, so the chi-square approximation is unreliable - use Fisher's exact p-value or merge rare categories.", r$low))
      )
    })
    output$q_obs   <- renderDT(simple_dt(qres()$obs))
    output$q_exp   <- renderDT(simple_dt(round_df(qres()$exp, 3)))
    output$q_resid <- renderDT(simple_dt(round_df(qres()$res, 2)))
  })
}

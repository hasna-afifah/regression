# =============================================================================
# mod_groups.R - Step 4: compare groups (t-test family, ANOVA, non-parametric)
# =============================================================================

group_tests <- list(
  "One sample vs a reference value" = c(
    "One-sample t-test"                      = "one_t",
    "One-sample Wilcoxon signed-rank"        = "one_w"),
  "Two independent groups" = c(
    "Two-sample t-test (Welch / Student)"    = "two_t",
    "Mann-Whitney U (Wilcoxon rank-sum)"     = "two_mw"),
  "Two paired measurements" = c(
    "Paired t-test"                          = "pair_t",
    "Wilcoxon signed-rank (paired)"          = "pair_w"),
  "Three or more groups" = c(
    "One-way ANOVA + Tukey HSD"              = "anova1",
    "Welch's ANOVA (unequal variances)"      = "welch",
    "Kruskal-Wallis + pairwise Wilcoxon"     = "kw",
    "Two-way ANOVA (two factors)"            = "anova2")
)

groupsUI <- function(id) {
  ns <- NS(id)
  in_tests <- function(x) sprintf("[%s].includes(input.test)",
                                  paste0("'", x, "'", collapse = ","))
  layout_columns(
    col_widths = c(3, 9),
    card(
      card_header("Compare Groups"),
      selectInput(ns("test"), "Test", choices = group_tests, selected = "anova1"),
      selectInput(ns("resp"), "Response variable (numeric)", choices = NULL),
      conditionalPanel(
        in_tests(c("one_t", "one_w")), ns = ns,
        numericInput(ns("mu"), "Reference value (mu)", value = 0)
      ),
      conditionalPanel(
        in_tests(c("pair_t", "pair_w")), ns = ns,
        selectInput(ns("resp2"), "Paired with (second measurement)", choices = NULL),
        tags$small(class = "text-muted",
                   "Each row = one unit (plot, tree) measured twice.")
      ),
      conditionalPanel(
        in_tests(c("two_t", "two_mw", "anova1", "welch", "kw", "anova2")), ns = ns,
        selectInput(ns("grp"), "Grouping variable", choices = NULL)
      ),
      conditionalPanel(
        in_tests(c("two_t", "two_mw")), ns = ns,
        fluidRow(
          column(6, selectInput(ns("lv1"), "Group 1", choices = NULL)),
          column(6, selectInput(ns("lv2"), "Group 2", choices = NULL))
        )
      ),
      conditionalPanel(
        in_tests("two_t"), ns = ns,
        checkboxInput(ns("var_equal"), "Assume equal variances (Student)", FALSE)
      ),
      conditionalPanel(
        in_tests("anova2"), ns = ns,
        selectInput(ns("grp2"), "Second factor", choices = NULL),
        checkboxInput(ns("interaction"), "Include interaction", TRUE)
      ),
      conditionalPanel(
        in_tests(c("one_t", "one_w", "two_t", "two_mw", "pair_t", "pair_w")), ns = ns,
        selectInput(ns("alt"), "Alternative hypothesis",
                    choices = c("Two-sided (different)" = "two.sided",
                                "Greater" = "greater", "Less" = "less"))
      ),
      hr(),
      tags$small(class = "text-muted",
                 "Parametric tests (t-test, ANOVA) compare means and assume ",
                 "normal residuals. Non-parametric tests (Wilcoxon, ",
                 "Mann-Whitney, Kruskal-Wallis) work on ranks - use them when ",
                 "normality fails and transformation does not help.")
    ),
    layout_columns(
      col_widths = 12,
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Plot"), plotOutput(ns("plot"), height = "360px")),
        card(card_header("Test result"),
             verbatimTextOutput(ns("result")),
             uiOutput(ns("interp")))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Descriptive statistics"), DTOutput(ns("desc"))),
        card(card_header("Assumption checks"),
             DTOutput(ns("assump")),
             uiOutput(ns("advice")))
      ),
      card(card_header(textOutput(ns("posthoc_title"), inline = TRUE)),
           DTOutput(ns("posthoc")))
    )
  )
}

# ---- helpers -----------------------------------------------------------------
desc_tbl <- function(y, g) {
  parts <- split(y, g, drop = TRUE)
  do.call(rbind, lapply(names(parts), function(nm) {
    v <- parts[[nm]]
    data.frame(Group = nm, N = length(v), Mean = mean(v), SD = sd(v),
               SE = sd(v) / sqrt(length(v)), Median = median(v),
               Min = min(v), Max = max(v))
  }))
}

assump_row <- function(check, stat, p) {
  data.frame(Check = check, Statistic = stat, p = p,
             Verdict = ifelse(is.na(p), "n/a", ifelse(p >= 0.05, "OK", "Violated")))
}

pw_long <- function(pw, label) {
  m   <- pw$p.value
  idx <- which(!is.na(m), arr.ind = TRUE)
  out <- data.frame(Comparison = paste(rownames(m)[idx[, 1]], "-", colnames(m)[idx[, 2]]),
                    p = m[idx])
  names(out)[2] <- label
  out
}

box_plot <- function(d, ylab, xlab) {
  ggplot(d, aes(x = g, y = y, fill = g)) +
    geom_boxplot(alpha = 0.55, outlier.shape = NA) +
    geom_jitter(width = 0.12, alpha = 0.5, size = 1.6) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5,
                 fill = "white", colour = "black") +
    scale_fill_manual(values = rep(eco_palette, length.out = nlevels(d$g))) +
    labs(x = xlab, y = ylab, caption = "Diamond = group mean") +
    eco_theme() + theme(legend.position = "none")
}

groupsServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      df <- rv$data
      req(df)
      nums <- num_cols(df); cats <- cat_cols(df)
      keep <- function(cur, ch, default = ch[1]) if (isTRUE(cur %in% ch)) cur else default
      updateSelectInput(session, "resp",  choices = nums, selected = keep(isolate(input$resp), nums))
      updateSelectInput(session, "resp2", choices = nums,
                        selected = keep(isolate(input$resp2), nums, nums[min(2, length(nums))]))
      updateSelectInput(session, "grp",   choices = cats, selected = keep(isolate(input$grp), cats))
      updateSelectInput(session, "grp2",  choices = cats,
                        selected = keep(isolate(input$grp2), cats, cats[min(2, length(cats))]))
    })

    observe({
      req(rv$data, input$grp %in% names(rv$data))
      lv <- levels(factor(rv$data[[input$grp]]))
      updateSelectInput(session, "lv1", choices = lv, selected = lv[1])
      updateSelectInput(session, "lv2", choices = lv, selected = lv[min(2, length(lv))])
    })

    res <- reactive({
      req(rv$data, input$test, input$resp)
      df   <- as.data.frame(rv$data, check.names = FALSE)
      test <- input$test
      yn   <- input$resp
      validate(need(yn %in% num_cols(df), "Choose a numeric response variable."))
      alt  <- input$alt
      out <- list(posthoc = NULL, posthoc_title = "Post-hoc comparisons / effect size", advice = NULL)

      # ---------------- one sample ----------------
      if (test %in% c("one_t", "one_w")) {
        x  <- df[[yn]]; x <- x[is.finite(x)]
        mu <- input$mu
        validate(need(is.finite(mu), "Enter a reference value."),
                 need(length(x) >= 3, "Need at least 3 observations."))
        ht <- if (test == "one_t") t.test(x, mu = mu, alternative = alt) else
          suppressWarnings(wilcox.test(x, mu = mu, alternative = alt, exact = FALSE, conf.int = TRUE))
        ht$data.name <- yn
        sw <- safe_shapiro(x)
        d  <- (mean(x) - mu) / sd(x)
        out$print  <- ht
        out$desc   <- desc_tbl(x, rep(yn, length(x)))
        out$assump <- assump_row("Normality of the variable (Shapiro-Wilk)", unname(sw$statistic), sw$p.value)
        out$posthoc <- data.frame(Measure = "Cohen's d (vs reference)", Value = d, Size = effect_label(d))
        stat_nm <- if (test == "one_t") "mean" else "median"
        out$interp <- sprintf(
          "Sample %s of %s = %.3g versus the reference value %g: the difference is %s (%s).",
          stat_nm, yn, if (test == "one_t") mean(x) else median(x), mu,
          sig_word(ht$p.value), fmt_peq(ht$p.value))
        out$plot <- ggplot(data.frame(x = x), aes(x = x)) +
          geom_histogram(bins = 20, fill = eco_green, colour = "white", alpha = 0.8) +
          geom_vline(xintercept = mu, colour = eco_red, linetype = "dashed", linewidth = 1) +
          geom_vline(xintercept = mean(x), colour = eco_orange, linewidth = 1) +
          labs(x = yn, y = "Count",
               caption = "Dashed red = reference value, orange = sample mean") +
          eco_theme()
        if (test == "one_t" && !is.na(sw$p.value) && sw$p.value < 0.05)
          out$advice <- "Normality is violated: transform the variable (Transform tab) or use the one-sample Wilcoxon test. With n > 30 the t-test is fairly robust."
        return(out)
      }

      # ---------------- paired ----------------
      if (test %in% c("pair_t", "pair_w")) {
        y2n <- input$resp2
        validate(need(y2n %in% num_cols(df), "Choose the second measurement."),
                 need(y2n != yn, "Choose two different columns for the paired measurements."))
        ok <- is.finite(df[[yn]]) & is.finite(df[[y2n]])
        x1 <- df[[yn]][ok]; x2 <- df[[y2n]][ok]
        validate(need(length(x1) >= 3, "Need at least 3 complete pairs."))
        dif <- x1 - x2
        ht <- if (test == "pair_t") t.test(x1, x2, paired = TRUE, alternative = alt) else
          suppressWarnings(wilcox.test(x1, x2, paired = TRUE, alternative = alt, exact = FALSE, conf.int = TRUE))
        ht$data.name <- paste(yn, "and", y2n)
        sw <- safe_shapiro(dif)
        dz <- mean(dif) / sd(dif)
        out$print  <- ht
        out$desc   <- rbind(desc_tbl(x1, rep(yn, length(x1))),
                            desc_tbl(x2, rep(y2n, length(x2))),
                            desc_tbl(dif, rep("Difference (1st - 2nd)", length(dif))))
        out$assump <- assump_row("Normality of the paired differences (Shapiro-Wilk)",
                                 unname(sw$statistic), sw$p.value)
        out$posthoc <- data.frame(Measure = "Cohen's d for paired data (mean diff / SD diff)",
                                  Value = dz, Size = effect_label(dz))
        out$interp <- sprintf(
          "Mean difference (%s - %s) = %.3g. The paired difference is %s (%s).",
          yn, y2n, mean(dif), sig_word(ht$p.value), fmt_peq(ht$p.value))
        long <- data.frame(id = rep(seq_along(x1), 2),
                           g  = factor(rep(c(yn, y2n), each = length(x1)), levels = c(yn, y2n)),
                           y  = c(x1, x2))
        out$plot <- ggplot(long, aes(x = g, y = y)) +
          geom_line(aes(group = id), colour = "grey60", alpha = 0.45) +
          geom_boxplot(aes(fill = g), alpha = 0.5, width = 0.35, outlier.shape = NA) +
          geom_point(alpha = 0.5, size = 1.4) +
          scale_fill_manual(values = eco_palette[1:2]) +
          labs(x = NULL, y = "Value", caption = "Grey lines join the two measurements of the same unit") +
          eco_theme() + theme(legend.position = "none")
        if (test == "pair_t" && !is.na(sw$p.value) && sw$p.value < 0.05)
          out$advice <- "The differences are not normal: use the Wilcoxon signed-rank test instead (or rely on the t-test only if n is large)."
        return(out)
      }

      # ---------------- tests with a grouping variable ----------------
      gn <- input$grp
      validate(need(gn %in% names(df), "Choose a grouping variable."),
               need(gn != yn, "Response and grouping variable must differ."))
      d <- data.frame(y = df[[yn]], g = factor(df[[gn]]))

      # ---- two independent groups ----
      if (test %in% c("two_t", "two_mw")) {
        validate(need(input$lv1 %in% levels(d$g) && input$lv2 %in% levels(d$g), "Choose two groups."),
                 need(input$lv1 != input$lv2, "Choose two different groups."))
        d <- d[is.finite(d$y) & d$g %in% c(input$lv1, input$lv2), ]
        d$g <- factor(d$g, levels = c(input$lv1, input$lv2))
        a <- d$y[d$g == input$lv1]; b <- d$y[d$g == input$lv2]
        validate(need(length(a) >= 3 && length(b) >= 3, "Each group needs at least 3 observations."))
        ht <- if (test == "two_t") t.test(a, b, var.equal = isTRUE(input$var_equal), alternative = alt) else
          suppressWarnings(wilcox.test(a, b, alternative = alt, exact = FALSE, conf.int = TRUE))
        ht$data.name <- sprintf("%s in %s vs %s", yn, input$lv1, input$lv2)
        swa <- safe_shapiro(a); swb <- safe_shapiro(b)
        lev <- tryCatch(car::leveneTest(y ~ g, data = d), error = function(e) NULL)
        out$print  <- ht
        out$desc   <- desc_tbl(d$y, d$g)
        out$assump <- rbind(
          assump_row(paste("Normality in", input$lv1, "(Shapiro-Wilk)"), unname(swa$statistic), swa$p.value),
          assump_row(paste("Normality in", input$lv2, "(Shapiro-Wilk)"), unname(swb$statistic), swb$p.value),
          assump_row("Equal variances (Levene)",
                     if (is.null(lev)) NA else lev$`F value`[1], if (is.null(lev)) NA else lev$`Pr(>F)`[1]))
        dd <- cohens_d_two(a, b)
        rb <- 2 * unname(suppressWarnings(wilcox.test(a, b, exact = FALSE)$statistic)) / (length(a) * length(b)) - 1
        out$posthoc <- rbind(
          data.frame(Measure = "Cohen's d (standardised mean difference)", Value = dd, Size = effect_label(dd)),
          data.frame(Measure = "Rank-biserial correlation (non-parametric effect size)", Value = rb, Size = ""))
        out$interp <- sprintf(
          "%s: %s mean = %.3g, median = %.3g; %s mean = %.3g, median = %.3g. The difference between the two groups is %s (%s).",
          yn, input$lv1, mean(a), median(a), input$lv2, mean(b), median(b),
          sig_word(ht$p.value), fmt_peq(ht$p.value))
        out$plot <- box_plot(d, yn, gn)
        nonnorm <- any(c(swa$p.value, swb$p.value) < 0.05, na.rm = TRUE)
        if (test == "two_t" && nonnorm)
          out$advice <- "Normality is violated in at least one group: transform the response (Transform tab) or use the Mann-Whitney U test."
        else if (test == "two_t" && isTRUE(input$var_equal) && !is.null(lev) && lev$`Pr(>F)`[1] < 0.05)
          out$advice <- "Variances differ: untick 'Assume equal variances' to use Welch's t-test (the safer default)."
        return(out)
      }

      # ---- two-way ANOVA ----
      if (test == "anova2") {
        g2n <- input$grp2
        validate(need(g2n %in% names(df), "Choose a second factor."),
                 need(!g2n %in% c(gn, yn), "The second factor must differ from the first factor and the response."))
        d$g2 <- factor(df[[g2n]])
        d <- d[is.finite(d$y) & !is.na(d$g) & !is.na(d$g2), ]
        d$g <- droplevels(d$g); d$g2 <- droplevels(d$g2)
        validate(need(nlevels(d$g) >= 2 && nlevels(d$g2) >= 2, "Each factor needs at least 2 levels."))
        inter <- isTRUE(input$interaction)
        fit <- tryCatch(aov(if (inter) y ~ g * g2 else y ~ g + g2, data = d), error = function(e) NULL)
        validate(need(!is.null(fit), "The two-way ANOVA could not be fitted."))
        at <- tryCatch(as.data.frame(car::Anova(fit, type = 2)), error = function(e) NULL)
        validate(need(!is.null(at), "ANOVA table could not be computed (empty factor combinations?)."))
        rn <- rownames(at)
        rn <- sub("^g:g2$", paste0(gn, " x ", g2n), sub("^g2$", g2n, sub("^g$", gn, rn)))
        ss_res <- at$`Sum Sq`[nrow(at)]
        at_out <- data.frame(Term = rn, `Sum Sq` = at$`Sum Sq`, Df = at$Df,
                             `F value` = at$`F value`, p = at$`Pr(>F)`,
                             `Partial eta-sq` = c(at$`Sum Sq`[-nrow(at)] / (at$`Sum Sq`[-nrow(at)] + ss_res), NA),
                             check.names = FALSE)
        sw  <- safe_shapiro(residuals(fit))
        lev <- tryCatch(car::leveneTest(y ~ g * g2, data = d), error = function(e) NULL)
        out$print_fun <- function() {
          cat("Two-way ANOVA (Type II sums of squares)\n")
          cat("Response:", yn, "\n\n")
          print(round_df(at_out, 4), row.names = FALSE)
        }
        d$cell <- interaction(d$g, d$g2, sep = " | ")
        out$desc   <- desc_tbl(d$y, d$cell)
        out$assump <- rbind(
          assump_row("Normality of residuals (Shapiro-Wilk)", unname(sw$statistic), sw$p.value),
          assump_row("Equal variances across cells (Levene)",
                     if (is.null(lev)) NA else lev$`F value`[1], if (is.null(lev)) NA else lev$`Pr(>F)`[1]))
        tk <- tryCatch(TukeyHSD(aov(y ~ g + g2, data = d)), error = function(e) NULL)
        if (!is.null(tk)) {
          ph <- do.call(rbind, lapply(names(tk), function(nm) {
            x <- as.data.frame(tk[[nm]])
            data.frame(Factor = c(g = gn, g2 = g2n)[[nm]], Comparison = rownames(x),
                       Difference = x$diff, Lower = x$lwr, Upper = x$upr,
                       `p (Tukey-adjusted)` = x$`p adj`, check.names = FALSE)
          }))
          out$posthoc <- ph
          out$posthoc_title <- "Post-hoc: Tukey HSD for the main effects"
        }
        ps <- setNames(at_out$p, at_out$Term)
        bits <- sprintf("%s: %s (%s)", names(ps)[-length(ps)],
                        vapply(ps[-length(ps)], sig_word, ""), vapply(ps[-length(ps)], fmt_peq, ""))
        out$interp <- paste0("Effects on ", yn, " - ", paste(bits, collapse = "; "), ".",
                             if (inter && isTRUE(ps[length(ps) - 1] < 0.05))
                               " The interaction is significant: the effect of one factor depends on the level of the other, so interpret main effects with care." else "")
        ms <- aggregate(y ~ g + g2, data = d,
                        FUN = function(v) c(m = mean(v), se = sd(v) / sqrt(length(v))))
        ms <- data.frame(g = ms$g, g2 = ms$g2, m = ms$y[, "m"], se = ms$y[, "se"])
        ms$se[is.na(ms$se)] <- 0
        pd <- position_dodge(width = 0.25)
        out$plot <- ggplot(ms, aes(x = g, y = m, colour = g2, group = g2)) +
          geom_line(position = pd, linewidth = 0.9) +
          geom_errorbar(aes(ymin = m - se, ymax = m + se), width = 0.15, position = pd) +
          geom_point(position = pd, size = 3) +
          scale_colour_manual(values = rep(eco_palette, length.out = nlevels(d$g2)), name = g2n) +
          labs(x = gn, y = paste("Mean", yn, "+/- SE"),
               caption = "Interaction plot: non-parallel lines suggest an interaction") +
          eco_theme() + theme(legend.position = "bottom")
        if (!is.na(sw$p.value) && sw$p.value < 0.05)
          out$advice <- "Residuals are not normal: transform the response (Transform tab) and re-run, or model the data with a GLM in the Regression tab (factors can be used as predictors)."
        return(out)
      }

      # ---- one factor, 3+ groups ----
      d <- d[is.finite(d$y) & !is.na(d$g), ]
      d$g <- droplevels(d$g)
      validate(need(nlevels(d$g) >= 2, "The grouping variable needs at least 2 groups."),
               need(min(table(d$g)) >= 2, "Each group needs at least 2 observations."))
      fit <- aov(y ~ g, data = d)
      sw  <- safe_shapiro(residuals(fit))
      lev <- tryCatch(car::leveneTest(y ~ g, data = d), error = function(e) NULL)
      lev_p <- if (is.null(lev)) NA else lev$`Pr(>F)`[1]
      out$desc   <- desc_tbl(d$y, d$g)
      out$assump <- rbind(
        assump_row("Normality of residuals (Shapiro-Wilk)", unname(sw$statistic), sw$p.value),
        assump_row("Equal variances (Levene)", if (is.null(lev)) NA else lev$`F value`[1], lev_p))
      out$plot <- box_plot(d, yn, gn)
      nonnorm <- !is.na(sw$p.value) && sw$p.value < 0.05
      hetero  <- !is.na(lev_p) && lev_p < 0.05

      if (test == "anova1") {
        s  <- summary(fit)[[1]]
        p  <- s$`Pr(>F)`[1]
        eta <- s$`Sum Sq`[1] / sum(s$`Sum Sq`)
        out$print_fun <- function() {
          cat("One-way ANOVA\n")
          cat("Response:", yn, "  Factor:", gn, "\n\n")
          rownames(s) <- c(gn, "Residuals")
          print(s)
          cat(sprintf("\nEta-squared = %.3f (share of variance explained by %s)\n", eta, gn))
        }
        tk <- as.data.frame(TukeyHSD(fit)$g)
        out$posthoc <- data.frame(Comparison = rownames(tk), Difference = tk$diff,
                                  Lower = tk$lwr, Upper = tk$upr,
                                  `p (Tukey-adjusted)` = tk$`p adj`, check.names = FALSE)
        out$posthoc_title <- "Post-hoc: Tukey HSD (which groups differ?)"
        out$interp <- sprintf(
          "Mean %s %s among the %d levels of %s (F = %.2f, %s); %s explains %.0f%% of the variance.%s",
          yn, if (p < 0.05) "differs significantly" else "does not differ significantly",
          nlevels(d$g), gn, s$`F value`[1], fmt_peq(p), gn, 100 * eta,
          if (p < 0.05) " See the Tukey table for which pairs differ." else "")
        if (nonnorm) out$advice <- "Residuals are not normal: transform the response (Transform tab) or use Kruskal-Wallis."
        else if (hetero) out$advice <- "Variances are unequal: use Welch's ANOVA, or transform the response."
      } else if (test == "welch") {
        ht <- oneway.test(y ~ g, data = d, var.equal = FALSE)
        ht$data.name <- paste(yn, "by", gn)
        out$print <- ht
        out$posthoc <- pw_long(pairwise.t.test(d$y, d$g, pool.sd = FALSE, p.adjust.method = "holm"),
                               "p (Holm-adjusted)")
        out$posthoc_title <- "Post-hoc: pairwise Welch t-tests (Holm correction)"
        out$interp <- sprintf(
          "Mean %s %s among the levels of %s (Welch F = %.2f, %s).",
          yn, if (ht$p.value < 0.05) "differs significantly" else "does not differ significantly",
          gn, ht$statistic, fmt_peq(ht$p.value))
        if (nonnorm) out$advice <- "Residuals are not normal: transform the response (Transform tab) or use Kruskal-Wallis."
      } else {
        ht <- kruskal.test(y ~ g, data = d)
        ht$data.name <- paste(yn, "by", gn)
        k <- nlevels(d$g); n <- nrow(d)
        eta_h <- max(0, (unname(ht$statistic) - k + 1) / (n - k))
        out$print <- ht
        out$posthoc <- pw_long(suppressWarnings(
          pairwise.wilcox.test(d$y, d$g, p.adjust.method = "holm", exact = FALSE)),
          "p (Holm-adjusted)")
        out$posthoc_title <- "Post-hoc: pairwise Wilcoxon rank-sum tests (Holm correction)"
        out$interp <- sprintf(
          "The distribution of %s %s among the levels of %s (Kruskal-Wallis chi-squared = %.2f, %s; eta-squared[H] = %.2f).",
          yn, if (ht$p.value < 0.05) "differs significantly" else "does not differ significantly",
          gn, ht$statistic, fmt_peq(ht$p.value), eta_h)
        if (!nonnorm && !hetero)
          out$advice <- "Assumptions for a one-way ANOVA look fine here - ANOVA would be more powerful than Kruskal-Wallis."
      }
      out
    })

    output$plot <- renderPlot(res()$plot)

    output$result <- renderPrint({
      r <- res()
      if (!is.null(r$print_fun)) r$print_fun() else print(r$print)
    })

    output$interp <- renderUI(help_note(tags$b("In words: "), res()$interp))

    output$desc <- renderDT(simple_dt(round_df(res()$desc, 3)))

    output$assump <- renderDT({
      simple_dt(round_df(res()$assump, 4)) |>
        formatStyle("Verdict", backgroundColor = styleEqual(
          c("OK", "Violated"), c("#c6efce", "#ffc7ce")))
    })

    output$advice <- renderUI({
      a <- res()$advice
      tagList(
        if (!is.null(a)) tags$div(class = "alert alert-warning py-2 px-3 mb-1",
                                  style = "font-size: 0.88rem;", a),
        tags$small(class = "text-muted", "p >= 0.05 = assumption not rejected (OK).")
      )
    })

    output$posthoc_title <- renderText(res()$posthoc_title)

    output$posthoc <- renderDT({
      ph <- res()$posthoc
      validate(need(!is.null(ph), "No post-hoc table for this test."))
      tbl <- simple_dt(round_df(ph, 4))
      pcol <- grep("^p \\(", names(ph), value = TRUE)
      if (length(pcol) == 1)
        tbl <- formatStyle(tbl, pcol, backgroundColor = styleInterval(0.05, c("#c6efce", "white")))
      tbl
    })
  })
}

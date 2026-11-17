# =============================================================================
# helpers.R - shared theme, constants and small statistical helpers
# (files in R/ are sourced automatically by Shiny before app.R runs)
# =============================================================================

# --- Theme & colours ----------------------------------------------------------
eco_green  <- "#2c7a3e"
eco_orange <- "#d35400"
eco_red    <- "#c0392b"
eco_blue   <- "#2471a3"
eco_palette <- c("#2c7a3e", "#d35400", "#2471a3", "#8e44ad",
                 "#b7950b", "#c0392b", "#16a085", "#7f8c8d")

app_theme <- bslib::bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = eco_green,
  success    = eco_green,
  "font-size-base" = "0.95rem"
)

family_choices <- c(
  "Gaussian (Normal)"                    = "gaussian",
  "Poisson (Counts)"                     = "poisson",
  "Negative binomial (overdispersed counts)" = "negbin",
  "Binomial (0/1, presence-absence)"     = "binomial",
  "Gamma (positive, right-skewed)"       = "Gamma",
  "Inverse Gaussian"                     = "inverse.gaussian"
)

# --- Small utilities ----------------------------------------------------------

# Wrap a column name in backticks if it has spaces or special characters
bt <- function(x) {
  needs_bt <- grepl("[^A-Za-z0-9._]", x) | grepl("^[0-9_]", x)
  ifelse(needs_bt, paste0("`", x, "`"), x)
}

fmt_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("NA")
  if (p < 0.001) "< 0.001" else formatC(p, digits = 3, format = "f")
}

sig_word <- function(p, alpha = 0.05) {
  if (is.na(p)) "not available" else if (p < alpha) "significant" else "not significant"
}

safe_shapiro <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3 || length(unique(x)) < 3) return(list(statistic = NA_real_, p.value = NA_real_))
  if (n > 5000) {
    set.seed(42)
    x <- sample(x, 5000)
  }
  out <- tryCatch(shapiro.test(x), error = function(e) NULL)
  if (is.null(out)) list(statistic = NA_real_, p.value = NA_real_) else out
}

skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- sqrt(mean((x - m)^2))
  if (s == 0) return(NA_real_)
  mean((x - m)^3) / s^3
}

# Columns by role --------------------------------------------------------------
num_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]

# Columns usable as grouping factors: text/factor/logical columns with a
# sensible number of levels, plus numeric columns with only a few distinct values
cat_cols <- function(df) {
  n <- nrow(df)
  ok <- vapply(df, function(x) {
    u <- length(unique(stats::na.omit(x)))
    if (u < 2) return(FALSE)
    if (is.numeric(x)) u <= 8 else u <= max(2, min(25, floor(n / 2)))
  }, logical(1))
  names(df)[ok]
}

eco_theme <- function() ggplot2::theme_minimal(base_size = 13)

# Columns that hold p-values (kept unrounded, shown with 3 significant digits)
is_p_col <- function(nm) grepl("^p$|^p \\(|Shapiro p$|^p\\.value$|^Pr\\(", nm)

simple_dt <- function(tbl, ...) {
  out <- DT::datatable(tbl, rownames = FALSE,
                       options = list(dom = "t", pageLength = 50, scrollX = TRUE),
                       class = "compact stripe", ...)
  pc <- names(tbl)[is_p_col(names(tbl)) & vapply(tbl, is.numeric, logical(1))]
  if (length(pc)) out <- DT::formatSignif(out, pc, digits = 3)
  out
}

round_df <- function(df, digits = 4) {
  for (nm in names(df)) {
    if (is.numeric(df[[nm]]) && !is_p_col(nm)) df[[nm]] <- round(df[[nm]], digits)
  }
  df
}

help_note <- function(...) {
  tags$div(class = "alert alert-success py-2 px-3 mb-2",
           style = "font-size: 0.88rem;", ...)
}

# --- Example data -------------------------------------------------------------
example_datasets <- c(
  "Forest plots (120 plots, 4 forest types)"       = "forest_plots.csv",
  "Tree community (40 plots x 14 taxa, stem counts)" = "tree_community.csv"
)

load_example <- function(file) {
  path <- file.path("data", file)
  if (!file.exists(path)) return(NULL)
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

# --- Transformations ----------------------------------------------------------
transform_choices <- c(
  "Log: ln(x)"                               = "log",
  "Log: ln(x + 1)"                           = "log1p",
  "Square root"                              = "sqrt",
  "Cube root"                                = "cuberoot",
  "Reciprocal: 1/x"                          = "inverse",
  "Square: x^2 (for left skew)"              = "square",
  "Arcsine square-root (proportions)"        = "asinsqrt",
  "Logit (proportions)"                      = "logit",
  "Box-Cox (optimal lambda, x > 0)"          = "boxcox",
  "Yeo-Johnson (optimal lambda, any x)"      = "yeojohnson",
  "Rank-based inverse normal"                = "rankint"
)

transform_suffix <- c(log = "log", log1p = "log1p", sqrt = "sqrt",
                      cuberoot = "cbrt", inverse = "inv", square = "sq",
                      asinsqrt = "asin", logit = "logit", boxcox = "boxcox",
                      yeojohnson = "yj", rankint = "rankint")

# Returns list(values = numeric vector (same length as x), note = text, ok = TRUE/FALSE)
apply_transform <- function(x, method) {
  fail <- function(msg) list(values = rep(NA_real_, length(x)), note = msg, ok = FALSE)
  if (!is.numeric(x)) return(fail("Variable is not numeric."))
  xf <- x[is.finite(x)]
  if (length(xf) < 3) return(fail("Not enough values."))
  note <- ""

  as_prop <- function(v) {
    if (min(xf) < 0) return(NULL)
    if (max(xf) <= 1) return(list(p = v, note = ""))
    if (max(xf) <= 100) return(list(p = v / 100, note = "Values looked like percentages and were divided by 100. "))
    NULL
  }

  out <- tryCatch(switch(
    method,
    log = {
      if (min(xf) <= 0) return(fail("ln(x) needs all values > 0. Try ln(x + 1) or Yeo-Johnson."))
      log(x)
    },
    log1p = {
      if (min(xf) <= -1) return(fail("ln(x + 1) needs all values > -1."))
      log1p(x)
    },
    sqrt = {
      if (min(xf) < 0) return(fail("Square root needs all values >= 0."))
      sqrt(x)
    },
    cuberoot = sign(x) * abs(x)^(1 / 3),
    inverse = {
      if (any(xf == 0)) return(fail("1/x is undefined for zero values."))
      note <- "Note: 1/x reverses the order of the values. "
      1 / x
    },
    square = x^2,
    asinsqrt = {
      pr <- as_prop(x)
      if (is.null(pr)) return(fail("Arcsine square-root needs proportions (0-1) or percentages (0-100)."))
      note <- pr$note
      asin(sqrt(pr$p))
    },
    logit = {
      pr <- as_prop(x)
      if (is.null(pr)) return(fail("Logit needs proportions (0-1) or percentages (0-100)."))
      note <- pr$note
      p <- pr$p
      if (any(p[is.finite(p)] %in% c(0, 1))) {
        n <- sum(is.finite(p))
        p <- (p * (n - 1) + 0.5) / n          # squeeze exact 0 and 1 inwards
        note <- paste0(note, "Exact 0/1 values were squeezed slightly inwards. ")
      }
      log(p / (1 - p))
    },
    boxcox = {
      if (min(xf) <= 0) return(fail("Box-Cox needs all values > 0. Use Yeo-Johnson instead."))
      lam <- unname(car::powerTransform(xf, family = "bcPower")$lambda)
      note <- sprintf("Estimated lambda = %.3f (lambda near 0 = log, 0.5 = square root, 1 = no change). ", lam)
      car::bcPower(x, lam)
    },
    yeojohnson = {
      lam <- unname(car::powerTransform(xf, family = "yjPower")$lambda)
      note <- sprintf("Estimated lambda = %.3f. ", lam)
      car::yjPower(x, lam)
    },
    rankint = {
      note <- "Forces a normal shape using ranks only; original units and distances are lost. "
      r <- rank(x, na.last = "keep")
      qnorm((r - 0.5) / sum(!is.na(x)))
    },
    return(fail("Unknown transformation."))
  ), error = function(e) NULL)

  if (is.null(out)) return(fail("Transformation could not be computed for this variable."))
  list(values = as.numeric(out), note = note, ok = TRUE)
}

normality_row <- function(x, label) {
  x  <- x[is.finite(x)]
  sw <- safe_shapiro(x)
  data.frame(
    Version    = label,
    N          = length(x),
    Mean       = mean(x),
    SD         = sd(x),
    Skewness   = skewness(x),
    `Shapiro W` = unname(sw$statistic),
    `Shapiro p` = sw$p.value,
    check.names = FALSE
  )
}

# --- Effect sizes -------------------------------------------------------------
cohens_d_two <- function(a, b) {
  na <- length(a); nb <- length(b)
  sp <- sqrt(((na - 1) * var(a) + (nb - 1) * var(b)) / (na + nb - 2))
  (mean(a) - mean(b)) / sp
}

effect_label <- function(d) {
  d <- abs(d)
  if (is.na(d)) "NA" else if (d < 0.2) "negligible" else if (d < 0.5) "small" else
    if (d < 0.8) "medium" else "large"
}

cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  n <- sum(tab)
  k <- min(nrow(tab), ncol(tab)) - 1
  if (k < 1) return(NA_real_)
  unname(sqrt(chi / (n * k)))
}

# "p < 0.001" / "p = 0.012" for sentences
fmt_peq <- function(p) {
  if (length(p) == 0 || is.na(p)) return("p = NA")
  if (p < 0.001) "p < 0.001" else paste("p =", formatC(p, digits = 3, format = "f"))
}

# --- ROC / AUC for binary (0/1) models ---------------------------------------
# All computed directly (no extra package needed).

# ROC curve points: one row per distinct threshold, from strictest to loosest
roc_curve <- function(y, p) {
  P <- sum(y == 1); N <- sum(y == 0)
  o  <- order(p, decreasing = TRUE)
  ps <- p[o]; ys <- y[o]
  tp <- cumsum(ys == 1); fp <- cumsum(ys == 0)
  last <- c(diff(ps) != 0, TRUE)          # last row of each tied block
  rbind(data.frame(threshold = Inf, tpr = 0, fpr = 0),
        data.frame(threshold = ps[last], tpr = tp[last] / P, fpr = fp[last] / N))
}

# AUC = probability that a random presence gets a higher prediction than a
# random absence (Mann-Whitney formulation; ties count one half)
auc_value <- function(y, p) {
  r  <- rank(p)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Standard error of AUC (Hanley & McNeil 1982) and a 95% confidence interval
auc_ci <- function(auc, n1, n0, level = 0.95) {
  q1 <- auc / (2 - auc); q2 <- 2 * auc^2 / (1 + auc)
  se <- sqrt((auc * (1 - auc) + (n1 - 1) * (q1 - auc^2) + (n0 - 1) * (q2 - auc^2)) / (n1 * n0))
  z  <- qnorm(1 - (1 - level) / 2)
  c(se = se, lower = max(0, auc - z * se), upper = min(1, auc + z * se))
}

# Threshold that maximises sensitivity + specificity - 1 (Youden's J = TSS)
roc_optimal <- function(roc) {
  r <- roc[is.finite(roc$threshold), ]
  r[which.max(r$tpr - r$fpr), ]
}

# Confusion-matrix statistics at a probability threshold
class_stats <- function(y, p, thr) {
  pred <- as.integer(p >= thr)
  TP <- sum(pred == 1 & y == 1); FP <- sum(pred == 1 & y == 0)
  TN <- sum(pred == 0 & y == 0); FN <- sum(pred == 0 & y == 1)
  n  <- TP + FP + TN + FN
  sens <- TP / (TP + FN); spec <- TN / (TN + FP)
  acc  <- (TP + TN) / n
  pe   <- ((TP + FP) * (TP + FN) + (FN + TN) * (FP + TN)) / n^2
  list(TP = TP, FP = FP, TN = TN, FN = FN,
       sensitivity = sens, specificity = spec, accuracy = acc,
       TSS = sens + spec - 1,
       kappa = if (pe < 1) (acc - pe) / (1 - pe) else NA_real_,
       PPV = if (TP + FP > 0) TP / (TP + FP) else NA_real_,
       NPV = if (TN + FN > 0) TN / (TN + FN) else NA_real_)
}

# k-fold cross-validated AUC for an ordinary binomial GLM (folds stratified so
# every fold keeps both presences and absences). Returns NA if not possible.
cv_auc <- function(model, data, resp, k = 5, seed = 42) {
  y <- data[[resp]]
  if (min(table(factor(y, levels = c(0, 1)))) < k) return(NA_real_)
  set.seed(seed)
  folds <- integer(length(y))
  for (cl in c(0, 1)) {
    idx <- which(y == cl)
    folds[idx] <- sample(rep(seq_len(k), length.out = length(idx)))
  }
  p <- rep(NA_real_, length(y))
  for (i in seq_len(k)) {
    tr  <- folds != i
    fit <- tryCatch(suppressWarnings(
      glm(formula(model), data = data[tr, , drop = FALSE], family = family(model))),
      error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    p[!tr] <- tryCatch(predict(fit, newdata = data[!tr, , drop = FALSE], type = "response"),
                       error = function(e) NA_real_)
  }
  if (anyNA(p)) NA_real_ else auc_value(y, p)
}

auc_label <- function(a) {
  if (is.na(a)) "NA" else if (a < 0.6) "poor (barely better than chance)" else
    if (a < 0.7) "weak" else if (a < 0.8) "acceptable" else
      if (a < 0.9) "excellent" else "outstanding"
}

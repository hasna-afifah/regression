# =============================================================================
# Trend Analysis Tool - app.R
# Regression, Correlation, Multiple Regression, GLM
#
# Tool developer : Hasna Afifah with Claude assistancy
# =============================================================================

# --- Packages -----------------------------------------------------------------
library(shiny)
library(bslib)
library(readxl)
library(tidyverse)
library(car)
library(performance)
library(DT)
library(plotly)
library(broom)
library(MASS)

# --- Theme & helpers ----------------------------------------------------------
app_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#2c7a3e",
  "font-size-base" = "0.95rem"
)

family_choices <- c(
  "Gaussian (Normal)" = "gaussian",
  "Poisson (Counts)"  = "poisson",
  "Binomial (0/1)"    = "binomial",
  "Gamma"             = "Gamma",
  "Inverse Gaussian"  = "inverse.gaussian"
)

tidy_model_output <- function(model) {
  broom::tidy(model, conf.int = TRUE) |>
    mutate(across(where(is.numeric), \(x) round(x, 5)))
}

safe_shapiro <- function(resid_vec) {
  n <- length(resid_vec)
  if (n < 3) return(list(statistic = NA, p.value = NA))
  if (n > 5000) {
    set.seed(42)
    resid_vec <- sample(resid_vec, 5000)
  }
  shapiro.test(resid_vec)
}

# Helper: wrap column name in backticks if it has spaces or special chars
bt <- function(x) {
  needs_bt <- grepl("[^A-Za-z0-9._]", x)
  ifelse(needs_bt, paste0("`", x, "`"), x)
}


# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  title    = "Trend Analysis Tool",
  theme    = app_theme,
  fillable = FALSE,
  
  # -- STEP 1: Upload ----------------------------------------------------------
  nav_panel(
    title = "1. Upload Data",
    icon  = icon("upload"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Upload your Excel / CSV file"),
        fileInput("file_upload", label = NULL,
                  accept = c(".xlsx", ".xls", ".csv"),
                  placeholder = "Choose .xlsx / .csv"),
        selectInput("sheet_select", "Sheet (Excel only)", choices = NULL),
        checkboxInput("header_row", "First row is header", value = TRUE),
        hr(),
        tags$small(class = "text-muted",
                   "Each column = one variable. Missing values as NA or empty cells.")
      ),
      card(
        card_header("Data Preview"),
        DTOutput("data_preview"),
        verbatimTextOutput("data_summary_text")
      )
    )
  ),
  
  # -- STEP 2: Explore ---------------------------------------------------------
  nav_panel(
    title = "2. Explore",
    icon  = icon("chart-bar"),
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Exploration Settings"),
        selectInput("explore_var", "Select variable", choices = NULL),
        radioButtons("explore_plot_type", "Plot type",
                     choices = c("Histogram", "Density", "Boxplot", "Bar chart"),
                     selected = "Histogram"),
        conditionalPanel(
          condition = "input.explore_plot_type == 'Histogram'",
          sliderInput("hist_bins", "Number of bins", 5, 60, 20, step = 1)
        ),
        hr(),
        h6("Bivariate exploration"),
        selectInput("explore_x", "X variable", choices = NULL),
        selectInput("explore_y", "Y variable", choices = NULL)
      ),
      layout_columns(
        col_widths = 12,
        card(
          card_header("Univariate Distribution"),
          plotlyOutput("explore_univariate", height = "350px")
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Scatter Plot (X vs Y)"),
            plotlyOutput("explore_bivariate", height = "350px")
          ),
          card(
            card_header("Correlation Matrix (numeric columns)"),
            plotlyOutput("explore_cormat", height = "350px")
          )
        )
      )
    )
  ),
  
  # -- STEP 3: Model Setup -----------------------------------------------------
  nav_panel(
    title = "3. Model Setup",
    icon  = icon("sliders"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Configure Your Model"),
        selectInput("model_type", "Analysis type",
                    choices = c(
                      "Simple Linear Regression"       = "lm_simple",
                      "Multiple Linear Regression"     = "lm_multiple",
                      "Polynomial Regression"          = "lm_poly",
                      "GLM (Generalised Linear Model)" = "glm"
                    )),
        selectInput("resp_var", "Response variable (Y)", choices = NULL),
        selectizeInput("pred_vars", "Predictor variable(s) (X)",
                       choices = NULL, multiple = TRUE),
        conditionalPanel(
          condition = "input.model_type == 'glm'",
          selectInput("glm_family", "GLM family", choices = family_choices),
          selectInput("glm_link", "Link function",
                      choices = c("default", "log", "logit", "probit",
                                  "inverse", "identity"))
        ),
        conditionalPanel(
          condition = "input.model_type == 'lm_poly'",
          sliderInput("poly_degree", "Polynomial degree", 2, 5, 2, step = 1)
        ),
        checkboxInput("include_interaction", "Include 2-way interactions", FALSE),
        checkboxInput("scale_predictors", "Scale predictor variables", FALSE),
        hr(),
        actionButton("fit_model", "Fit Model",
                     class = "btn-success btn-lg w-100", icon = icon("play"))
      ),
      card(
        card_header("Model Formula Preview"),
        verbatimTextOutput("formula_preview"),
        hr(),
        h6("Variable Summary"),
        DTOutput("var_summary_table")
      )
    )
  ),
  
  # -- STEP 4: Results ---------------------------------------------------------
  nav_panel(
    title = "4. Results",
    icon  = icon("table"),
    layout_columns(
      col_widths = 12,
      card(
        card_header("Model Summary"),
        verbatimTextOutput("model_summary")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Coefficient Table"),
          DTOutput("coef_table")
        ),
        card(
          card_header("Model Fit Statistics"),
          DTOutput("fit_stats_table")
        )
      ),
      conditionalPanel(
        condition = "output.show_vif == true",
        card(
          card_header("Multicollinearity Check (VIF)"),
          DTOutput("vif_table"),
          tags$small(class = "text-muted",
                     "VIF > 5 = potential concern. VIF > 10 = serious collinearity.")
        )
      )
    )
  ),
  
  # -- STEP 5: Diagnostics -----------------------------------------------------
  nav_panel(
    title = "5. Diagnostics",
    icon  = icon("stethoscope"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Q-Q Plot of Residuals"),
        plotlyOutput("qq_plot", height = "380px"),
        verbatimTextOutput("shapiro_result")
      ),
      card(
        card_header("Residuals vs Fitted"),
        plotlyOutput("resid_fitted", height = "380px")
      ),
      card(
        card_header("Residuals vs Predictor(s)"),
        plotlyOutput("resid_predictor", height = "380px")
      ),
      card(
        card_header("Scale-Location"),
        plotlyOutput("scale_location", height = "380px")
      )
    )
  ),
  
  # -- STEP 6: Visualise -------------------------------------------------------
  nav_panel(
    title = "6. Visualise",
    icon  = icon("chart-line"),
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Plot Options"),
        checkboxInput("show_ci", "Show confidence interval", TRUE),
        sliderInput("ci_level", "Confidence level", 0.80, 0.99, 0.95, 0.01),
        checkboxInput("show_points", "Show data points", TRUE),
        conditionalPanel(
          condition = "output.has_multiple_preds",
          selectInput("vis_x_var", "X-axis variable", choices = NULL)
        ),
        hr(),
        downloadButton("download_plot", "Download Plot",
                       class = "btn-outline-success w-100")
      ),
      card(
        card_header("Regression Plot"),
        plotlyOutput("regression_plot", height = "500px")
      )
    )
  ),
  
  # -- About -------------------------------------------------------------------
  nav_panel(
    title = "About",
    icon  = icon("info-circle"),
    card(
      card_header("How to Use This App"),
      tags$div(
        class = "p-3",
        tags$h5("A glimpse"),
        tags$ol(
          tags$li(tags$strong("Upload Data"), " - load your .xlsx or .csv file."),
          tags$li(tags$strong("Explore"), " - histograms, scatter plots, correlations."),
          tags$li(tags$strong("Model Setup"), " - choose model type, response, predictors."),
          tags$li(tags$strong("Results"), " - coefficients, R-squared, AIC, VIF."),
          tags$li(tags$strong("Diagnostics"), " - Q-Q, Shapiro-Wilk, residual plots."),
          tags$li(tags$strong("Visualise"), " - regression line with confidence bands.")
        ),
        hr(),
        tags$h5("Supported models"),
        tags$ul(
          tags$li("Simple & multiple linear regression (lm)"),
          tags$li("Polynomial regression (degree 2-5)"),
          tags$li("GLM: Gaussian, Poisson, Binomial, Gamma, Inverse Gaussian")
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
        
        tags$h5("3. Set up your model"),
        tags$ul(
          tags$li("Pick the response (Y) and one or more predictors (X)."),
          tags$li("For GLM, choose the family (Poisson for counts, Binomial for 0/1, etc.) ",
                  "and link function."),
          tags$li("Optionally scale predictors (recommended when they are on different ",
                  "measurement scales)."),
          tags$li("Optionally include two-way interactions.")
        ),
        
        tags$h5("4. Interpret results"),
        tags$ul(
          tags$li("Coefficient table with estimates, standard errors, t/z-values, p-values, ",
                  "and confidence intervals."),
          tags$li("Fit statistics: R-squared, adjusted R-squared, F-test (for lm), ",
                  "deviance and AIC (for glm)."),
          tags$li("VIF table appears automatically when you have 2+ predictors ",
                  "(VIF > 5 = concern).")
        ),
        
        tags$h5("5. Check diagnostics"),
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
        
        tags$h5("6. Visualise"),
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
                  "is reasonably met."),
          tags$li("For ", tags$strong("logistic regression"), ": ensure the response ",
                  "variable is binary (0/1), then choose GLM -> Binomial."),
          tags$li("For ", tags$strong("Poisson regression"), " (count data): ",
                  "choose GLM -> Poisson."),
          tags$li("For ", tags$strong("Gaussian response curves"),
                  " (e.g. species abundance vs pH): ",
                  "use Polynomial Regression with degree 2."),
          tags$li("Scale your predictors when variables are measured on different units."),
          tags$li("Always check the diagnostic plots before trusting your results.")
        ),
        
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
  
  rv <- reactiveValues(raw_data = NULL, model = NULL)
  
  # -- STEP 1: Upload ----------------------------------------------------------
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    if (ext %in% c("xlsx", "xls")) {
      sheets <- readxl::excel_sheets(input$file_upload$datapath)
      updateSelectInput(session, "sheet_select", choices = sheets)
    } else {
      updateSelectInput(session, "sheet_select", choices = "N/A (CSV)")
    }
  })
  
  loaded_data <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    tryCatch({
      if (ext == "csv") {
        read.csv(input$file_upload$datapath, header = input$header_row)
      } else {
        readxl::read_excel(input$file_upload$datapath,
                           sheet = input$sheet_select,
                           col_names = input$header_row)
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })
  })
  
  observe({
    df <- loaded_data()
    req(df)
    rv$raw_data <- df
    cols     <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    updateSelectInput(session, "explore_var", choices = cols)
    updateSelectInput(session, "explore_x",   choices = num_cols)
    updateSelectInput(session, "explore_y",   choices = num_cols,
                      selected = num_cols[min(2, length(num_cols))])
    updateSelectInput(session, "resp_var",     choices = cols)
    updateSelectizeInput(session, "pred_vars", choices = cols)
    updateSelectInput(session, "vis_x_var",    choices = num_cols)
  })
  
  output$data_preview <- renderDT({
    req(rv$raw_data)
    datatable(rv$raw_data,
              options  = list(pageLength = 8, scrollX = TRUE),
              rownames = FALSE, class = "compact stripe")
  })
  
  output$data_summary_text <- renderPrint({
    req(rv$raw_data)
    cat(nrow(rv$raw_data), "rows x", ncol(rv$raw_data), "columns\n\n")
    str(rv$raw_data, give.attr = FALSE)
  })
  
  # -- STEP 2: Explore ---------------------------------------------------------
  
  output$explore_univariate <- renderPlotly({
    req(rv$raw_data, input$explore_var)
    df <- rv$raw_data
    v  <- input$explore_var
    
    if (is.numeric(df[[v]])) {
      p <- switch(input$explore_plot_type,
                  "Histogram" = ggplot(df, aes(x = .data[[v]])) +
                    geom_histogram(bins = input$hist_bins, fill = "#2c7a3e",
                                   colour = "white", alpha = 0.8) +
                    labs(title = paste("Histogram of", v), x = v, y = "Count"),
                  "Density" = ggplot(df, aes(x = .data[[v]])) +
                    geom_density(fill = "#2c7a3e", alpha = 0.5) +
                    labs(title = paste("Density of", v), x = v, y = "Density"),
                  "Boxplot" = ggplot(df, aes(y = .data[[v]])) +
                    geom_boxplot(fill = "#2c7a3e", alpha = 0.6) +
                    labs(title = paste("Boxplot of", v), y = v),
                  "Bar chart" = ggplot(df, aes(x = .data[[v]])) +
                    geom_histogram(bins = input$hist_bins, fill = "#2c7a3e", alpha = 0.8) +
                    labs(title = paste("Bar chart of", v), x = v, y = "Count")
      )
    } else {
      p <- ggplot(df, aes(x = factor(.data[[v]]))) +
        geom_bar(fill = "#2c7a3e", alpha = 0.8) +
        labs(title = paste("Bar chart of", v), x = v, y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    ggplotly(p + theme_minimal())
  })
  
  output$explore_bivariate <- renderPlotly({
    req(rv$raw_data, input$explore_x, input$explore_y)
    df <- rv$raw_data
    p <- ggplot(df, aes(x = .data[[input$explore_x]],
                        y = .data[[input$explore_y]])) +
      geom_point(alpha = 0.6, colour = "#2c7a3e") +
      geom_smooth(method = "loess", se = TRUE, colour = "#d35400",
                  linewidth = 0.8) +
      labs(title = paste(input$explore_y, "vs", input$explore_x)) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$explore_cormat <- renderPlotly({
    req(rv$raw_data)
    # Base R subsetting to avoid MASS::select masking dplyr::select
    num_cols <- sapply(rv$raw_data, is.numeric)
    num_df   <- rv$raw_data[, num_cols, drop = FALSE]
    req(ncol(num_df) >= 2)
    cor_mat  <- cor(num_df, use = "pairwise.complete.obs")
    cor_long <- as.data.frame(as.table(cor_mat))
    names(cor_long) <- c("Var1", "Var2", "Correlation")
    
    p <- ggplot(cor_long, aes(Var1, Var2, fill = Correlation)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = round(Correlation, 2)), size = 3) +
      scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#2c7a3e",
                           midpoint = 0, limits = c(-1, 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = NULL, title = "Correlation Matrix")
    ggplotly(p)
  })
  
  # -- STEP 3: Model Setup (backtick-safe formula builder) ---------------------
  
  model_formula_text <- reactive({
    req(input$resp_var, input$pred_vars)
    y  <- bt(input$resp_var)
    xs <- input$pred_vars
    
    wrap <- function(v) {
      v_safe <- bt(v)
      if (input$scale_predictors) paste0("scale(", v_safe, ")") else v_safe
    }
    
    if (input$model_type == "lm_poly") {
      req(length(xs) == 1)
      terms <- paste(sapply(1:input$poly_degree, function(d) {
        if (d == 1) wrap(xs[1]) else paste0("I(", wrap(xs[1]), "^", d, ")")
      }), collapse = " + ")
    } else {
      terms <- paste(sapply(xs, wrap), collapse = " + ")
      if (input$include_interaction && length(xs) >= 2) {
        wrapped_xs <- sapply(xs, wrap)
        combos <- combn(wrapped_xs, 2,
                        FUN = function(pair) paste(pair, collapse = ":"))
        terms <- paste(c(terms, combos), collapse = " + ")
      }
    }
    paste(y, "~", terms)
  })
  
  output$formula_preview <- renderPrint({
    tryCatch({
      f <- model_formula_text()
      fam_text <- ""
      if (input$model_type == "glm") {
        fam_text <- paste0("  [family = ", input$glm_family, "]")
      }
      cat("Formula:\n", f, fam_text, "\n")
    }, error = function(e) {
      cat("Select response and predictor variables above.")
    })
  })
  
  output$var_summary_table <- renderDT({
    req(rv$raw_data, input$pred_vars, input$resp_var)
    vars <- unique(c(input$resp_var, input$pred_vars))
    df   <- rv$raw_data[, vars, drop = FALSE]
    
    calc_mean <- function(x) {
      if (is.numeric(x)) round(mean(x, na.rm = TRUE), 3) else NA
    }
    calc_sd <- function(x) {
      if (is.numeric(x)) round(sd(x, na.rm = TRUE), 3) else NA
    }
    
    summ <- data.frame(
      Variable = vars,
      Type     = sapply(df, function(x) class(x)[1]),
      N        = sapply(df, function(x) sum(!is.na(x))),
      Missing  = sapply(df, function(x) sum(is.na(x))),
      Mean     = sapply(df, calc_mean),
      SD       = sapply(df, calc_sd)
    )
    datatable(summ, rownames = FALSE,
              options = list(dom = "t", pageLength = 20),
              class = "compact stripe")
  })
  
  # -- Fit model ---------------------------------------------------------------
  
  observeEvent(input$fit_model, {
    req(rv$raw_data, input$resp_var, input$pred_vars)
    df <- rv$raw_data
    complete <- complete.cases(df[, c(input$resp_var, input$pred_vars)])
    df <- df[complete, ]
    f  <- as.formula(model_formula_text())
    
    tryCatch({
      if (input$model_type %in% c("lm_simple", "lm_multiple", "lm_poly")) {
        rv$model <- lm(f, data = df)
      } else {
        fam <- input$glm_family
        lnk <- input$glm_link
        if (lnk == "default") {
          fam_obj <- do.call(fam, list())
        } else {
          fam_obj <- do.call(fam, list(link = lnk))
        }
        rv$model <- glm(f, data = df, family = fam_obj)
      }
      showNotification("Model fitted successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Model error:", e$message), type = "error")
    })
  })
  
  # -- STEP 4: Results ---------------------------------------------------------
  
  output$model_summary <- renderPrint({
    req(rv$model)
    summary(rv$model)
  })
  
  output$coef_table <- renderDT({
    req(rv$model)
    tbl <- tidy_model_output(rv$model)
    datatable(tbl, rownames = FALSE,
              options = list(dom = "t", pageLength = 20),
              class = "compact stripe") |>
      formatStyle("p.value",
                  backgroundColor = styleInterval(
                    c(0.001, 0.01, 0.05),
                    c("#c6efce", "#c6efce", "#ffeb9c", "#ffc7ce")))
  })
  
  output$fit_stats_table <- renderDT({
    req(rv$model)
    m <- rv$model
    if (inherits(m, "lm") && !inherits(m, "glm")) {
      s <- summary(m)
      stats_list <- list(
        "R-squared"         = round(s$r.squared, 4),
        "Adjusted R-sq"     = round(s$adj.r.squared, 4),
        "F-statistic"       = round(s$fstatistic[1], 3),
        "p-value (F)"       = format.pval(pf(s$fstatistic[1], s$fstatistic[2],
                                             s$fstatistic[3], lower.tail = FALSE)),
        "RSE"               = round(s$sigma, 4),
        "df (residual)"     = s$df[2],
        "AIC"               = round(AIC(m), 2),
        "BIC"               = round(BIC(m), 2)
      )
    } else {
      s <- summary(m)
      stats_list <- list(
        "Null deviance"     = round(s$null.deviance, 3),
        "Residual deviance" = round(s$deviance, 3),
        "AIC"               = round(s$aic, 2),
        "BIC"               = round(BIC(m), 2),
        "df (residual)"     = s$df.residual
      )
    }
    tbl <- data.frame(Metric = names(stats_list),
                      Value  = as.character(unlist(stats_list)))
    datatable(tbl, rownames = FALSE,
              options = list(dom = "t", pageLength = 20),
              class = "compact stripe")
  })
  
  output$show_vif <- reactive({
    req(rv$model)
    length(input$pred_vars) >= 2
  })
  outputOptions(output, "show_vif", suspendWhenHidden = FALSE)
  
  output$vif_table <- renderDT({
    req(rv$model, length(input$pred_vars) >= 2)
    tryCatch({
      v <- car::vif(rv$model)
      if (is.matrix(v)) {
        tbl <- as.data.frame(v)
        tbl$Term <- rownames(tbl)
        tbl <- tbl[, c("Term", setdiff(names(tbl), "Term")), drop = FALSE]
      } else {
        tbl <- data.frame(Term = names(v), VIF = round(v, 3))
      }
      datatable(tbl, rownames = FALSE,
                options = list(dom = "t"), class = "compact stripe") |>
        formatStyle("VIF",
                    backgroundColor = styleInterval(c(5, 10),
                                                    c("#c6efce", "#ffeb9c", "#ffc7ce")))
    }, error = function(e) {
      datatable(data.frame(Note = paste("VIF not available:", e$message)),
                rownames = FALSE, options = list(dom = "t"))
    })
  })
  
  # -- STEP 5: Diagnostics ----------------------------------------------------
  
  output$qq_plot <- renderPlotly({
    req(rv$model)
    resids  <- residuals(rv$model)
    qq_vals <- qqnorm(resids, plot.it = FALSE)
    qq_data <- data.frame(theoretical = qq_vals$x, sample = qq_vals$y)
    p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
      geom_point(colour = "#2c7a3e", alpha = 0.7) +
      geom_abline(slope = sd(resids), intercept = mean(resids),
                  colour = "#c0392b", linetype = "dashed") +
      labs(title = "Normal Q-Q Plot",
           x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$shapiro_result <- renderPrint({
    req(rv$model)
    sw <- safe_shapiro(residuals(rv$model))
    cat("Shapiro-Wilk normality test\n")
    cat("W =", round(sw$statistic, 4),
        "  p-value =", format.pval(sw$p.value), "\n")
    if (sw$p.value < 0.05) {
      cat("=> Residuals deviate significantly from normality (p < 0.05).\n")
    } else {
      cat("=> No significant departure from normality (p >= 0.05).\n")
    }
  })
  
  output$resid_fitted <- renderPlotly({
    req(rv$model)
    df <- data.frame(fitted = fitted(rv$model),
                     residuals = residuals(rv$model))
    p <- ggplot(df, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6, colour = "#2c7a3e") +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#c0392b") +
      geom_smooth(method = "loess", se = FALSE, colour = "#d35400",
                  linewidth = 0.7) +
      labs(title = "Residuals vs Fitted",
           x = "Fitted Values", y = "Residuals") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$resid_predictor <- renderPlotly({
    req(rv$model, input$pred_vars)
    pred1      <- input$pred_vars[1]
    actual_col <- rv$raw_data[[pred1]]
    req(actual_col)
    n <- length(residuals(rv$model))
    plot_df <- data.frame(x = actual_col[1:n],
                          residuals = residuals(rv$model))
    p <- ggplot(plot_df, aes(x = x, y = residuals)) +
      geom_point(alpha = 0.6, colour = "#2c7a3e") +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#c0392b") +
      geom_smooth(method = "loess", se = FALSE, colour = "#d35400",
                  linewidth = 0.7) +
      labs(title = paste("Residuals vs", pred1),
           x = pred1, y = "Residuals") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$scale_location <- renderPlotly({
    req(rv$model)
    df <- data.frame(fitted         = fitted(rv$model),
                     sqrt_abs_resid = sqrt(abs(rstandard(rv$model))))
    p <- ggplot(df, aes(x = fitted, y = sqrt_abs_resid)) +
      geom_point(alpha = 0.6, colour = "#2c7a3e") +
      geom_smooth(method = "loess", se = FALSE, colour = "#d35400",
                  linewidth = 0.7) +
      labs(title = "Scale-Location", x = "Fitted Values",
           y = "sqrt(|Std. Residuals|)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # -- STEP 6: Visualise ------------------------------------------------------
  
  output$has_multiple_preds <- reactive({
    length(input$pred_vars) > 1
  })
  outputOptions(output, "has_multiple_preds", suspendWhenHidden = FALSE)
  
  observe({
    req(input$pred_vars)
    updateSelectInput(session, "vis_x_var", choices = input$pred_vars,
                      selected = input$pred_vars[1])
  })
  
  reg_plot <- reactive({
    req(rv$model, rv$raw_data, input$pred_vars, input$resp_var)
    
    all_vars <- c(input$resp_var, input$pred_vars)
    df <- rv$raw_data
    complete <- complete.cases(df[, all_vars])
    df <- df[complete, ]
    
    x_var <- if (length(input$pred_vars) == 1) {
      input$pred_vars[1]
    } else {
      input$vis_x_var
    }
    req(x_var)
    
    if (length(input$pred_vars) == 1 || input$model_type == "lm_poly") {
      newdata <- data.frame(
        x = seq(min(df[[x_var]], na.rm = TRUE),
                max(df[[x_var]], na.rm = TRUE), length.out = 200)
      )
      names(newdata) <- x_var
      pred_out    <- predict(rv$model, newdata = newdata,
                             type = "response", se.fit = TRUE)
      newdata$fit <- pred_out$fit
      
      if (!is.null(pred_out$se.fit)) {
        z <- qnorm(1 - (1 - input$ci_level) / 2)
        newdata$lwr <- pred_out$fit - z * pred_out$se.fit
        newdata$upr <- pred_out$fit + z * pred_out$se.fit
      }
      
      p <- ggplot()
      if (input$show_points) {
        p <- p + geom_point(
          data = df,
          aes(x = .data[[x_var]], y = .data[[input$resp_var]]),
          alpha = 0.6, colour = "#2c7a3e")
      }
      p <- p + geom_line(data = newdata,
                         aes(x = .data[[x_var]], y = fit),
                         colour = "#c0392b", linewidth = 1)
      if (input$show_ci && "lwr" %in% names(newdata)) {
        p <- p + geom_ribbon(data = newdata,
                             aes(x = .data[[x_var]], ymin = lwr, ymax = upr),
                             alpha = 0.15, fill = "#c0392b")
      }
      p <- p + labs(title = paste(input$resp_var, "~", x_var),
                    x = x_var, y = input$resp_var) + theme_minimal()
      
    } else {
      p <- ggplot(df, aes(x = .data[[x_var]],
                          y = .data[[input$resp_var]])) +
        geom_point(alpha = 0.6, colour = "#2c7a3e") +
        geom_smooth(method = "lm", se = input$show_ci,
                    level = input$ci_level, colour = "#c0392b") +
        labs(title = paste(input$resp_var, "vs", x_var, "(partial view)"),
             x = x_var, y = input$resp_var) +
        theme_minimal()
    }
    p
  })
  
  output$regression_plot <- renderPlotly({
    ggplotly(reg_plot())
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("regression_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = reg_plot(), width = 10, height = 6, dpi = 150)
    }
  )
}


# =============================================================================
# RUN
# =============================================================================
shinyApp(ui = ui, server = server)

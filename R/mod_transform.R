# =============================================================================
# mod_transform.R - Step 3: check normality and transform a variable
# =============================================================================

transformUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),
    tags$div(
      card(
        card_header("Normality & Transformation"),
        selectInput(ns("var"), "Numeric variable", choices = NULL),
        selectInput(ns("method"), "Transformation", choices = transform_choices),
        sliderInput(ns("bins"), "Histogram bins", 5, 50, 20, step = 1),
        hr(),
        textInput(ns("new_name"), "Name of new column", value = ""),
        actionButton(ns("add_col"), "Add transformed column to data",
                     class = "btn-success w-100", icon = icon("plus")),
        tags$small(class = "text-muted mt-2",
                   "The new column becomes available in every other tab.")
      ),
      card(
        card_header("Which transformation?"),
        tags$ul(
          class = "small ps-3 mb-1",
          tags$li(tags$b("Right-skewed, positive"), " (biomass, DBH, basal area): log, or square root if the skew is mild."),
          tags$li(tags$b("Counts"), " (stems, seedlings): square root or ln(x + 1) - but a Poisson / negative binomial GLM is usually better."),
          tags$li(tags$b("Proportions / % cover"), ": logit or arcsine square-root - or a binomial GLM."),
          tags$li(tags$b("Left-skewed"), ": square."),
          tags$li(tags$b("Not sure"), ": Box-Cox (x > 0) or Yeo-Johnson pick the power for you.")
        ),
        tags$small(class = "text-muted",
                   "Remember: t-tests, ANOVA and linear regression assume normal ",
                   tags$em("residuals"), ", not a normal raw variable. Always re-check ",
                   "the model diagnostics after transforming.")
      )
    ),
    layout_columns(
      col_widths = 12,
      card(
        card_header("Before vs after"),
        plotOutput(ns("hist"), height = "240px"),
        plotOutput(ns("qq"), height = "240px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Normality check: original vs transformed"),
          DTOutput(ns("stats")),
          uiOutput(ns("verdict"))
        ),
        card(
          card_header("All transformations ranked (highest Shapiro W = closest to normal)"),
          DTOutput(ns("ranking"))
        )
      )
    )
  )
}

transformServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      df <- rv$data
      req(df)
      updateSelectInput(session, "var", choices = num_cols(df),
                        selected = isolate(input$var))
    })

    observe({
      req(input$var, input$method)
      updateTextInput(session, "new_name",
                      value = paste0(input$var, "_", transform_suffix[[input$method]]))
    })

    x_raw <- reactive({
      req(rv$data, input$var %in% names(rv$data))
      x <- rv$data[[input$var]]
      validate(need(is.numeric(x), "Please choose a numeric variable."))
      x
    })

    tr <- reactive(apply_transform(x_raw(), input$method))

    plot_data <- reactive({
      x <- x_raw()
      t <- tr()
      validate(need(t$ok, t$note))
      lab_t <- names(transform_choices)[transform_choices == input$method]
      d <- rbind(
        data.frame(value = x[is.finite(x)], version = "Original"),
        data.frame(value = t$values[is.finite(t$values)],
                   version = paste("Transformed -", lab_t))
      )
      d$version <- factor(d$version, levels = unique(d$version))
      d
    })

    output$hist <- renderPlot({
      d <- plot_data()
      # normal curve with the same mean / sd, per panel
      curve_df <- do.call(rbind, lapply(split(d, d$version), function(s) {
        xs <- seq(min(s$value), max(s$value), length.out = 200)
        data.frame(value = xs, dens = dnorm(xs, mean(s$value), sd(s$value)),
                   version = s$version[1])
      }))
      ggplot(d, aes(x = value)) +
        geom_histogram(aes(y = after_stat(density)), bins = input$bins,
                       fill = eco_green, colour = "white", alpha = 0.8) +
        geom_line(data = curve_df, aes(y = dens), colour = eco_red, linewidth = 1) +
        facet_wrap(~version, scales = "free") +
        labs(x = NULL, y = "Density",
             subtitle = "Histogram with the matching normal curve (red)") +
        eco_theme()
    })

    output$qq <- renderPlot({
      ggplot(plot_data(), aes(sample = value)) +
        stat_qq(colour = eco_green, alpha = 0.7) +
        stat_qq_line(colour = eco_red, linetype = "dashed") +
        facet_wrap(~version, scales = "free") +
        labs(x = "Theoretical quantiles", y = "Sample quantiles",
             subtitle = "Q-Q plot: points on the dashed line = normal") +
        eco_theme()
    })

    output$stats <- renderDT({
      t <- tr()
      validate(need(t$ok, t$note))
      tbl <- rbind(normality_row(x_raw(), "Original"),
                   normality_row(t$values, "Transformed"))
      simple_dt(round_df(tbl, 4)) |>
        formatStyle("Shapiro p",
                    backgroundColor = styleInterval(0.05, c("#ffc7ce", "#c6efce")))
    })

    output$verdict <- renderUI({
      t <- tr()
      req(t$ok)
      p0 <- safe_shapiro(x_raw())$p.value
      p1 <- safe_shapiro(t$values)$p.value
      s0 <- skewness(x_raw()); s1 <- skewness(t$values)
      msg <- if (is.na(p1)) {
        "Shapiro-Wilk could not be computed."
      } else if (p1 >= 0.05) {
        sprintf("After transformation there is no significant departure from normality (%s). Skewness went from %.2f to %.2f.",
                fmt_peq(p1), s0, s1)
      } else if (!is.na(p0) && p0 >= 0.05) {
        sprintf("The original variable already looked normal (%s); this transformation makes it worse.", fmt_peq(p0))
      } else {
        sprintf("Still significantly non-normal after transformation (%s; skewness %.2f -> %.2f). Try another option from the ranking, a non-parametric test, or a GLM.",
                fmt_peq(p1), s0, s1)
      }
      tagList(
        help_note(msg),
        if (nzchar(t$note)) tags$small(class = "text-muted", t$note),
        tags$small(class = "text-muted d-block",
                   "Shapiro-Wilk: p >= 0.05 (green) = no evidence against normality. ",
                   "With large samples it flags tiny deviations - trust the Q-Q plot too.")
      )
    })

    output$ranking <- renderDT({
      x <- x_raw()
      rows <- list(cbind(Transformation = "None (original)",
                         normality_row(x, "")[, c("Skewness", "Shapiro W", "Shapiro p")]))
      for (i in seq_along(transform_choices)) {
        t <- apply_transform(x, transform_choices[[i]])
        if (t$ok) {
          rows[[length(rows) + 1]] <- cbind(
            Transformation = names(transform_choices)[i],
            normality_row(t$values, "")[, c("Skewness", "Shapiro W", "Shapiro p")])
        }
      }
      tbl <- do.call(rbind, rows)
      tbl <- tbl[order(-tbl$`Shapiro W`), ]
      simple_dt(round_df(tbl, 4)) |>
        formatStyle("Shapiro p",
                    backgroundColor = styleInterval(0.05, c("#ffc7ce", "#c6efce")))
    })

    observeEvent(input$add_col, {
      req(rv$data)
      t <- tr()
      if (!t$ok) {
        showNotification(t$note, type = "error")
        return()
      }
      nm <- trimws(input$new_name)
      if (!nzchar(nm)) {
        showNotification("Please give the new column a name.", type = "error")
        return()
      }
      df <- as.data.frame(rv$data, check.names = FALSE)
      df[[nm]] <- t$values
      rv$data <- df
      showNotification(paste0("Column '", nm, "' added to the data."), type = "message")
    })
  })
}

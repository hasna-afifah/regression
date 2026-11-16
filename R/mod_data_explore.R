# =============================================================================
# mod_data_explore.R - Step 1 (load data) and Step 2 (explore)
# `rv` is the app-wide reactiveValues holding rv$data (the working data frame)
# =============================================================================

# ---- 1. Data -----------------------------------------------------------------
dataUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    tags$div(
      card(
        card_header("Upload your Excel / CSV file"),
        fileInput(ns("file_upload"), label = NULL,
                  accept = c(".xlsx", ".xls", ".csv"),
                  placeholder = "Choose .xlsx / .csv"),
        selectInput(ns("sheet_select"), "Sheet (Excel only)", choices = NULL),
        checkboxInput(ns("header_row"), "First row is header", value = TRUE),
        tags$small(class = "text-muted",
                   "Each column = one variable, each row = one observation ",
                   "(plot, tree, transect...). Missing values as NA or empty cells.")
      ),
      card(
        card_header("...or try an example dataset"),
        selectInput(ns("example_file"), NULL, choices = example_datasets),
        actionButton(ns("load_example"), "Load example data",
                     class = "btn-success w-100", icon = icon("tree")),
        tags$small(class = "text-muted mt-2",
                   "Simulated forest-ecology data used throughout the teaching ",
                   "module (see the module/ folder).")
      )
    ),
    card(
      card_header(textOutput(ns("data_title"), inline = TRUE)),
      DTOutput(ns("data_preview")),
      verbatimTextOutput(ns("data_summary_text"))
    )
  )
}

dataServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$file_upload, {
      ext <- tolower(tools::file_ext(input$file_upload$name))
      if (ext %in% c("xlsx", "xls")) {
        sheets <- readxl::excel_sheets(input$file_upload$datapath)
        updateSelectInput(session, "sheet_select", choices = sheets)
      } else {
        updateSelectInput(session, "sheet_select", choices = "N/A (CSV)")
      }
    })

    observe({
      req(input$file_upload)
      ext <- tolower(tools::file_ext(input$file_upload$name))
      header <- input$header_row
      sheet  <- input$sheet_select
      df <- tryCatch({
        if (ext == "csv") {
          read.csv(input$file_upload$datapath, header = header,
                   check.names = FALSE, stringsAsFactors = FALSE)
        } else {
          req(sheet, sheet != "N/A (CSV)")
          as.data.frame(readxl::read_excel(input$file_upload$datapath,
                                           sheet = sheet, col_names = header))
        }
      }, error = function(e) {
        if (!inherits(e, "shiny.silent.error"))
          showNotification(paste("Error reading file:", e$message), type = "error")
        NULL
      })
      req(df)
      rv$data <- df
      rv$name <- input$file_upload$name
    })

    observeEvent(input$load_example, {
      df <- load_example(input$example_file)
      if (is.null(df)) {
        showNotification("Example file not found in the data/ folder.", type = "error")
      } else {
        rv$data <- df
        rv$name <- names(example_datasets)[example_datasets == input$example_file]
        showNotification("Example data loaded.", type = "message")
      }
    })

    output$data_title <- renderText({
      if (is.null(rv$data)) "Data Preview" else paste("Data Preview -", rv$name)
    })

    output$data_preview <- renderDT({
      req(rv$data)
      datatable(rv$data,
                options  = list(pageLength = 8, scrollX = TRUE),
                rownames = FALSE, class = "compact stripe")
    })

    output$data_summary_text <- renderPrint({
      req(rv$data)
      cat(nrow(rv$data), "rows x", ncol(rv$data), "columns\n\n")
      str(as.data.frame(rv$data), give.attr = FALSE)
    })
  })
}

# ---- 2. Explore --------------------------------------------------------------
exploreUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),
    card(
      card_header("Exploration Settings"),
      selectInput(ns("explore_var"), "Select variable", choices = NULL),
      radioButtons(ns("explore_plot_type"), "Plot type",
                   choices = c("Histogram", "Density", "Boxplot", "Bar chart"),
                   selected = "Histogram"),
      conditionalPanel(
        condition = "input.explore_plot_type == 'Histogram' || input.explore_plot_type == 'Bar chart'", ns = ns,
        sliderInput(ns("hist_bins"), "Number of bins", 5, 60, 20, step = 1)
      ),
      selectInput(ns("explore_group"), "Split by group (optional)", choices = "None"),
      hr(),
      h6("Bivariate exploration"),
      selectInput(ns("explore_x"), "X variable", choices = NULL),
      selectInput(ns("explore_y"), "Y variable", choices = NULL)
    ),
    layout_columns(
      col_widths = 12,
      card(
        card_header("Univariate Distribution"),
        plotlyOutput(ns("explore_univariate"), height = "350px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Scatter Plot (X vs Y)"),
          plotlyOutput(ns("explore_bivariate"), height = "350px")
        ),
        card(
          card_header("Correlation Matrix (numeric columns, Pearson)"),
          plotlyOutput(ns("explore_cormat"), height = "350px")
        )
      )
    )
  )
}

exploreServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      df <- rv$data
      req(df)
      nums <- num_cols(df)
      updateSelectInput(session, "explore_var", choices = names(df),
                        selected = isolate(input$explore_var))
      updateSelectInput(session, "explore_group", choices = c("None", cat_cols(df)))
      updateSelectInput(session, "explore_x", choices = nums,
                        selected = isolate(input$explore_x))
      updateSelectInput(session, "explore_y", choices = nums,
                        selected = if (isTRUE(isolate(input$explore_y) %in% nums))
                          isolate(input$explore_y) else nums[min(2, length(nums))])
    })

    output$explore_univariate <- renderPlotly({
      req(rv$data, input$explore_var %in% names(rv$data))
      df <- as.data.frame(rv$data)
      v  <- input$explore_var
      g  <- input$explore_group
      has_g <- !is.null(g) && g != "None" && g %in% names(df) && g != v
      if (has_g) df$.grp <- factor(df[[g]])

      if (is.numeric(df[[v]])) {
        aes_fill <- if (has_g) aes(fill = .grp) else aes()
        p <- switch(
          input$explore_plot_type,
          "Histogram" = ggplot(df, aes(x = .data[[v]])) + aes_fill +
            geom_histogram(bins = input$hist_bins, colour = "white", alpha = 0.8,
                           position = "identity") +
            labs(title = paste("Histogram of", v), x = v, y = "Count"),
          "Density" = ggplot(df, aes(x = .data[[v]])) + aes_fill +
            geom_density(alpha = 0.5) +
            labs(title = paste("Density of", v), x = v, y = "Density"),
          "Bar chart" = ggplot(df, aes(x = .data[[v]])) + aes_fill +
            geom_histogram(bins = input$hist_bins, alpha = 0.8, position = "identity") +
            labs(title = paste("Bar chart of", v), x = v, y = "Count"),
          "Boxplot" = if (has_g) {
            ggplot(df, aes(x = .grp, y = .data[[v]], fill = .grp)) +
              geom_boxplot(alpha = 0.6) +
              labs(title = paste("Boxplot of", v, "by", g), x = g, y = v)
          } else {
            ggplot(df, aes(x = "", y = .data[[v]])) +
              geom_boxplot(alpha = 0.6) +
              labs(title = paste("Boxplot of", v), x = NULL, y = v)
          }
        )
        if (has_g) {
          p <- p + scale_fill_manual(values = rep(eco_palette, length.out = nlevels(df$.grp)),
                                     name = g)
        } else {
          p$layers[[1]]$aes_params$fill <- eco_green
        }
      } else {
        p <- ggplot(df, aes(x = factor(.data[[v]]))) +
          geom_bar(fill = eco_green, alpha = 0.8) +
          labs(title = paste("Bar chart of", v), x = v, y = "Count") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      ggplotly(p + theme_minimal())
    })

    output$explore_bivariate <- renderPlotly({
      req(rv$data, input$explore_x, input$explore_y)
      df <- as.data.frame(rv$data)
      req(all(c(input$explore_x, input$explore_y) %in% names(df)))
      p <- ggplot(df, aes(x = .data[[input$explore_x]],
                          y = .data[[input$explore_y]])) +
        geom_point(alpha = 0.6, colour = eco_green) +
        geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                    colour = eco_orange, linewidth = 0.8) +
        labs(title = paste(input$explore_y, "vs", input$explore_x)) +
        theme_minimal()
      ggplotly(p)
    })

    output$explore_cormat <- renderPlotly({
      req(rv$data)
      df <- as.data.frame(rv$data)
      num_df <- df[, num_cols(df), drop = FALSE]
      validate(need(ncol(num_df) >= 2, "Need at least two numeric columns."))
      cor_mat  <- cor(num_df, use = "pairwise.complete.obs")
      cor_long <- as.data.frame(as.table(cor_mat))
      names(cor_long) <- c("Var1", "Var2", "Correlation")
      p <- ggplot(cor_long, aes(Var1, Var2, fill = Correlation)) +
        geom_tile(colour = "white") +
        scale_fill_gradient2(low = eco_red, mid = "white", high = eco_green,
                             midpoint = 0, limits = c(-1, 1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = NULL, y = NULL)
      if (ncol(num_df) <= 10) p <- p + geom_text(aes(label = round(Correlation, 2)), size = 3)
      ggplotly(p)
    })
  })
}

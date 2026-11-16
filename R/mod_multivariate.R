# =============================================================================
# mod_multivariate.R - Step 7: PCA, NMDS + PERMANOVA, cluster analysis
# =============================================================================

has_vegan <- function() requireNamespace("vegan", quietly = TRUE)

multivariateUI <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    title = "Multivariate analysis",

    # ---- PCA -----------------------------------------------------------------
    nav_panel(
      "PCA",
      layout_columns(
        col_widths = c(3, 9),
        tags$div(
          selectizeInput(ns("p_vars"), "Numeric variables (3 or more)", choices = NULL, multiple = TRUE),
          actionLink(ns("p_all"), "Select all numeric columns"),
          checkboxInput(ns("p_scale"), "Standardise variables (recommended when units differ)", TRUE),
          selectInput(ns("p_group"), "Colour points by", choices = "None"),
          fluidRow(
            column(6, selectInput(ns("p_x"), "X axis", choices = "PC1")),
            column(6, selectInput(ns("p_y"), "Y axis", choices = "PC2"))
          ),
          checkboxInput(ns("p_ellipse"), "Draw group ellipses", TRUE),
          hr(),
          actionButton(ns("p_add"), "Add PC1 & PC2 scores to data",
                       class = "btn-outline-success w-100", icon = icon("plus")),
          tags$small(
            class = "text-muted mt-2",
            "PCA summarises many correlated ", tags$b("continuous"), " variables ",
            "(soil chemistry, stand structure, climate) into a few uncorrelated axes. ",
            "Use the scores as predictors in a regression to avoid collinearity. ",
            "For species abundance tables use NMDS instead.")
        ),
        tagList(
          layout_columns(
            col_widths = c(7, 5),
            card(card_header("Biplot (points = observations, arrows = variables)"),
                 plotOutput(ns("p_biplot"), height = "430px")),
            card(card_header("Scree plot (variance explained)"),
                 plotOutput(ns("p_scree"), height = "430px"))
          ),
          uiOutput(ns("p_interp")),
          layout_columns(
            col_widths = c(7, 5),
            card(card_header("Loadings (how each variable contributes to each axis)"),
                 DTOutput(ns("p_load"))),
            card(card_header("Importance of components"), DTOutput(ns("p_imp")))
          )
        )
      )
    ),

    # ---- NMDS ----------------------------------------------------------------
    nav_panel(
      "NMDS + PERMANOVA",
      layout_columns(
        col_widths = c(3, 9),
        tags$div(
          selectizeInput(ns("n_vars"), "Species / abundance columns", choices = NULL, multiple = TRUE),
          actionLink(ns("n_all"), "Select all numeric columns"),
          selectInput(ns("n_trans"), "Pre-transformation",
                      choices = c("None" = "none", "Square root (down-weights dominant species)" = "sqrt",
                                  "Presence / absence" = "pa")),
          selectInput(ns("n_dist"), "Dissimilarity",
                      choices = c("Bray-Curtis (abundance)" = "bray",
                                  "Jaccard" = "jaccard", "Euclidean" = "euclidean")),
          selectInput(ns("n_group"), "Grouping variable (colour + PERMANOVA)", choices = "None"),
          checkboxInput(ns("n_species"), "Show species labels", TRUE),
          actionButton(ns("n_run"), "Run NMDS", class = "btn-success w-100", icon = icon("play")),
          tags$small(
            class = "text-muted mt-2",
            "NMDS maps plots so that plots with similar species composition sit ",
            "close together. PERMANOVA then tests whether composition differs ",
            "between groups. Rows = plots, columns = species counts or cover.")
        ),
        tagList(
          uiOutput(ns("n_msg")),
          layout_columns(
            col_widths = c(7, 5),
            card(card_header("NMDS ordination"), plotOutput(ns("n_plot"), height = "430px")),
            card(card_header("PERMANOVA (adonis2, 999 permutations)"),
                 verbatimTextOutput(ns("n_perm")),
                 uiOutput(ns("n_interp")))
          )
        )
      )
    ),

    # ---- Cluster -------------------------------------------------------------
    nav_panel(
      "Cluster analysis",
      layout_columns(
        col_widths = c(3, 9),
        tags$div(
          selectizeInput(ns("k_vars"), "Numeric variables", choices = NULL, multiple = TRUE),
          actionLink(ns("k_all"), "Select all numeric columns"),
          selectInput(ns("k_dist"), "Distance",
                      choices = c("Euclidean on standardised variables (environment)" = "euclid_std",
                                  "Euclidean (raw)" = "euclidean",
                                  "Bray-Curtis (species abundance)" = "bray")),
          selectInput(ns("k_link"), "Linkage",
                      choices = c("Ward (compact groups)" = "ward.D2",
                                  "Average (UPGMA)" = "average",
                                  "Complete" = "complete", "Single" = "single")),
          sliderInput(ns("k_k"), "Number of clusters", 2, 8, 4, step = 1),
          selectInput(ns("k_label"), "Label / compare with", choices = "None"),
          actionButton(ns("k_add"), "Add cluster membership to data",
                       class = "btn-outline-success w-100", icon = icon("plus"))
        ),
        tagList(
          card(card_header("Dendrogram"), plotOutput(ns("k_plot"), height = "430px")),
          card(card_header("Cluster membership vs the chosen label variable"),
               DTOutput(ns("k_tab")))
        )
      )
    )
  )
}

multivariateServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      df <- rv$data
      req(df)
      nums <- num_cols(df); cats <- c("None", cat_cols(df))
      for (i in c("p_vars", "n_vars", "k_vars"))
        updateSelectizeInput(session, i, choices = nums,
                             selected = intersect(isolate(input[[i]]), nums))
      updateSelectInput(session, "p_group", choices = cats,
                        selected = if (isTRUE(isolate(input$p_group) %in% cats)) isolate(input$p_group) else "None")
      updateSelectInput(session, "n_group", choices = cats,
                        selected = if (isTRUE(isolate(input$n_group) %in% cats)) isolate(input$n_group) else "None")
      labs <- c("None", setdiff(names(df), nums), intersect(cat_cols(df), nums))
      updateSelectInput(session, "k_label", choices = unique(labs))
    })

    observeEvent(input$p_all, updateSelectizeInput(session, "p_vars", selected = num_cols(rv$data)))
    observeEvent(input$n_all, updateSelectizeInput(session, "n_vars", selected = num_cols(rv$data)))
    observeEvent(input$k_all, updateSelectizeInput(session, "k_vars", selected = num_cols(rv$data)))

    # ---- PCA -----------------------------------------------------------------
    pca <- reactive({
      req(rv$data)
      df <- as.data.frame(rv$data, check.names = FALSE)
      vars <- intersect(input$p_vars, num_cols(df))
      validate(need(length(vars) >= 3, "Select at least 3 numeric variables."))
      keep <- complete.cases(df[, vars, drop = FALSE])
      X <- df[keep, vars, drop = FALSE]
      const <- vapply(X, function(x) sd(x) == 0, logical(1))
      validate(need(!any(const), paste("Remove constant variable(s):", paste(vars[const], collapse = ", "))),
               need(nrow(X) > length(vars), "Need more complete rows than variables."))
      fit <- prcomp(X, center = TRUE, scale. = isTRUE(input$p_scale))
      list(fit = fit, keep = keep, df = df[keep, , drop = FALSE], vars = vars,
           ve = fit$sdev^2 / sum(fit$sdev^2))
    })

    observe({
      p <- tryCatch(pca(), error = function(e) NULL)
      req(p)
      pcs <- colnames(p$fit$x)
      updateSelectInput(session, "p_x", choices = pcs, selected = "PC1")
      updateSelectInput(session, "p_y", choices = pcs, selected = "PC2")
    })

    output$p_scree <- renderPlot({
      p <- pca()
      d <- data.frame(PC = factor(seq_along(p$ve)), ve = 100 * p$ve, cum = 100 * cumsum(p$ve))
      ggplot(d, aes(x = PC)) +
        geom_col(aes(y = ve), fill = eco_green, alpha = 0.85) +
        geom_line(aes(y = cum, group = 1), colour = eco_orange, linewidth = 0.9) +
        geom_point(aes(y = cum), colour = eco_orange, size = 2.5) +
        geom_text(aes(y = ve, label = sprintf("%.0f%%", ve)), vjust = -0.4, size = 3.6) +
        labs(x = "Principal component", y = "% of variance",
             caption = "Bars = each axis, orange line = cumulative") +
        eco_theme()
    })

    output$p_biplot <- renderPlot({
      p <- pca()
      req(input$p_x %in% colnames(p$fit$x), input$p_y %in% colnames(p$fit$x))
      validate(need(input$p_x != input$p_y, "Choose two different axes."))
      sc <- data.frame(x = p$fit$x[, input$p_x], y = p$fit$x[, input$p_y])
      ld <- data.frame(x = p$fit$rotation[, input$p_x], y = p$fit$rotation[, input$p_y],
                       var = rownames(p$fit$rotation))
      mult <- 0.8 * min(max(abs(sc$x)) / max(abs(ld$x)), max(abs(sc$y)) / max(abs(ld$y)))
      ld$x <- ld$x * mult; ld$y <- ld$y * mult
      g <- input$p_group
      has_g <- !is.null(g) && g != "None" && g %in% names(p$df)
      ix <- match(input$p_x, colnames(p$fit$x)); iy <- match(input$p_y, colnames(p$fit$x))
      plt <- ggplot(sc, aes(x, y)) +
        geom_hline(yintercept = 0, colour = "grey80") + geom_vline(xintercept = 0, colour = "grey80")
      if (has_g) {
        sc$grp <- factor(p$df[[g]])
        plt <- plt + geom_point(data = sc, aes(colour = grp), size = 2.3, alpha = 0.75)
        if (isTRUE(input$p_ellipse) && min(table(sc$grp)) >= 4)
          plt <- plt + stat_ellipse(data = sc, aes(colour = grp), linewidth = 0.7)
        plt <- plt + scale_colour_manual(values = rep(eco_palette, length.out = nlevels(sc$grp)), name = g)
      } else {
        plt <- plt + geom_point(colour = eco_green, size = 2.3, alpha = 0.7)
      }
      plt +
        geom_segment(data = ld, aes(x = 0, y = 0, xend = x, yend = y),
                     arrow = arrow(length = unit(0.22, "cm")), colour = "grey20") +
        geom_text(data = ld, aes(x = x * 1.1, y = y * 1.1, label = var), size = 3.6, colour = "grey10") +
        labs(x = sprintf("%s (%.1f%%)", input$p_x, 100 * p$ve[ix]),
             y = sprintf("%s (%.1f%%)", input$p_y, 100 * p$ve[iy])) +
        eco_theme() + theme(legend.position = "bottom")
    })

    output$p_interp <- renderUI({
      p <- pca()
      top <- function(k) {
        l <- p$fit$rotation[, k]
        o <- order(-abs(l))[seq_len(min(3, length(l)))]
        paste(sprintf("%s (%s%.2f)", names(l)[o], ifelse(l[o] >= 0, "+", ""), l[o]), collapse = ", ")
      }
      n_kaiser <- sum(p$fit$sdev^2 > 1)
      help_note(
        tags$b("In words: "),
        sprintf("PC1 explains %.0f%% and PC2 %.0f%% of the total variance (%.0f%% together). ",
                100 * p$ve[1], 100 * p$ve[2], 100 * sum(p$ve[1:2])),
        sprintf("PC1 is driven mostly by %s; PC2 by %s. ", top(1), top(2)),
        if (isTRUE(input$p_scale)) sprintf("%d component(s) have an eigenvalue above 1 (Kaiser rule of thumb for how many to keep). ", n_kaiser),
        "Arrows pointing the same way = positively correlated variables; opposite = negatively correlated; at right angles = unrelated.")
    })

    output$p_load <- renderDT({
      p <- pca()
      k <- min(5, ncol(p$fit$rotation))
      tbl <- data.frame(Variable = rownames(p$fit$rotation), round(p$fit$rotation[, 1:k, drop = FALSE], 3))
      out <- simple_dt(tbl)
      for (cn in names(tbl)[-1])
        out <- formatStyle(out, cn, backgroundColor = styleInterval(
          c(-0.4, 0.4), c("#f5c6cb", "white", "#c6efce")))
      out
    })

    output$p_imp <- renderDT({
      p <- pca()
      simple_dt(data.frame(Component = colnames(p$fit$x),
                           Eigenvalue = round(p$fit$sdev^2, 3),
                           `% variance` = round(100 * p$ve, 1),
                           `Cumulative %` = round(100 * cumsum(p$ve), 1), check.names = FALSE))
    })

    observeEvent(input$p_add, {
      p <- tryCatch(pca(), error = function(e) NULL)
      if (is.null(p)) {
        showNotification("Run a PCA first (select at least 3 variables).", type = "error")
        return()
      }
      df <- as.data.frame(rv$data, check.names = FALSE)
      df$PC1 <- NA_real_; df$PC2 <- NA_real_
      df$PC1[p$keep] <- p$fit$x[, 1]
      df$PC2[p$keep] <- p$fit$x[, 2]
      rv$data <- df
      showNotification("Columns PC1 and PC2 added to the data.", type = "message")
    })

    # ---- NMDS ----------------------------------------------------------------
    nmds <- eventReactive(input$n_run, {
      validate(need(has_vegan(), "This analysis needs the 'vegan' package: install.packages('vegan')"))
      req(rv$data)
      df <- as.data.frame(rv$data, check.names = FALSE)
      vars <- intersect(input$n_vars, num_cols(df))
      validate(need(length(vars) >= 3, "Select at least 3 species / abundance columns."))
      X <- df[, vars, drop = FALSE]
      keep <- complete.cases(X)
      validate(need(all(X[keep, ] >= 0), "Abundance data cannot contain negative values."))
      keep <- keep & rowSums(X, na.rm = TRUE) > 0
      X <- X[keep, , drop = FALSE]
      validate(need(nrow(X) >= 6, "Need at least 6 non-empty rows."))
      X <- switch(input$n_trans, sqrt = sqrt(X), pa = (X > 0) * 1, X)
      set.seed(123)
      fit <- tryCatch(
        suppressWarnings(vegan::metaMDS(X, distance = input$n_dist, k = 2, trymax = 50,
                                        autotransform = FALSE, trace = 0,
                                        binary = input$n_dist == "jaccard")),
        error = function(e) NULL)
      validate(need(!is.null(fit), "NMDS failed to converge on these data."))
      g <- input$n_group
      grp <- if (!is.null(g) && g != "None" && g %in% names(df)) droplevels(factor(df[[g]][keep])) else NULL
      perm <- disp <- NULL
      if (!is.null(grp) && nlevels(grp) >= 2 && !anyNA(grp)) {
        D <- vegan::vegdist(X, method = input$n_dist, binary = input$n_dist == "jaccard")
        pd <- data.frame(grp = grp)
        names(pd) <- "Group"
        set.seed(123)
        perm <- tryCatch(vegan::adonis2(D ~ Group, data = pd, permutations = 999), error = function(e) NULL)
        disp <- tryCatch(vegan::permutest(vegan::betadisper(D, grp), permutations = 999), error = function(e) NULL)
      }
      list(fit = fit, grp = grp, gname = g, perm = perm, disp = disp, n = nrow(X),
           dropped = sum(!keep), show_species = isTRUE(input$n_species))
    })

    output$n_msg <- renderUI({
      if (!has_vegan()) return(tags$div(class = "alert alert-warning",
        "NMDS and PERMANOVA need the 'vegan' package. Run install.packages('vegan') and restart the app."))
      if (is.null(input$n_run) || input$n_run == 0)
        return(help_note("Select the species columns and click Run NMDS. Tip: load the 'Tree community' example dataset on the Data tab."))
      r <- nmds()
      s <- r$fit$stress
      qual <- if (s < 0.05) "excellent" else if (s < 0.1) "good" else if (s < 0.2) "usable, interpret with some caution" else "poor - the 2-D map may be misleading"
      help_note(tags$b("Stress = ", sprintf("%.3f", s)), sprintf(" (%s). ", qual),
                sprintf("%d plots ordinated", r$n),
                if (r$dropped > 0) sprintf("; %d empty or incomplete rows dropped", r$dropped), ". ",
                "Rules of thumb: < 0.05 excellent, < 0.1 good, < 0.2 usable, > 0.2 poor.")
    })

    output$n_plot <- renderPlot({
      r <- nmds()
      sc <- as.data.frame(vegan::scores(r$fit, display = "sites"))
      names(sc)[1:2] <- c("x", "y")
      plt <- ggplot(sc, aes(x, y))
      if (!is.null(r$grp)) {
        sc$grp <- r$grp
        plt <- ggplot(sc, aes(x, y, colour = grp)) + geom_point(size = 2.8, alpha = 0.8)
        if (min(table(sc$grp)) >= 4) plt <- plt + stat_ellipse(linewidth = 0.7)
        plt <- plt + scale_colour_manual(values = rep(eco_palette, length.out = nlevels(sc$grp)),
                                         name = r$gname)
      } else {
        plt <- plt + geom_point(size = 2.8, alpha = 0.8, colour = eco_green)
      }
      if (r$show_species) {
        sp <- tryCatch(as.data.frame(vegan::scores(r$fit, display = "species")), error = function(e) NULL)
        if (!is.null(sp) && !anyNA(sp)) {
          names(sp)[1:2] <- c("x", "y"); sp$lab <- rownames(sp)
          plt <- plt + geom_text(data = sp, aes(x, y, label = lab), inherit.aes = FALSE,
                                 size = 3.2, colour = "grey25", fontface = "italic")
        }
      }
      plt + labs(x = "NMDS1", y = "NMDS2",
                 caption = "Axes have no units: only the relative distances between plots matter") +
        coord_equal() + eco_theme() + theme(legend.position = "bottom")
    })

    output$n_perm <- renderPrint({
      r <- nmds()
      if (is.null(r$perm)) {
        cat("Choose a grouping variable and re-run to test for differences in composition.")
        return(invisible())
      }
      cat("PERMANOVA: does composition differ among groups?\n\n")
      print(as.data.frame(r$perm), digits = 3)
      if (!is.null(r$disp)) {
        cat("\nHomogeneity of multivariate dispersion (betadisper):\n")
        print(as.data.frame(r$disp$tab), digits = 3)
      }
    })

    output$n_interp <- renderUI({
      r <- nmds()
      req(!is.null(r$perm))
      p  <- r$perm$`Pr(>F)`[1]; r2 <- r$perm$R2[1]
      dp <- if (!is.null(r$disp)) r$disp$tab$`Pr(>F)`[1] else NA
      tagList(
        help_note(tags$b("In words: "),
                  sprintf("Species composition %s among the levels of %s (PERMANOVA R-squared = %.2f, %s): the grouping explains %.0f%% of the variation in composition.",
                          if (p < 0.05) "differs significantly" else "does not differ significantly",
                          r$gname, r2, fmt_peq(p), 100 * r2)),
        if (!is.na(dp) && dp < 0.05) tags$div(
          class = "alert alert-warning py-2 px-3 mb-0", style = "font-size: 0.88rem;",
          sprintf("Group dispersions differ (%s): a significant PERMANOVA may partly reflect differences in within-group spread, not only in average composition. Check the ordination.", fmt_peq(dp)))
      )
    })

    # ---- Cluster -------------------------------------------------------------
    clus <- reactive({
      req(rv$data)
      df <- as.data.frame(rv$data, check.names = FALSE)
      vars <- intersect(input$k_vars, num_cols(df))
      validate(need(length(vars) >= 2, "Select at least 2 numeric variables."))
      keep <- complete.cases(df[, vars, drop = FALSE])
      X <- df[keep, vars, drop = FALSE]
      validate(need(nrow(X) >= 5, "Need at least 5 complete rows."),
               need(nrow(X) <= 400, "Dendrograms become unreadable above 400 rows."))
      D <- switch(
        input$k_dist,
        euclid_std = {
          validate(need(all(vapply(X, sd, 0) > 0), "Remove constant variables."))
          dist(scale(X))
        },
        euclidean = dist(X),
        bray = {
          validate(need(has_vegan(), "Bray-Curtis needs the 'vegan' package."),
                   need(all(X >= 0) && all(rowSums(X) > 0), "Bray-Curtis needs non-negative data and no empty rows."))
          vegan::vegdist(X, method = "bray")
        })
      hc <- hclust(D, method = input$k_link)
      k  <- min(input$k_k, nrow(X) - 1)
      lab <- input$k_label
      labels <- if (!is.null(lab) && lab != "None" && lab %in% names(df)) as.character(df[[lab]][keep]) else
        as.character(which(keep))
      list(hc = hc, k = k, member = cutree(hc, k), labels = labels, keep = keep,
           lab = lab, has_lab = !is.null(lab) && lab != "None" && lab %in% names(df))
    })

    output$k_plot <- renderPlot({
      cl <- clus()
      op <- par(mar = c(1, 4, 2, 1)); on.exit(par(op))
      plot(cl$hc, labels = cl$labels, hang = -1, cex = if (length(cl$labels) > 60) 0.55 else 0.8,
           main = sprintf("Hierarchical clustering (%s linkage), cut into %d clusters", input$k_link, cl$k),
           xlab = "", sub = "", ylab = "Dissimilarity (height)")
      rect.hclust(cl$hc, k = cl$k, border = rep(eco_palette, length.out = cl$k))
    })

    output$k_tab <- renderDT({
      cl <- clus()
      if (cl$has_lab) {
        tab <- as.data.frame.matrix(table(Cluster = paste("Cluster", cl$member), cl$labels))
        tbl <- cbind(Cluster = rownames(tab), tab, Total = rowSums(tab))
        validate(need(ncol(tab) <= 30, "The label variable has too many distinct values to cross-tabulate; showing a dendrogram only."))
      } else {
        tt <- table(cl$member)
        tbl <- data.frame(Cluster = paste("Cluster", names(tt)), N = as.integer(tt))
      }
      simple_dt(tbl)
    })

    observeEvent(input$k_add, {
      cl <- tryCatch(clus(), error = function(e) NULL)
      if (is.null(cl)) {
        showNotification("Select variables for the cluster analysis first.", type = "error")
        return()
      }
      df <- as.data.frame(rv$data, check.names = FALSE)
      df$cluster <- NA_character_
      df$cluster[cl$keep] <- paste0("C", cl$member)
      rv$data <- df
      showNotification("Column 'cluster' added to the data.", type = "message")
    })
  })
}

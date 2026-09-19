### Plot builders for the GOF and correlation tabs ###

# ggpmisc labels are resolved by after_stat() at plot build time, not at parse time
utils::globalVariables(c("eq.label", "rr.label"))

facet_ncol <- function(n_panels) {
  if (n_panels <= 4) 2 else if (n_panels <= 9) 3 else if (n_panels <= 16) 4 else 5
}

# Continuous stratification variables are binned so that faceting stays readable
bin_continuous_facets <- function(plot_data, facet_vars, data, var_type) {
  for (sv in facet_vars) {
    if (!is.null(data[[sv]]) && var_type(sv) == "Continuous") {
      n_bins <- min(5, length(unique(plot_data[[sv]])))
      plot_data[[sv]] <- cut(plot_data[[sv]], breaks = n_bins,
                             include.lowest = TRUE, dig.lab = 3)
    }
  }
  plot_data
}

add_facets <- function(p, plot_data, facet_vars, scale_parameter) {
  facet_formula   <- stats::as.formula(paste("~", paste(facet_vars, collapse = " + ")))
  n_panels        <- nrow(unique(plot_data[facet_vars]))
  n_facet_vars    <- length(facet_vars)
  strip_margin_tb <- 4 + (n_facet_vars - 1) * 6

  p +
    facet_wrap(facet_formula, labeller = label_both, scales = scale_parameter,
               ncol = facet_ncol(n_panels)) +
    theme(panel.spacing.x = grid::unit(1, "lines"),
          panel.spacing.y = grid::unit(1, "lines"),
          strip.text      = element_text(size = 9, margin = margin(b = strip_margin_tb, t = strip_margin_tb)),
          plot.margin     = margin(t = 50 + (n_facet_vars - 1) * 15, b = 5, l = 5, r = 5))
}

add_gof_points <- function(p, color_var, var_type) {
  if (is.null(color_var)) {
    return(p + geom_point(shape = 1, size = 2, stroke = 0.3, color = "blue"))
  }
  if (var_type(color_var) == "Categorical") {
    p + geom_point(aes(color = factor(.data[[color_var]])),
                   shape = 1, size = 2, stroke = 0.3) + labs(color = color_var)
  } else {
    p + geom_point(aes(color = .data[[color_var]]), shape = 1, size = 2, stroke = 0.3)
  }
}

gof_observed_plots <- function(data, mapping) {
  obs_pred <-
    ggplot(data = data, aes(x = .data[[mapping$PRED]], y = .data[[mapping$DV]])) +
    geom_point(shape = 1, color = "blue", size = 2) +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.5) +
    geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, linewidth = 0.7) +
    theme_bw() + ylab("Observed") + xlab("Predicted") +
    ggtitle("Observed vs. Predicted") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_fixed(ratio = 1) +
    xlim(min(data[[mapping$PRED]], data[[mapping$DV]]),
         max(data[[mapping$PRED]], data[[mapping$DV]])) +
    ylim(min(data[[mapping$PRED]], data[[mapping$DV]]),
         max(data[[mapping$PRED]], data[[mapping$DV]]))

  obs_ipred <-
    ggplot(data = data, aes(x = .data[[mapping$IPRED]], y = .data[[mapping$DV]])) +
    geom_point(shape = 1, color = "blue", size = 2) +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.5) +
    geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, linewidth = 0.7) +
    theme_bw() + ylab("Observed") + xlab("Individual Predicted") +
    ggtitle("Observed vs. Individual Predicted") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_fixed(ratio = 1) +
    xlim(min(data[[mapping$IPRED]], data[[mapping$DV]]),
         max(data[[mapping$IPRED]], data[[mapping$DV]])) +
    ylim(min(data[[mapping$IPRED]], data[[mapping$DV]]),
         max(data[[mapping$IPRED]], data[[mapping$DV]]))

  grid.arrange(obs_pred, obs_ipred, ncol = 2)
}

gof_cwres_plots <- function(data, mapping) {
  cwres_pred <-
    ggplot(data = data, aes(x = .data[[mapping$PRED]], y = .data[[mapping$CWRES]])) +
    geom_point(shape = 1, color = "blue", size = 2) +
    geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, linewidth = 0.7) +
    geom_abline(intercept = 0, slope = 0, linewidth = 0.5) +
    theme_bw() + ylab("CWRES") + xlab("Predicted") +
    ggtitle("CWRES vs. Predicted") +
    theme(plot.title = element_text(hjust = 0.5))

  cwres_time <-
    ggplot(data = data, aes(x = .data[[mapping$TIME]], y = .data[[mapping$CWRES]])) +
    geom_point(shape = 1, color = "blue", size = 2) +
    geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, linewidth = 0.7) +
    geom_abline(intercept = 0, slope = 0, linewidth = 0.5) +
    theme_bw() + ylab("CWRES") + xlab("Time") +
    ggtitle("CWRES vs. Time") +
    theme(plot.title = element_text(hjust = 0.5))

  grid.arrange(cwres_pred, cwres_time, ncol = 2)
}

gof_interactive_plot <- function(data, mapping, gof_type,
                                 stratify        = NULL,
                                 color_var       = NULL,
                                 axis_scale      = NULL,
                                 x_axis_scale    = NULL,
                                 scale_parameter = "fixed",
                                 x_start         = NA_real_,
                                 x_end           = NA_real_,
                                 y_start         = NA_real_,
                                 y_end           = NA_real_,
                                 x_axis_title    = "",
                                 y_axis_title    = "",
                                 plot_title      = "",
                                 var_type        = function(column) "Categorical",
                                 notify          = function(message, type, duration = 5) showNotification(message, type = type, duration = duration)) {

  id_var   <- mapping$ID
  time_var <- mapping$TIME

  if (gof_type %in% c("Observed vs. Predicted", "Observed vs. Individual Predicted")) {

    x_var <- switch(gof_type,
                    "Observed vs. Predicted"            = mapping$PRED,
                    "Observed vs. Individual Predicted" = mapping$IPRED)
    y_var <- mapping$DV

    if (!is.null(axis_scale) && axis_scale == "Log") {
      plot_data <- data %>%
        mutate(!!x_var := as.numeric(.data[[x_var]]),
               !!y_var := as.numeric(.data[[y_var]])) %>%
        filter(is.finite(.data[[x_var]]), .data[[x_var]] > 0,
               is.finite(.data[[y_var]]), .data[[y_var]] > 0)
    } else {
      plot_data <- data
    }

    x_start <- ifelse(!is.na(x_start), x_start, min(data[[x_var]], data[[y_var]], na.rm = TRUE))
    x_end   <- ifelse(!is.na(x_end),   x_end,   max(data[[x_var]], data[[y_var]], na.rm = TRUE))
    y_start <- ifelse(!is.na(y_start), y_start, min(data[[x_var]], data[[y_var]], na.rm = TRUE))
    y_end   <- ifelse(!is.na(y_end),   y_end,   max(data[[x_var]], data[[y_var]], na.rm = TRUE))

    facet_vars <- stratify
    if (!is.null(facet_vars) && length(facet_vars) > 0) {
      plot_data <- bin_continuous_facets(plot_data, facet_vars, data, var_type)
    }

    p <- ggplot(data = plot_data,
                aes(x = .data[[x_var]], y = .data[[y_var]],
                    text = paste0("ID: ", .data[[id_var]], "<br>",
                                  "TIME: ", .data[[time_var]], "<br>",
                                  x_var, ": ", .data[[x_var]], "<br>",
                                  y_var, ": ", .data[[y_var]])))

    p <- add_gof_points(p, color_var, var_type)

    p <- p +
      geom_abline(intercept = 0, slope = 1, linewidth = 0.4) +
      geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, linewidth = 0.5,
                  aes(text = "")) +
      theme_bw() +
      xlab(ifelse(x_axis_title != "", x_axis_title, strsplit(gof_type, " vs. ")[[1]][2])) +
      ylab(ifelse(y_axis_title != "", y_axis_title, strsplit(gof_type, " vs. ")[[1]][1])) +
      ggtitle(ifelse(plot_title != "", plot_title, gof_type)) +
      theme(plot.title = element_text(hjust = 0.5))

    # Skip coord when log scale is active
    if (scale_parameter == "fixed" && (is.null(axis_scale) || axis_scale != "Log")) {
      p <- p + coord_fixed(ratio = 1, xlim = c(x_start, x_end), ylim = c(y_start, y_end))
    } else if (scale_parameter != "fixed" && (is.null(axis_scale) || axis_scale != "Log")) {
      p <- p + theme(aspect.ratio = 1)
    }

    if (!is.null(stratify) && length(stratify) > 0) {
      p <- add_facets(p, plot_data, facet_vars, scale_parameter)
    }

    if (!is.null(axis_scale) && axis_scale == "Log") {
      n_total    <- nrow(data)
      n_excluded <- n_total - nrow(plot_data)
      if (n_excluded > 0) {
        notify(sprintf("%d of %d observations (%.1f%%) excluded: zero, negative, or missing values cannot be shown on a log scale.",
                       n_excluded, n_total, 100 * n_excluded / n_total),
               "warning", duration = NULL)
        # Carried on the plot itself so the exclusion stays visible in downloaded figures
        p <- p +
          labs(subtitle = sprintf("%d of %d observations excluded (zero, negative, or missing values on log scale)",
                                  n_excluded, n_total)) +
          theme(plot.subtitle = element_text(hjust = 0.5, size = 12))
      }
      if (nrow(plot_data) == 0) {
        notify("No positive values available for log scale after filtering.", "error")
        return(NULL)
      }
      log_x_start <- ifelse(!is.na(x_start) && x_start > 0, x_start, min(plot_data[[x_var]], na.rm = TRUE))
      log_x_end   <- ifelse(!is.na(x_end)   && x_end   > 0, x_end,   max(plot_data[[x_var]], na.rm = TRUE))
      log_y_start <- ifelse(!is.na(y_start) && y_start > 0, y_start, min(plot_data[[y_var]], na.rm = TRUE))
      log_y_end   <- ifelse(!is.na(y_end)   && y_end   > 0, y_end,   max(plot_data[[y_var]], na.rm = TRUE))
      if (!all(is.finite(c(log_x_start, log_x_end, log_y_start, log_y_end))) ||
          any(c(log_x_start, log_x_end, log_y_start, log_y_end) <= 0)) {
        notify("Axis limits for log scale are not valid.", "error")
        return(NULL)
      }
      p <- p +
        scale_x_log10(labels = scales::label_number(), limits = c(log_x_start, log_x_end)) +
        scale_y_log10(labels = scales::label_number(), limits = c(log_y_start, log_y_end)) +
        annotation_logticks(sides = "lb", linewidth = 0.1) +
        theme(panel.grid.minor = element_blank())
    }

    p

  } else if (gof_type %in% c("CWRES vs. Predicted", "CWRES vs. Time")) {

    x_var <- switch(gof_type,
                    "CWRES vs. Predicted" = mapping$PRED,
                    "CWRES vs. Time"      = mapping$TIME)
    y_var <- mapping$CWRES

    if (!is.null(x_axis_scale) && x_axis_scale == "Log") {
      plot_data <- data %>%
        mutate(!!x_var := as.numeric(.data[[x_var]])) %>%
        filter(is.finite(.data[[x_var]]), .data[[x_var]] > 0)
    } else {
      plot_data <- data
    }

    x_start <- ifelse(!is.na(x_start), x_start, min(data[[x_var]], na.rm = TRUE))
    x_end   <- ifelse(!is.na(x_end),   x_end,   max(data[[x_var]], na.rm = TRUE))
    y_limit <- max(abs(data[[y_var]]), na.rm = TRUE)
    y_start <- ifelse(!is.na(y_start), y_start, -y_limit)
    y_end   <- ifelse(!is.na(y_end),   y_end,    y_limit)

    facet_vars <- stratify
    if (!is.null(facet_vars) && length(facet_vars) > 0) {
      plot_data <- bin_continuous_facets(plot_data, facet_vars, data, var_type)
    }

    p <- ggplot(data = plot_data,
                aes(x = .data[[x_var]], y = .data[[y_var]],
                    text = paste0("ID: ", .data[[id_var]], "<br>",
                                  "TIME: ", .data[[time_var]], "<br>",
                                  x_var, ": ", .data[[x_var]], "<br>",
                                  y_var, ": ", .data[[y_var]])))

    p <- add_gof_points(p, color_var, var_type)

    p <- p +
      geom_abline(intercept = 0, slope = 0, linewidth = 0.4) +
      geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, linewidth = 0.5,
                  aes(text = "")) +
      theme_bw() +
      xlab(ifelse(x_axis_title != "", x_axis_title, strsplit(gof_type, " vs. ")[[1]][2])) +
      ylab(ifelse(y_axis_title != "", y_axis_title, strsplit(gof_type, " vs. ")[[1]][1])) +
      ggtitle(ifelse(plot_title != "", plot_title, gof_type)) +
      theme(plot.title = element_text(hjust = 0.5))

    # Skip coord when log scale is active; use aspect ratio matching x/y range
    if (scale_parameter == "fixed" && (is.null(x_axis_scale) || x_axis_scale != "Log")) {
      x_range <- x_end - x_start
      y_range <- y_end - y_start
      aspect  <- if (y_range > 0) x_range / y_range else 1
      p <- p + coord_fixed(ratio = aspect, xlim = c(x_start, x_end), ylim = c(y_start, y_end))
    } else if (scale_parameter != "fixed" && (is.null(x_axis_scale) || x_axis_scale != "Log")) {
      p <- p + theme(aspect.ratio = (y_end - y_start) / (x_end - x_start))
    }

    if (!is.null(stratify) && length(stratify) > 0) {
      p <- add_facets(p, plot_data, facet_vars, scale_parameter)
    }

    if (!is.null(x_axis_scale) && x_axis_scale == "Log") {
      n_total    <- nrow(data)
      n_excluded <- n_total - nrow(plot_data)
      if (n_excluded > 0) {
        notify(sprintf("%d of %d observations (%.1f%%) excluded: zero, negative, or missing X-axis values cannot be shown on a log scale.",
                       n_excluded, n_total, 100 * n_excluded / n_total),
               "warning", duration = NULL)
        # Carried on the plot itself so the exclusion stays visible in downloaded figures
        p <- p +
          labs(subtitle = sprintf("%d of %d observations excluded (zero, negative, or missing X values on log scale)",
                                  n_excluded, n_total)) +
          theme(plot.subtitle = element_text(hjust = 0.5, size = 12))
      }
      if (nrow(plot_data) == 0) {
        notify("No positive values available for log scale after filtering.", "error")
        return(NULL)
      }
      log_x_start <- ifelse(!is.na(x_start) && x_start > 0, x_start, min(plot_data[[x_var]], na.rm = TRUE))
      log_x_end   <- ifelse(!is.na(x_end)   && x_end   > 0, x_end,   max(plot_data[[x_var]], na.rm = TRUE))
      if (!all(is.finite(c(log_x_start, log_x_end))) || any(c(log_x_start, log_x_end) <= 0)) {
        notify("X-axis limits for log scale are not valid.", "error")
        return(NULL)
      }
      p <- p +
        scale_x_log10(labels = scales::label_number(), limits = c(log_x_start, log_x_end)) +
        annotation_logticks(sides = "b", linewidth = 0.1)
    }

    p
  }
}

correlation_plots <- function(data, x_vars, y_vars, plot_type, regression_type, display_ci) {
  combinations <- expand.grid(x = x_vars, y = y_vars, stringsAsFactors = FALSE)

  # lapply, not a for loop: aes() quosures resolve lazily, so each plot needs its
  # own environment or every panel would render the last variable pair.
  plot_list <- lapply(seq_len(nrow(combinations)), function(i) {
    x_var <- combinations$x[i]
    y_var <- combinations$y[i]

    if (plot_type == "Line plot") {
      p <- ggplot(data = data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(color = "blue") + theme_bw() +
        xlab(x_var) + ylab(y_var) +
        geom_smooth(method = regression_type, se = display_ci, level = 0.95)
      if (regression_type == "lm") {
        p <- p + stat_poly_eq(
          aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
          formula = y ~ x, parse = TRUE
        )
      }
      p
    } else {
      ggplot(data = data, aes(x = factor(.data[[x_var]]), y = .data[[y_var]])) +
        geom_boxplot() + geom_jitter(color = "blue", width = 0.1, size = 1.5, alpha = 0.6) +
        theme_bw() + xlab(x_var) + ylab(y_var)
    }
  })

  grid.arrange(grobs = plot_list, ncol = 2)
}

### Server logic ###

#' PopPK FitCheckR server function
#'
#' @param input,output,session Shiny server arguments.
#'
#' @return Called for its side effects.
#' @export
app_server <- function(input, output, session) {

  #### File upload GOF ####

  table_unfiltered  <- reactiveVal()
  current_file_name <- reactiveVal()

  processTableFile <- function(fileName, data) {
    current_file_name(fileName)
    table_unfiltered(data)

    updateSelectizeInput(session, "select_column",   choices = names(data), selected = "")
    updateSelectizeInput(session, "select_stratify", choices = names(data), selected = "")
    updateSelectizeInput(session, "select_color",    choices = c("", names(data)), selected = "")

    output$var_type_selector <- renderUI({
      vars <- names(data)
      tagList(
        lapply(vars, function(var) {
          fluidRow(
            column(width = 3, offset = 3, p(var)),
            column(width = 5,
                   radioButtons(
                     inputId  = paste0("variable_type_", var),
                     label    = NULL,
                     choices  = c("Continuous", "Categorical"),
                     selected = auto_classify(data[[var]]),
                     inline   = TRUE
                   ))
          )
        })
      )
    })

    output$col_name_selector <- renderUI({
      identifiers  <- c("PRED", "IPRED", "DV", "CWRES", "TIME", "ID")
      data_columns <- names(data)
      threshold    <- 0.2
      tagList(
        lapply(identifiers, function(identifier) {
          distances      <- stringdist(identifier, data_columns, method = "jw")
          exact_matches  <- data_columns[distances == 0]
          close_matches  <- data_columns[distances < threshold]
          default_selection <- if (length(exact_matches) > 0) {
            exact_matches[1]
          } else if (length(close_matches) > 0) {
            data_columns[which.min(distances)]
          } else {
            NULL
          }
          fluidRow(
            column(width = 3, offset = 3, p(identifier)),
            column(width = 5,
                   selectizeInput(
                     inputId  = paste0("column_for_", identifier),
                     label    = NULL,
                     choices  = c("Please select a column" = "", data_columns),
                     selected = default_selection,
                     multiple = FALSE
                   ))
          )
        })
      )
    })
  }

  observeEvent(input$table_file, {
    req(input$table_file)
    tryCatch(
      processTableFile(
        input$table_file$name,
        read_uploaded_table(input$table_file$datapath, input$table_file$name)
      ),
      error = function(msg) {
        message(conditionMessage(msg))
        showNotification(conditionMessage(msg), type = "error", duration = 10)
      })
  })

  #### Filter data ####

  output$filter_inputs <- renderUI({
    req(table_unfiltered(), input$select_column)
    filter_ui_list <- lapply(input$select_column, function(selected_column) {
      column_data   <- table_unfiltered()[[selected_column]]
      variable_type <- get_variable_type(selected_column, column_data, input)
      # Existing values are carried over so adding or removing a column does not reset the other filters
      if (variable_type == "Continuous") {
        value_range <- range(column_data, na.rm = TRUE)
        prev_start  <- suppressWarnings(as.numeric(isolate(input[[paste0("filter_start_", selected_column)]])))
        prev_end    <- suppressWarnings(as.numeric(isolate(input[[paste0("filter_end_",   selected_column)]])))
        start_value <- if (length(prev_start) == 1 && !is.na(prev_start) &&
                           prev_start >= value_range[1] && prev_start <= value_range[2]) prev_start else value_range[1]
        end_value   <- if (length(prev_end) == 1 && !is.na(prev_end) &&
                           prev_end >= value_range[1] && prev_end <= value_range[2]) prev_end else value_range[2]
        fluidRow(
          column(6, textInput(paste0("filter_start_", selected_column),
                              paste("Start value for", selected_column),
                              value = start_value)),
          column(6, textInput(paste0("filter_end_", selected_column),
                              paste("End value for", selected_column),
                              value = end_value))
        )
      } else {
        unique_values <- sort(unique(column_data))
        prev_selected <- isolate(input[[paste0("filter_select_", selected_column)]])
        # Drop carried-over values that are absent from the current file
        kept_values   <- intersect(as.character(prev_selected), as.character(unique_values))
        selectizeInput(paste0("filter_select_", selected_column),
                       paste("Select values for", selected_column),
                       choices  = unique_values,
                       selected = if (length(kept_values) > 0) kept_values else unique_values,
                       multiple = TRUE)
      }
    })
    do.call(tagList, filter_ui_list)
  })

  table <- reactive({
    req(table_unfiltered())
    data <- table_unfiltered()
    if (is.null(input$select_column) || length(input$select_column) == 0) return(data)
    for (selected_column in input$select_column) {
      column_data <- data[[selected_column]]
      if (is.null(input[[paste0("filter_start_", selected_column)]]) &&
          is.null(input[[paste0("filter_end_",   selected_column)]]) &&
          is.null(input[[paste0("filter_select_", selected_column)]])) next
      variable_type <- get_variable_type(selected_column, column_data, input)
      if (variable_type == "Continuous") {
        start_value <- as.numeric(input[[paste0("filter_start_", selected_column)]])
        end_value   <- as.numeric(input[[paste0("filter_end_",   selected_column)]])
        if (!is.na(start_value) & !is.na(end_value)) {
          data <- data %>% filter(data[[selected_column]] >= start_value &
                                    data[[selected_column]] <= end_value)
        }
      } else {
        filter_select <- input[[paste0("filter_select_", selected_column)]]
        # An emptied selection is treated as no filter rather than returning zero rows
        if (length(filter_select) == 0) next
        data <- data %>% filter(data[[selected_column]] %in% filter_select)
      }
    }
    data
  })


  #### GOF plots ####

  column_mapping <- reactive({
    identifiers  <- c("PRED", "IPRED", "DV", "CWRES", "TIME", "ID")
    data_columns <- names(table())
    if (all(identifiers %in% data_columns)) {
      mapping <- lapply(identifiers, function(id) id)
      names(mapping) <- identifiers
    } else {
      mapping <- lapply(identifiers, function(identifier) {
        selected_column <- input[[paste0("column_for_", identifier)]]
        if (!is.null(selected_column) && selected_column != "") selected_column else NULL
      })
      names(mapping) <- identifiers
    }
    validate(need(!any(sapply(mapping, is.null)), "Please map all required columns."))
    mapping
  })

  combined_obs <- reactive({
    mapping <- column_mapping()
    if (any(sapply(mapping[c("PRED", "DV", "IPRED")], is.null))) return(NULL)
    gof_observed_plots(table(), mapping)
  })

  output$obs_plot_output <- renderPlot({ combined_obs() }, res = 96)

  combined_cwres <- reactive({
    mapping <- column_mapping()
    if (any(sapply(mapping[c("PRED", "CWRES", "TIME")], is.null))) return(NULL)
    gof_cwres_plots(table(), mapping)
  })

  output$cwres_plot_output <- renderPlot({ combined_cwres() }, res = 96)

  output$download_gofs <- downloadHandler(
    filename = function() paste0("gof_plots_", get_timestamp(), ".png"),
    content  = function(file) {
      png(file, width = 1400, height = 800, res = 96)
      grid.arrange(combined_obs(), combined_cwres(), nrow = 2)
      dev.off()
    }
  )


  #### Individual plot ####

  output$axis_scale_ui <- renderUI({
    if (input$gof_type %in% c("Observed vs. Predicted", "Observed vs. Individual Predicted")) {
      radioButtons("axis_scale", "X and Y axes scale", choices = c("Linear", "Log"), selected = "Linear")
    } else if (input$gof_type %in% c("CWRES vs. Predicted", "CWRES vs. Time")) {
      radioButtons("x_axis_scale", "X axis scale", choices = c("Linear", "Log"), selected = "Linear")
    }
  })

  output$free_scale_ui <- renderUI({
    if (input$gof_type %in% c("Observed vs. Predicted", "Observed vs. Individual Predicted")) {
      checkboxInput("free_scale_xy", "Free X and Y axes scale (when grouped)", value = FALSE)
    } else if (input$gof_type %in% c("CWRES vs. Predicted", "CWRES vs. Time")) {
      checkboxInput("free_scale_x", "Free X axis scale (when grouped)", value = FALSE)
    }
  })

  gof_plot <- reactive({
    req(input$gof_type, column_mapping(), table())
    data <- table()

    gof_interactive_plot(
      data            = data,
      mapping         = column_mapping(),
      gof_type        = input$gof_type,
      stratify        = input$select_stratify,
      color_var       = if (!is.null(input$select_color) && input$select_color != "") input$select_color else NULL,
      axis_scale      = input$axis_scale,
      x_axis_scale    = input$x_axis_scale,
      scale_parameter = case_when(
        isTruthy(input$free_scale_xy) ~ "free",
        isTruthy(input$free_scale_x)  ~ "free_x",
        TRUE ~ "fixed"
      ),
      x_start      = as.numeric(input$x_start),
      x_end        = as.numeric(input$x_end),
      y_start      = as.numeric(input$y_start),
      y_end        = as.numeric(input$y_end),
      x_axis_title = input$x_axis_title,
      y_axis_title = input$y_axis_title,
      plot_title   = input$plot_title,
      var_type     = variable_type_lookup(data, input),
      notify       = function(message, type, duration = 5) showNotification(message, type = type, duration = duration)
    )
  })

  facet_plot_height <- reactive({
    h <- 500
    if (!is.null(input$select_stratify) && length(input$select_stratify) > 0 && !is.null(table())) {
      plot_data  <- table()
      facet_vars <- input$select_stratify
      panels_per_var <- sapply(facet_vars, function(sv) {
        if (!is.null(plot_data[[sv]])) {
          n_unique <- length(unique(plot_data[[sv]]))
          if (get_variable_type(sv, plot_data[[sv]], input) == "Continuous") min(5, n_unique) else n_unique
        } else 1
      })
      n_panels   <- prod(panels_per_var)
      nrow_facet <- ceiling(n_panels / facet_ncol(n_panels))
      h <- max(500, 300 * nrow_facet)
    }
    h
  })

  output$plot_output_ui <- renderUI({
    plotlyOutput("plot_output", width = "100%", height = paste0(facet_plot_height(), "px"))
  })

  output$plot_output <- renderPlotly({
    req(input$gof_type, gof_plot())
    h <- facet_plot_height()
    n_strat <- if (!is.null(input$select_stratify)) length(input$select_stratify) else 0
    top_margin <- 70 + max(0, n_strat - 1) * 20
    # ggplotly drops the ggplot subtitle, so it is folded into the plotly title
    plot_subtitle <- gof_plot()$labels$subtitle
    title_text <- gof_plot()$labels$title
    if (!is.null(plot_subtitle))
      title_text <- paste0(title_text, "<br><span style='font-size:12px'>", plot_subtitle, "</span>")
    ggplotly(gof_plot(), tooltip = "text", height = h) %>%
      layout(
        title  = list(text = title_text, font = list(size = 14)),
        margin = list(t = top_margin, b = 40)
      )
  })

  output$download_gof <- downloadHandler(
    filename = function() paste0(gsub(" vs. ", "-", input$gof_type), "_", get_timestamp(), ".png"),
    content  = function(file) {
      ggsave(file, plot = gof_plot(), width = 8, height = 8, dpi = 300)
    },
    contentType = "image/png"
  )


  #### Covariate correlation plots ####

  cov_unfiltered        <- reactiveVal()
  current_file_name_cov <- reactiveVal()

  cov_data <- reactive({
    req(cov_unfiltered())
    df <- cov_unfiltered()
    if (isTRUE(input$dedup_by_id)) {
      # This tab has no column mapping, so the ID column is matched by name
      id_col <- names(df)[match("id", tolower(names(df)))]
      if (is.na(id_col)) {
        showNotification("No 'ID' column found in the uploaded file; deduplication was not applied.",
                         type = "warning", duration = 10)
      } else {
        df <- df[!duplicated(df[[id_col]]), , drop = FALSE]
      }
    }
    df
  })

  processCovFile <- function(fileName, data) {
    current_file_name_cov(fileName)
    cov_unfiltered(data)
    updateSelectizeInput(session, "select_cov_x", choices = names(data), selected = "")
    updateSelectizeInput(session, "select_cov_y", choices = names(data), selected = "")
  }

  observeEvent(input$cov_file, {
    req(input$cov_file)
    tryCatch(
      processCovFile(
        input$cov_file$name,
        read_uploaded_table(input$cov_file$datapath, input$cov_file$name)
      ),
      error = function(msg) {
        message(conditionMessage(msg))
        showNotification(conditionMessage(msg), type = "error", duration = 10)
      })
  })

  cov_plot <- reactive({
    req(input$select_cov_x, input$select_cov_y)
    correlation_plots(
      data            = cov_data(),
      x_vars          = input$select_cov_x,
      y_vars          = input$select_cov_y,
      plot_type       = input$plot_type,
      regression_type = input$regression_type,
      display_ci      = input$display_ci
    )
  })

  cov_plot_height <- reactive({
    n_plots    <- length(input$select_cov_x) * length(input$select_cov_y)
    nrow_plots <- ceiling(n_plots / 2)
    max(400, 300 * nrow_plots)
  })

  output$cov_plot_output_ui <- renderUI({
    plotOutput("cov_plot_output", width = "100%", height = paste0(cov_plot_height(), "px"))
  })

  output$cov_plot_output <- renderPlot({ cov_plot() }, res = 96)

  output$download_cov <- downloadHandler(
    filename = function() {
      paste0(paste(input$select_cov_x, collapse = "-"), "_",
             paste(input$select_cov_y, collapse = "-"), "_",
             get_timestamp(), ".png")
    },
    content = function(file) {
      png(file, width = 1400, height = 800, res = 96)
      grid::grid.draw(cov_plot())
      dev.off()
    }
  )

}

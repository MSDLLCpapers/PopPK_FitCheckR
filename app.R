### PopPK FitCheckR R Shiny App ###
### Ruilian (Roxy) Zhang ###
### Last update in Sep 2026 ###

library(shiny)
library(shinydashboard)
library(shinyBS)
library(bslib)
library(tidyverse)
library(dplyr)
library(stringdist)
library(ggplot2)
library(DT)
library(gridExtra)
library(plotly)
library(htmlwidgets)
library(ggpmisc)
library(webshot)

Sys.setenv(TZ = "America/New_York")

# Define UI
ui <- fluidPage(
  theme = bs_theme(
    version = 4,
    bootswatch = "flatly",
    primary = "#234aff",
    secondary = "#71a1ff",
    success = "#009E73"
  ),
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-color: #f0f0f0;
          font-family: Arial, sans-serif;
        }
        .container {
          margin-top: 20px;
          max-width: 95%;
        }
        .page-header {
          background-color: #71a1ff;
          color: #ffffff;
          padding: 3px 0;
          text-align: center;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        .well {
          background-color: white;
          border: 1px solid #ddd;
          border-radius: 5px;
          box-shadow: 0 0 5px rgba(0, 0, 0, 0.1);
          padding: 40px;
        }
        .upload-filter-panel {
          background-color: #ffffff;
          border: 1px solid #c8d8ff;
          border-radius: 5px;
          padding: 16px 20px 8px 20px;
          margin-bottom: 16px;
        }
        .info-icon {
          margin-left: 5px;
          color: #17a2b8;
          cursor: pointer;
        }
        .nav-tabs a {
          color: #234aff;
          font-weight: bold !important;
          background-color: #d6e4ff;
          border-radius: 4px 4px 0 0;
          margin-right: 2px;
        }
        .nav-tabs a:hover {
          background-color: #b8d0ff;
        }
        .nav-tabs .active a,
        .nav-tabs .nav-link.active {
          background-color: #ffffff;
        }
        #main_tabs > .nav-tabs > li > a {
          background-color: #a3c0ff;
          color: #ffffff;
        }
        #main_tabs > .nav-tabs > li > a:hover {
          background-color: #8db0ff;
        }
        #main_tabs > .nav-tabs > li.active > a,
        #main_tabs > .nav-tabs > .nav-item > .nav-link.active {
          background-color: #ffffff;
          color: #234aff;
        }
        .custom-select-input {
          border: 1.5px solid #71a1ff;
          border-radius: 4px;
        }
        .custom-select-input option {
          background-color: #f0f8ff;
          padding: 5px;
        }
        .selectize-input, .selectize-dropdown-content {
          border: 1.5px solid #71a1ff;
          border-radius: 4px;
        }
        .selectize-input.items > .item {
          background-color: #f0f8ff;
          padding: 5px;
        }
        .clear-btn {
          cursor: pointer;
          font-weight: bold;
          margin-left: 5px;
        }
        .modebar-container {
          top: 30px !important;
        }
        "
      )
    )
  ),

  div(
    class = "container",
    div(class = "page-header", h3("PopPK FitCheckR")),
    br(),

    # Top-level tabs
    tabsetPanel(
      id = "main_tabs",

      # ════════════════════════════════════════════════════════════════════════
      # TAB 1 — Instructions
      # ════════════════════════════════════════════════════════════════════════
      tabPanel(
        "Instructions",
        br(),
        div(
          class = "well",
          style = "font-size: 14px; line-height: 1.7;",

          # ── Introduction ──────────────────────────────────────────────────
          HTML('
            <p style="font-size:15px; font-weight:600; color:#234aff; border-left:4px solid #71a1ff;
                       padding-left:10px; margin-bottom:6px; margin-top:4px;">
              Introduction
            </p>
            <p style="margin-top:0;">
              The <strong>PopPK FitCheckR</strong> app automates goodness-of-fit (GOF) diagnostics and
              variable correlation analysis for population pharmacokinetic (popPK) models built in NONMEM.
              The app is organized into two main analysis tabs:
              <strong>Goodness of Fit (GOF) Plots</strong> and <strong>Correlation Plots</strong>.
            </p>
            <hr style="border-top:1px solid #e0e8ff; margin:14px 0;">
          '),

          # ── GOF Plots Tab ─────────────────────────────────────────────────
          HTML('
            <p style="font-size:15px; font-weight:600; color:#234aff; border-left:4px solid #71a1ff;
                       padding-left:10px; margin-bottom:6px;">
              Goodness of Fit (GOF) Plots Tab
            </p>
            <p style="margin-top:0;">
              Upload a NONMEM output file (e.g., sdtab) using the <strong>Browse</strong> button.
              The following sub-tabs are available after upload:
            </p>
            <table style="width:100%; border-collapse:collapse; font-size:13.5px;">
              <colgroup><col style="width:28%"><col style="width:72%"></colgroup>
              <thead>
                <tr style="background-color:#f0f4ff;">
                  <th style="padding:7px 10px; text-align:left; border-bottom:2px solid #c8d8ff;">Sub-tab</th>
                  <th style="padding:7px 10px; text-align:left; border-bottom:2px solid #c8d8ff;">Description</th>
                </tr>
              </thead>
              <tbody>
                <tr style="border-bottom:1px solid #eef0f8;">
                  <td style="padding:7px 10px; font-weight:600;">Data Standardization<br><span style="font-weight:400; font-style:italic; color:#555;">&#8627; Column Name</span></td>
                  <td style="padding:7px 10px;">Map columns in the uploaded file to the required variable names: <code>DV</code>, <code>PRED</code>, <code>IPRED</code>, <code>CWRES</code>, <code>TIME</code>, <code>ID</code>. Auto-matching is attempted based on name similarity.</td>
                </tr>
                <tr style="border-bottom:1px solid #eef0f8; background-color:#fafbff;">
                  <td style="padding:7px 10px; font-style:italic; color:#555;">&#8627; Variable Type <span style="font-size:11px; background:#e8f4e8; color:#2a7a2a; border-radius:3px; padding:1px 5px; font-style:normal;">optional</span></td>
                  <td style="padding:7px 10px;">Change the automatically assigned variable type (Continuous or Categorical) for each column. This affects how filter controls are displayed in the left panel.</td>
                </tr>
                <tr style="border-bottom:1px solid #eef0f8;">
                  <td style="padding:7px 10px; font-weight:600;">Filter Data <span style="font-size:11px; background:#e8f4e8; color:#2a7a2a; border-radius:3px; padding:1px 5px; font-weight:400;">(optional)</span></td>
                  <td style="padding:7px 10px;">Select one or more columns to subset the data prior to or during plotting. Continuous variables are filtered by numeric range; categorical variables are filtered by selecting specific values.</td>
                </tr>
                <tr style="border-bottom:1px solid #eef0f8; background-color:#fafbff;">
                  <td style="padding:7px 10px; font-weight:600;">GOF Plots</td>
                  <td style="padding:7px 10px;">Displays four static diagnostic plots: Observed vs. Predicted, Observed vs. Individual Predicted, Conditional Weighted Residuals (CWRES) vs. Predicted, and CWRES vs. Time. Download as <code>.png</code> file.</td>
                </tr>
                <tr>
                  <td style="padding:7px 10px; font-weight:600;">Interactive GOF Plot</td>
                  <td style="padding:7px 10px;">Select one of the four GOF plot types and explore interactively. Supports faceting by any user-selected column(s), color-coding, linear/log axis scale, axis range customization, and axis/plot title renaming. Download as <code>.png</code> file.</td>
                </tr>
              </tbody>
            </table>
            <hr style="border-top:1px solid #e0e8ff; margin:14px 0;">
          '),

          # ── Correlation Plots Tab ─────────────────────────────────────────
          HTML('
            <p style="font-size:15px; font-weight:600; color:#234aff; border-left:4px solid #71a1ff;
                       padding-left:10px; margin-bottom:6px;">
              Correlation Plots Tab
            </p>
            <p style="margin-top:0;">
              Upload a NONMEM output table file containing the variables of interest
              (e.g., <code>patab</code> for ETAs, <code>cotab</code> for covariates) using the
              <strong>Browse</strong> button. Select the <strong>X axes</strong> and
              <strong>Y axes</strong> variables. All pairwise combinations will be plotted.
            </p>
            <table style="width:100%; border-collapse:collapse; font-size:13.5px;">
              <colgroup><col style="width:28%"><col style="width:72%"></colgroup>
              <thead>
                <tr style="background-color:#f0f4ff;">
                  <th style="padding:7px 10px; text-align:left; border-bottom:2px solid #c8d8ff;">Option</th>
                  <th style="padding:7px 10px; text-align:left; border-bottom:2px solid #c8d8ff;">Description</th>
                </tr>
              </thead>
              <tbody>
                <tr style="border-bottom:1px solid #eef0f8;">
                  <td style="padding:7px 10px; font-weight:600;">Plot type</td>
                  <td style="padding:7px 10px;">Line plot (scatter plot with regression line overlay) or box plot. Download all pairwise plots as <code>.png</code> file.</td>
                </tr>
                <tr style="border-bottom:1px solid #eef0f8; background-color:#fafbff;">
                  <td style="padding:7px 10px; font-weight:600;">Regression method <span style="font-size:11px; font-weight:400; color:#888;">(line plot only)</span></td>
                  <td style="padding:7px 10px;"><code>lm</code> (linear model) or <code>loess</code> (locally estimated smoothing).</td>
                </tr>
                <tr>
                  <td style="padding:7px 10px; font-weight:600;">Display standard error <span style="font-size:11px; font-weight:400; color:#888;">(line plot only)</span></td>
                  <td style="padding:7px 10px;">Option to show standard error band around the regression line.</td>
                </tr>
              </tbody>
            </table>
            <hr style="border-top:1px solid #e0e8ff; margin:14px 0;">
          '),

          # ── Notes ─────────────────────────────────────────────────────────
          HTML('
            <p style="font-size:15px; font-weight:600; color:#234aff; border-left:4px solid #71a1ff;
                       padding-left:10px; margin-bottom:6px;">
              Notes
            </p>
            <ul style="margin-top:0; padding-left:20px; line-height:1.8;">
              <li><strong>File format:</strong> The first line of each uploaded file is <strong>automatically skipped</strong>,
                  consistent with the NONMEM output table format (e.g., <code>TABLE NO. 1</code> header).
                  Any plain-text tabular file is accepted: files with a <code>.csv</code> extension are read as
                  comma-separated; all others (including extensionless NONMEM tables such as <code>sdtab</code>,
                  <code>patab</code>, and <code>cotab</code>) are read as whitespace-delimited. All input table files are expected to be generated with
                  the <code>ONEHEADER</code> option in the NONMEM <code>$TABLE</code> record, so that the file
                  contains only a single header row.</li>
              <li><strong>Variable type auto-detection:</strong> Each column is automatically classified
                  as <em>Continuous</em> or <em>Categorical</em> based on two criteria: the column is numeric,
                  and the number of unique values exceeds 10% of the total number of rows (with a minimum of
                  10 unique values). Columns that do not meet both criteria are classified as <em>Categorical</em>
                  by default. This determines whether filter controls appear as a numeric range or a value selector.
                  Override any assignment in <strong>Data Standardization &gt; Variable Type</strong> if needed.</li>
              <li><strong>Column name auto-matching:</strong> The Column Name mapping uses Jaro-Winkler
                  string similarity to suggest matches (e.g., <code>ipred</code> may match to
                  <code>IPRED</code>). Always verify all mappings before proceeding to GOF Plots.</li>
              <li><strong>Pairwise correlation plots:</strong> All combinations of the selected X and Y
                  axes are plotted simultaneously. Selecting many variables may produce a large grid
                  of plots and slow rendering.</li>
            </ul>
          ')
        )
      ), # Instructions

      # ════════════════════════════════════════════════════════════════════════
      # TAB 2 — GOF Plots
      # ════════════════════════════════════════════════════════════════════════
      tabPanel(
        "Goodness of Fit (GOF) Plots",
        br(),

        fluidRow(
          # Left column — Input File + Filter Data
          column(
            width = 2,
            div(
              class = "upload-filter-panel",
              style = "min-height: 640px;",
              HTML('<h6 style="font-weight: bold; margin-bottom: 4px;">Input File</h6>'),
              fileInput(
                "table_file",
                label       = NULL,
                buttonLabel = "Browse",
                accept      = ""
              ),
              tags$hr(),
              HTML('<h6 style="font-weight: bold;">Filter Data</h6>'),
              selectizeInput(
                "select_column",
                label    = NULL,
                choices  = NULL,
                selected = NULL,
                multiple = TRUE,
                options  = list(
                  placeholder      = "",
                  allowEmptyOption = TRUE,
                  plugins          = list("remove_button")
                )
              ),
              uiOutput("filter_inputs")
            )
          ),

          # Right column — GOF workflow sub-tabs
          column(
            width = 10,
            tabsetPanel(
              id = "gof_tabs",

          navbarMenu(
            "Data Standardization",
            tabPanel(
              "Column Name",
              div(
                class = "well",
                style = "min-height: 600px;",
                HTML('<p><i>Please make sure columns below are correctly mapped before proceeding to GOF Plots</i></p>'),
                br(),
                fluidRow(
                  column(width = 3, offset = 3,
                         HTML('<p style="font-weight: bold;">Standardized name</p>')),
                  column(width = 5,
                         HTML('<p style="font-weight: bold;">Column name in data</p>'))
                ),
                uiOutput("col_name_selector")
              )
            ),
            tabPanel(
              "Variable Type",
              div(
                class = "well",
                style = "min-height: 600px;",
                HTML('<p><i>This tab is optional</i></p>'),
                br(),
                fluidRow(
                  column(width = 3, offset = 3,
                         HTML('<p style="font-weight: bold;">Column name</p>')),
                  column(width = 5,
                         HTML('<p style="font-weight: bold;">Variable type</p>'))
                ),
                uiOutput("var_type_selector")
              )
            )
          ), # Data Standardization navbarMenu

          tabPanel(
            "GOF Plots",
            div(
              class = "well",
              style = "min-height: 600px;",
              plotOutput("obs_plot_output"),
              plotOutput("cwres_plot_output"),
              tags$hr(),
              downloadButton("download_gofs", "Download Plots", class = "btn btn-primary")
            )
          ),
          tabPanel(
            "Interactive GOF Plot",
            div(
              class = "well",
              style = "min-height: 600px;",
              fluidRow(
                column(3, selectInput("gof_type", "GOF type",
                                      choices = c("Observed vs. Predicted",
                                                  "Observed vs. Individual Predicted",
                                                  "CWRES vs. Predicted",
                                                  "CWRES vs. Time")))
              ),
              tags$hr(),
              HTML('<h6 style="font-weight: bold;">Grouping and color</h6>'),
              fluidRow(
                column(3,
                       selectizeInput("select_stratify", "Stratification",
                                      choices  = NULL, selected = NULL, multiple = TRUE,
                                      options  = list(placeholder = "Optional",
                                                      allowEmptyOption = TRUE,
                                                      plugins = list("remove_button")))),
                column(3,
                       selectizeInput("select_color", "Color Stratification",
                                      choices = NULL, selected = "",
                                      options = list(placeholder = "Optional")))
              ),
              fluidRow(
                column(8, offset = 2,
                       uiOutput("plot_output_ui"))
              ),
              tags$hr(),
              HTML('<h6 style="font-weight: bold;">Plot customization</h6>'),
              fluidRow(
                column(4, uiOutput("axis_scale_ui")),
                column(5, uiOutput("free_scale_ui"))
              ),
              fluidRow(
                column(3, conditionalPanel(
                  condition = "input.free_scale_xy == false || input.free_scale_x == false",
                  numericInput("x_start", "X axis start value", value = "")
                )),
                column(3, conditionalPanel(
                  condition = "input.free_scale_xy == false || input.free_scale_x == false",
                  numericInput("x_end", "X axis end value", value = "")
                )),
                column(3, conditionalPanel(
                  condition = "input.free_scale_xy == false",
                  numericInput("y_start", "Y axis start value", value = "")
                )),
                column(3, conditionalPanel(
                  condition = "input.free_scale_xy == false",
                  numericInput("y_end", "Y axis end value", value = "")
                ))
              ),
              fluidRow(
                column(6, textInput("x_axis_title", "Change X-axis title", value = "")),
                column(6, textInput("y_axis_title", "Change Y-axis title", value = ""))
              ),
              fluidRow(
                column(6, textInput("plot_title", "Change plot title", value = ""))
              ),
              downloadButton("download_gof", "Download Plot", class = "btn btn-primary")
            )
          )

            ) # gof_tabs tabsetPanel
          ) # right column
        ) # fluidRow
      ), # GOF Plots tabPanel

      # ════════════════════════════════════════════════════════════════════════
      # TAB 3 — Correlation Plots
      # ════════════════════════════════════════════════════════════════════════
      tabPanel(
        "Correlation Plots",
        br(),

        fluidRow(
          style = "display: flex; align-items: stretch;",
          # Left column — Input File + Select Axes
          column(
            width = 2,
            div(
              class = "upload-filter-panel",
              style = "min-height: 640px; height: 100%;",
              HTML('<h6 style="font-weight: bold; margin-bottom: 4px;">Input File</h6>'),
              fileInput("cov_file", label = NULL,
                        buttonLabel = "Browse",
                        accept = ""),
              checkboxInput("dedup_by_id", "Deduplicate by ID", value = FALSE),
              tags$div(
                style = "font-size: 12px; color: #888; margin-top: -20px; margin-bottom: 8px;",
                "Assumes covariates are time-invariant."
              ),
              tags$hr(),
              HTML('<h6 style="font-weight: bold;">Select Axes</h6>'),
              selectizeInput("select_cov_x", "X axes",
                             choices = NULL, selected = NULL, multiple = TRUE,
                             options = list(placeholder = "",
                                            allowEmptyOption = TRUE,
                                            plugins = list("remove_button"))),
              selectizeInput("select_cov_y", "Y axes",
                             choices = NULL, selected = NULL, multiple = TRUE,
                             options = list(placeholder = "",
                                            allowEmptyOption = TRUE,
                                            plugins = list("remove_button")))
            )
          ),

          # Right column — Plot Options + Plot + Download
          column(
            width = 10,
            div(
              class = "well",
              style = "min-height: 640px; height: 100%;",
              HTML('<h6 style="font-weight: bold;">Plot Options</h6>'),
              fluidRow(
                column(3,
                       selectizeInput("plot_type", "Plot type",
                                      choices  = c("Line plot", "Box plot"),
                                      selected = "Line plot")),
                column(3,
                       conditionalPanel(
                         condition = "input.plot_type == 'Line plot'",
                         selectizeInput("regression_type", "Regression method",
                                        choices  = c("lm", "loess"),
                                        selected = "loess")
                       )),
                column(3,
                       br(), br(),
                       conditionalPanel(
                         condition = "input.plot_type == 'Line plot'",
                         checkboxInput("display_ci", "Display 95% CI", value = TRUE)
                       ))
              ),
              uiOutput("cov_plot_output_ui"),
              tags$hr(),
              downloadButton("download_cov", "Download Plots", class = "btn btn-primary")
            )
          )
        ) # fluidRow
      ) # Correlation Plots tabPanel

    ) # main_tabs tabsetPanel
  ), # container div
  br()
)


# Server

server <- function(input, output, session) {

  #### Predefined functions ####

  get_timestamp <- function() {
    format(Sys.time(), "%Y%m%d%H%M")
  }

  auto_classify <- function(column_data) {
    n_unique <- length(unique(column_data))
    n_total  <- length(column_data)
    # Continuous only if column is numeric AND unique values exceed 10% of rows (floor of 10)
    if (is.numeric(column_data) && n_unique > max(10, ceiling(n_total * 0.10))) {
      "Continuous"
    } else {
      "Categorical"
    }
  }

  get_variable_type <- function(column_name, column_data, input) {
    input_id <- paste0("variable_type_", column_name)
    variable_type <- input[[input_id]]
    if (is.null(variable_type)) {
      variable_type <- auto_classify(column_data)
    }
    return(variable_type)
  }


  #### File upload GOF ####

  table_unfiltered <- reactiveVal()
  current_file_name <- reactiveVal()

  processTableFile <- function(fileName, data) {
    current_file_name(fileName)
    ext <- tools::file_ext(fileName)

    if (file.exists(data)) {
      if (tolower(ext) == "csv") {
        data <- read.table(data, header = TRUE, sep = ",", skip = 1)
      } else {
        data <- read.table(data, header = TRUE, skip = 1)
      }
    } else {
      if (tolower(ext) == "csv") {
        data <- read.table(text = data, header = TRUE, sep = ",", skip = 1)
      } else {
        data <- read.table(text = data, header = TRUE, skip = 1)
      }
    }

    table_unfiltered(data)

    updateSelectizeInput(session, "select_column",  choices = names(data), selected = "")
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
            close_matches[1]
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
    processTableFile(input$table_file$name, input$table_file$datapath)
  })

  #### Filter data ####

  output$filter_inputs <- renderUI({
    req(table_unfiltered(), input$select_column)
    filter_ui_list <- lapply(input$select_column, function(selected_column) {
      column_data   <- table_unfiltered()[[selected_column]]
      variable_type <- get_variable_type(selected_column, column_data, input)
      if (variable_type == "Continuous") {
        fluidRow(
          column(6, textInput(paste0("filter_start_", selected_column),
                              paste("Start value for", selected_column),
                              value = min(column_data, na.rm = TRUE))),
          column(6, textInput(paste0("filter_end_", selected_column),
                              paste("End value for", selected_column),
                              value = max(column_data, na.rm = TRUE)))
        )
      } else {
        selectizeInput(paste0("filter_select_", selected_column),
                       paste("Select values for", selected_column),
                       choices  = sort(unique(column_data)),
                       selected = unique(column_data),
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
        data <- data %>% filter(data[[selected_column]] %in% filter_select)
      }
    }
    return(data)
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
    obs_pred <-
      ggplot(data = table(), aes_string(x = mapping$PRED, y = mapping$DV)) +
      geom_point(shape = 1, color = "blue", size = 2) +
      geom_abline(intercept = 0, slope = 1, size = 0.5) +
      geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, size = 0.7) +
      theme_bw() + ylab("Observed") + xlab("Predicted") +
      ggtitle("Observed vs. Predicted") +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_fixed(ratio = 1) +
      xlim(min(table()[[mapping$PRED]], table()[[mapping$DV]]),
           max(table()[[mapping$PRED]], table()[[mapping$DV]])) +
      ylim(min(table()[[mapping$PRED]], table()[[mapping$DV]]),
           max(table()[[mapping$PRED]], table()[[mapping$DV]]))
    obs_ipred <-
      ggplot(data = table(), aes_string(x = mapping$IPRED, y = mapping$DV)) +
      geom_point(shape = 1, color = "blue", size = 2) +
      geom_abline(intercept = 0, slope = 1, size = 0.5) +
      geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, size = 0.7) +
      theme_bw() + ylab("Observed") + xlab("Individual Predicted") +
      ggtitle("Observed vs. Individual Predicted") +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_fixed(ratio = 1) +
      xlim(min(table()[[mapping$IPRED]], table()[[mapping$DV]]),
           max(table()[[mapping$IPRED]], table()[[mapping$DV]])) +
      ylim(min(table()[[mapping$IPRED]], table()[[mapping$DV]]),
           max(table()[[mapping$IPRED]], table()[[mapping$DV]]))
    grid.arrange(obs_pred, obs_ipred, ncol = 2)
  })

  output$obs_plot_output <- renderPlot({ combined_obs() }, res = 96)

  combined_cwres <- reactive({
    mapping <- column_mapping()
    if (any(sapply(mapping[c("PRED", "CWRES", "TIME")], is.null))) return(NULL)
    cwres_pred <-
      ggplot(data = table(), aes_string(x = mapping$PRED, y = mapping$CWRES)) +
      geom_point(shape = 1, color = "blue", size = 2) +
      geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, size = 0.7) +
      geom_abline(intercept = 0, slope = 0, size = 0.5) +
      theme_bw() + ylab("CWRES") + xlab("Predicted") +
      ggtitle("CWRES vs. Predicted") +
      theme(plot.title = element_text(hjust = 0.5))
    cwres_time <-
      ggplot(data = table(), aes_string(x = mapping$TIME, y = mapping$CWRES)) +
      geom_point(shape = 1, color = "blue", size = 2) +
      geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, size = 0.7) +
      geom_abline(intercept = 0, slope = 0, size = 0.5) +
      theme_bw() + ylab("CWRES") + xlab("Time") +
      ggtitle("CWRES vs. Time") +
      theme(plot.title = element_text(hjust = 0.5))
    grid.arrange(cwres_pred, cwres_time, ncol = 2)
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
    mapping <- column_mapping()

    scale_parameter <- case_when(
      isTruthy(input$free_scale_xy) ~ "free",
      isTruthy(input$free_scale_x)  ~ "free_x",
      TRUE ~ "fixed"
    )

    x_start   <- as.numeric(input$x_start)
    x_end     <- as.numeric(input$x_end)
    y_start   <- as.numeric(input$y_start)
    y_end     <- as.numeric(input$y_end)
    color_var <- if (!is.null(input$select_color) && input$select_color != "") input$select_color else NULL
    id_var    <- mapping$ID
    time_var  <- mapping$TIME

    if (input$gof_type %in% c("Observed vs. Predicted", "Observed vs. Individual Predicted")) {

      x_var <- switch(input$gof_type,
                      "Observed vs. Predicted"            = mapping$PRED,
                      "Observed vs. Individual Predicted" = mapping$IPRED)
      y_var <- mapping$DV

      if (!is.null(input$axis_scale) && input$axis_scale == "Log") {
        plot_data <- table() %>%
          mutate(!!x_var := as.numeric(.data[[x_var]]),
                 !!y_var := as.numeric(.data[[y_var]])) %>%
          filter(is.finite(.data[[x_var]]), .data[[x_var]] > 0,
                 is.finite(.data[[y_var]]), .data[[y_var]] > 0)
      } else {
        plot_data <- table()
      }

      x_start <- ifelse(!is.na(x_start), x_start, min(table()[[x_var]], table()[[y_var]]))
      x_end   <- ifelse(!is.na(x_end),   x_end,   max(table()[[x_var]], table()[[y_var]]))
      y_start <- ifelse(!is.na(y_start), y_start, min(table()[[x_var]], table()[[y_var]]))
      y_end   <- ifelse(!is.na(y_end),   y_end,   max(table()[[x_var]], table()[[y_var]]))

      # Bin continuous stratification variables for faceting
      facet_vars <- input$select_stratify
      if (!is.null(facet_vars) && length(facet_vars) > 0) {
        for (sv in facet_vars) {
          if (!is.null(table()[[sv]]) &&
              get_variable_type(sv, table()[[sv]], input) == "Continuous") {
            bin_col <- sv
            n_bins  <- min(5, length(unique(plot_data[[sv]])))
            plot_data[[bin_col]] <- cut(plot_data[[sv]], breaks = n_bins,
                                        include.lowest = TRUE, dig.lab = 3)
            facet_vars[facet_vars == sv] <- bin_col
          }
        }
      }

      p <- ggplot(data = plot_data,
                  aes(x = .data[[x_var]], y = .data[[y_var]],
                      text = paste0("ID: ", .data[[id_var]], "<br>",
                                    "TIME: ", .data[[time_var]], "<br>",
                                    x_var, ": ", .data[[x_var]], "<br>",
                                    y_var, ": ", .data[[y_var]])))

      if (!is.null(color_var)) {
        color_type <- get_variable_type(color_var, table()[[color_var]], input)
        if (color_type == "Categorical") {
          p <- p + geom_point(aes_string(color = paste0("factor(", color_var, ")")),
                              shape = 1, size = 2, stroke = 0.3) + labs(color = color_var)
        } else {
          p <- p + geom_point(aes_string(color = color_var), shape = 1, size = 2, stroke = 0.3)
        }
      } else {
        p <- p + geom_point(shape = 1, size = 2, stroke = 0.3, color = "blue")
      }

      p <- p +
        geom_abline(intercept = 0, slope = 1, size = 0.4) +
        geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, size = 0.5,
                    aes(text = "")) +
        theme_bw() +
        xlab(ifelse(input$x_axis_title != "", input$x_axis_title,
                    strsplit(input$gof_type, " vs. ")[[1]][2])) +
        ylab(ifelse(input$y_axis_title != "", input$y_axis_title,
                    strsplit(input$gof_type, " vs. ")[[1]][1])) +
        ggtitle(ifelse(input$plot_title != "", input$plot_title, input$gof_type)) +
        theme(plot.title = element_text(hjust = 0.5))

      # Skip coord when log scale is active
      if (scale_parameter == "fixed" && (is.null(input$axis_scale) || input$axis_scale != "Log")) {
        p <- p + coord_fixed(ratio = 1, xlim = c(x_start, x_end), ylim = c(y_start, y_end))
      } else if (scale_parameter != "fixed" && (is.null(input$axis_scale) || input$axis_scale != "Log")) {
        p <- p + theme(aspect.ratio = 1)
      }

      if (!is.null(input$select_stratify) && length(input$select_stratify) > 0) {
        facet_formula <- as.formula(paste("~", paste(facet_vars, collapse = " + ")))
        n_panels <- nrow(unique(plot_data[facet_vars]))
        ncol_facet <- if (n_panels <= 4) 2 else if (n_panels <= 9) 3 else if (n_panels <= 16) 4 else 5
        n_facet_vars <- length(facet_vars)
        strip_margin_tb <- 4 + (n_facet_vars - 1) * 6
        p <- p +
          facet_wrap(facet_formula, labeller = label_both, scales = scale_parameter, ncol = ncol_facet) +
          theme(panel.spacing.x = unit(1, "lines"),
                panel.spacing.y = unit(1, "lines"),
                strip.text      = element_text(size = 9, margin = margin(b = strip_margin_tb, t = strip_margin_tb)),
                plot.margin     = margin(t = 50 + (n_facet_vars - 1) * 15, b = 5, l = 5, r = 5))
      }

      if (!is.null(input$axis_scale) && input$axis_scale == "Log") {
        if (any(table()[[x_var]] <= 0 | table()[[y_var]] <= 0))
          showNotification("Zero or negative values detected. These points will be excluded from the log-scale plot.", type = "warning")
        if (nrow(plot_data) == 0) {
          showNotification("No positive values available for log scale after filtering.", type = "error")
          return(NULL)
        }
        log_x_start <- ifelse(!is.na(x_start) && x_start > 0, x_start, min(plot_data[[x_var]], na.rm = TRUE))
        log_x_end   <- ifelse(!is.na(x_end)   && x_end   > 0, x_end,   max(plot_data[[x_var]], na.rm = TRUE))
        log_y_start <- ifelse(!is.na(y_start) && y_start > 0, y_start, min(plot_data[[y_var]], na.rm = TRUE))
        log_y_end   <- ifelse(!is.na(y_end)   && y_end   > 0, y_end,   max(plot_data[[y_var]], na.rm = TRUE))
        if (!all(is.finite(c(log_x_start, log_x_end, log_y_start, log_y_end))) ||
            any(c(log_x_start, log_x_end, log_y_start, log_y_end) <= 0)) {
          showNotification("Axis limits for log scale are not valid.", type = "error")
          return(NULL)
        }
        p <- p +
          scale_x_log10(labels = scales::label_number(), limits = c(log_x_start, log_x_end)) +
          scale_y_log10(labels = scales::label_number(), limits = c(log_y_start, log_y_end)) +
          annotation_logticks(sides = "lb", size = 0.1) +
          theme(panel.grid.minor = element_blank())
      }

      p

    } else if (input$gof_type %in% c("CWRES vs. Predicted", "CWRES vs. Time")) {

      x_var <- switch(input$gof_type,
                      "CWRES vs. Predicted" = mapping$PRED,
                      "CWRES vs. Time"      = mapping$TIME)
      y_var <- mapping$CWRES

      if (!is.null(input$x_axis_scale) && input$x_axis_scale == "Log") {
        plot_data <- table() %>%
          mutate(!!x_var := as.numeric(.data[[x_var]])) %>%
          filter(is.finite(.data[[x_var]]), .data[[x_var]] > 0)
      } else {
        plot_data <- table()
      }

      x_start <- ifelse(!is.na(x_start), x_start, min(table()[[x_var]]))
      x_end   <- ifelse(!is.na(x_end),   x_end,   max(table()[[x_var]]))
      y_limit <- max(abs(table()[[y_var]]))
      y_start <- ifelse(!is.na(y_start), y_start, -y_limit)
      y_end   <- ifelse(!is.na(y_end),   y_end,    y_limit)

      # Bin continuous stratification variables for faceting
      facet_vars <- input$select_stratify
      if (!is.null(facet_vars) && length(facet_vars) > 0) {
        for (sv in facet_vars) {
          if (!is.null(table()[[sv]]) &&
              get_variable_type(sv, table()[[sv]], input) == "Continuous") {
            bin_col <- sv
            n_bins  <- min(5, length(unique(plot_data[[sv]])))
            plot_data[[bin_col]] <- cut(plot_data[[sv]], breaks = n_bins,
                                        include.lowest = TRUE, dig.lab = 3)
            facet_vars[facet_vars == sv] <- bin_col
          }
        }
      }

      p <- ggplot(data = plot_data,
                  aes(x = .data[[x_var]], y = .data[[y_var]],
                      text = paste0("ID: ", .data[[id_var]], "<br>",
                                    "TIME: ", .data[[time_var]], "<br>",
                                    x_var, ": ", .data[[x_var]], "<br>",
                                    y_var, ": ", .data[[y_var]])))

      if (!is.null(color_var)) {
        color_type <- get_variable_type(color_var, table()[[color_var]], input)
        if (color_type == "Categorical") {
          p <- p + geom_point(aes_string(color = paste0("factor(", color_var, ")")),
                              shape = 1, size = 2, stroke = 0.3) + labs(color = color_var)
        } else {
          p <- p + geom_point(aes_string(color = color_var), shape = 1, size = 2, stroke = 0.3)
        }
      } else {
        p <- p + geom_point(shape = 1, size = 2, stroke = 0.3, color = "blue")
      }

      p <- p +
        geom_abline(intercept = 0, slope = 0, size = 0.4) +
        geom_smooth(method = "loess", span = 0.75, color = "red", se = FALSE, size = 0.5,
                    aes(text = "")) +
        theme_bw() +
        xlab(ifelse(input$x_axis_title != "", input$x_axis_title,
                    strsplit(input$gof_type, " vs. ")[[1]][2])) +
        ylab(ifelse(input$y_axis_title != "", input$y_axis_title,
                    strsplit(input$gof_type, " vs. ")[[1]][1])) +
        ggtitle(ifelse(input$plot_title != "", input$plot_title, input$gof_type)) +
        theme(plot.title = element_text(hjust = 0.5))

      # Skip coord when log scale is active; use aspect ratio matching x/y range
      if (scale_parameter == "fixed" && (is.null(input$x_axis_scale) || input$x_axis_scale != "Log")) {
        x_range <- x_end - x_start
        y_range <- y_end - y_start
        aspect  <- if (y_range > 0) x_range / y_range else 1
        p <- p + coord_fixed(ratio = aspect / 1, xlim = c(x_start, x_end), ylim = c(y_start, y_end))
      } else if (scale_parameter != "fixed" && (is.null(input$x_axis_scale) || input$x_axis_scale != "Log")) {
        p <- p + theme(aspect.ratio = (y_end - y_start) / (x_end - x_start))
      }

      if (!is.null(input$select_stratify) && length(input$select_stratify) > 0) {
        facet_formula <- as.formula(paste("~", paste(facet_vars, collapse = " + ")))
        n_panels <- nrow(unique(plot_data[facet_vars]))
        ncol_facet <- if (n_panels <= 4) 2 else if (n_panels <= 9) 3 else if (n_panels <= 16) 4 else 5
        n_facet_vars <- length(facet_vars)
        strip_margin_tb <- 4 + (n_facet_vars - 1) * 6
        p <- p +
          facet_wrap(facet_formula, labeller = label_both, scales = scale_parameter, ncol = ncol_facet) +
          theme(panel.spacing.x = unit(1, "lines"),
                panel.spacing.y = unit(1, "lines"),
                strip.text      = element_text(size = 9, margin = margin(b = strip_margin_tb, t = strip_margin_tb)),
                plot.margin     = margin(t = 50 + (n_facet_vars - 1) * 15, b = 5, l = 5, r = 5))
      }

      if (!is.null(input$x_axis_scale) && input$x_axis_scale == "Log") {
        if (any(table()[[x_var]] <= 0))
          showNotification("Zero or negative values detected in X-axis variable. These points will be excluded.", type = "warning")
        if (nrow(plot_data) == 0) {
          showNotification("No positive values available for log scale after filtering.", type = "error")
          return(NULL)

        }
        log_x_start <- ifelse(!is.na(x_start) && x_start > 0, x_start, min(plot_data[[x_var]], na.rm = TRUE))
        log_x_end   <- ifelse(!is.na(x_end)   && x_end   > 0, x_end,   max(plot_data[[x_var]], na.rm = TRUE))
        if (!all(is.finite(c(log_x_start, log_x_end))) || any(c(log_x_start, log_x_end) <= 0)) {
          showNotification("X-axis limits for log scale are not valid.", type = "error")
          return(NULL)
        }
        p <- p +
          scale_x_log10(labels = scales::label_number(), limits = c(log_x_start, log_x_end)) +
          annotation_logticks(sides = "b", size = 0.1)
      }

      p

    } # CWRES plots
  })

  facet_plot_height <- reactive({
    h <- 500
    if (!is.null(input$select_stratify) && length(input$select_stratify) > 0 && !is.null(table())) {
      plot_data <- table()
      facet_vars <- input$select_stratify
      panels_per_var <- sapply(facet_vars, function(sv) {
        if (!is.null(plot_data[[sv]])) {
          n_unique <- length(unique(plot_data[[sv]]))
          if (get_variable_type(sv, plot_data[[sv]], input) == "Continuous") min(5, n_unique) else n_unique
        } else 1
      })
      n_panels <- prod(panels_per_var)
      ncol_facet <- if (n_panels <= 4) 2 else if (n_panels <= 9) 3 else if (n_panels <= 16) 4 else 5
      nrow_facet <- ceiling(n_panels / ncol_facet)
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
    ggplotly(gof_plot(), tooltip = "text", height = h) %>%
      layout(
        title  = list(text = gof_plot()$labels$title, font = list(size = 14)),
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

  cov_unfiltered      <- reactiveVal()
  current_file_name_cov <- reactiveVal()

  cov_data <- reactive({
    req(cov_unfiltered())
    df <- cov_unfiltered()
    if (isTRUE(input$dedup_by_id) && "ID" %in% names(df)) {
      df <- dplyr::distinct(df, ID, .keep_all = TRUE)
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
    file_path <- input$cov_file$datapath
    file_name <- input$cov_file$name
    ext       <- tools::file_ext(file_name)
    if (tolower(ext) == "csv") {
      data <- read.table(file_path, header = TRUE, sep = ",", skip = 1)
    } else {
      data <- read.table(file_path, header = TRUE, skip = 1)
    }
    processCovFile(file_name, data)
  })

  cov_plot <- reactive({
    req(input$select_cov_x, input$select_cov_y)
    combinations <- expand.grid(x = input$select_cov_x, y = input$select_cov_y,
                                stringsAsFactors = FALSE)
    plot_list <- list()
    for (i in seq_len(nrow(combinations))) {
      if (input$plot_type == "Line plot") {
        p <- ggplot(data = cov_data(),
                    aes_string(x = combinations$x[i], y = combinations$y[i])) +
          geom_point(color = "blue") + theme_bw() +
          xlab(combinations$x[i]) + ylab(combinations$y[i]) +
          geom_smooth(method = input$regression_type, se = input$display_ci)
        if (input$regression_type == "lm") {
          p <- p + stat_poly_eq(
            aes_string(label = "paste(..eq.label.., ..rr.label.., sep = '~~~')"),
            formula = y ~ x, parse = TRUE
          )
        }
      } else {
        p <- ggplot(data = cov_data(),
                    aes_string(x = paste0("factor(", combinations$x[i], ")"),
                               y = combinations$y[i])) +
          geom_boxplot() + geom_jitter(color = "blue", width = 0.1, size = 1.5, alpha = 0.6) + theme_bw() +
          xlab(combinations$x[i]) + ylab(combinations$y[i])
      }
      plot_list[[i]] <- p
    }
    grid.arrange(grobs = plot_list, ncol = 2)
  })

  cov_plot_height <- reactive({
    n_x <- length(input$select_cov_x)
    n_y <- length(input$select_cov_y)
    n_plots <- n_x * n_y
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

shinyApp(ui, server)

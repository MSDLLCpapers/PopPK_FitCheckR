### User interface ###

app_css <- function() {
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
}

ui_instructions <- function() {
  tabPanel(
    "Instructions",
    br(),
    div(
      class = "well",
      style = "font-size: 14px; line-height: 1.7;",

      # -- Introduction --------------------------------------------------
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

      # -- GOF Plots Tab -------------------------------------------------
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

      # -- Correlation Plots Tab -----------------------------------------
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

      # -- Notes ---------------------------------------------------------
      HTML('
            <p style="font-size:15px; font-weight:600; color:#234aff; border-left:4px solid #71a1ff;
                       padding-left:10px; margin-bottom:6px;">
              Notes
            </p>
            <ul style="margin-top:0; padding-left:20px; line-height:1.8;">
              <li><strong>File format:</strong> Plain-text tabular files are supported. NONMEM output tables
                  (<code>sdtab</code>, <code>patab</code>, <code>cotab</code>, etc.) may be uploaded directly,
                  as the <code>TABLE NO.</code> title line is detected and excluded automatically.
                  Comma-separated files exported from other tools are also supported, provided the file carries a
                  <code>.csv</code> extension and the first row contains the column names. Files without a
                  <code>.csv</code> extension are read as space- or tab-delimited. NONMEM tables should be generated
                  with the <code>ONEHEADER</code> option in the <code>$TABLE</code> record so that the file contains
                  a single header row.</li>
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
  )
}

ui_gof <- function() {
  tabPanel(
    "Goodness of Fit (GOF) Plots",
    br(),

    fluidRow(
      # Left column - Input File + Filter Data
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

      # Right column - GOF workflow sub-tabs
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
  )
}

ui_correlation <- function() {
  tabPanel(
    "Correlation Plots",
    br(),

    fluidRow(
      style = "display: flex; align-items: stretch;",
      # Left column - Input File + Select Axes
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

      # Right column - Plot Options + Plot + Download
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
  )
}

#' PopPK FitCheckR user interface
#'
#' @return A Shiny UI definition.
#' @export
app_ui <- function() {
  fluidPage(
    theme = bs_theme(
      version = 4,
      bootswatch = "flatly",
      primary = "#234aff",
      secondary = "#71a1ff",
      success = "#009E73"
    ),
    tags$head(tags$style(app_css())),

    div(
      class = "container",
      div(class = "page-header", h3("PopPK FitCheckR")),
      br(),

      tabsetPanel(
        id = "main_tabs",
        ui_instructions(),
        ui_gof(),
        ui_correlation()
      )
    ),
    br()
  )
}

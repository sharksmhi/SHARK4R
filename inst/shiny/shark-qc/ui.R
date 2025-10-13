library(shiny)
library(DT)
library(leaflet)
library(SHARK4R)
library(shinythemes)
library(plotly)

shinyUI(
  div(
    class = "scaled-wrapper",
    fluidPage(
      theme = shinytheme("cerulean"),

      # --- Include Google Font
      tags$head(
        tags$style(HTML("
    /* Wrap entire page in a scale transform */
    .scaled-wrapper {
      transform: scale(0.8);   /* 80% zoom */
      transform-origin: top left;
      width: 125%;             /* compensate for scale shrink */
    }
  ")),
        tags$link(
          rel = "stylesheet",
          href = "https://fonts.googleapis.com/css2?family=Bangers&display=swap"
        ),
        tags$style(HTML("
    .app-title {
      font-family: 'Bangers', cursive;
      font-size: 3rem;
      color: #1a1a1a;
      letter-spacing: 1px;
    }
  "))
      ),

      # --- Clipboard script
      tags$script(HTML("
  Shiny.addCustomMessageHandler('setClipboard', function(message) {
    var btn = document.getElementById(message.id);
    if (!btn) return;
    btn.onclick = function() {
      navigator.clipboard.writeText(message.text).then(function() {
        console.log('Copied to clipboard: ' + message.text);
      }, function(err) {
        console.error('Could not copy text: ', err);
      });
    };
  });
")),

      # --- Header
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; align-items: center; justify-content: space-between; padding: 10px 15px 0 15px;",
            tags$a(
              href = "https://sharksmhi.github.io/SHARK4R/",
              target = "_blank",
              tags$img(src = "logo.png", height = "100px", style = "display: block;")
            ),
            h2(tags$strong("SHARK4R Bio-QC Tool"),
               class = "app-title",
               style = "margin: 0 20px; flex-grow: 1; line-height: 1;"),
            h5(
              tags$a(
                href = "https://github.com/sharksmhi/SHARK4R/",
                target = "_blank",
                paste("v", as.character(packageVersion("SHARK4R")))
              ),
              style = "margin: 0; text-align: right; color: #555;"
            )
          )
        )
      ),
      tags$hr(),

      # --- Sidebar + Main content
      sidebarLayout(
        sidebarPanel(
          h4("Environment"),
          selectInput(
            inputId = "env",
            label = "Select Environment:",
            choices = c("PROD", "TEST"),
            selected = "PROD"
          ),

          h4("Data Input"),
          helpText("Download a selected dataset from SHARK OR upload a local ZIP file"),

          # --- Dynamic inputs
          selectizeInput("datatype", "Select Data Type", choices = NULL, options = list(placeholder = 'Choose data type...')),
          selectizeInput("dataset", "Select Dataset", choices = NULL, options = list(placeholder = 'Choose dataset...')),

          # --- Download + Refresh buttons in one row
          fluidRow(
            column(
              width = 8,
              actionButton(
                "downloadDataset",
                "Download Selected Dataset",
                icon = icon("download"),
                class = "btn-primary",
                width = "100%"
              )
            ),
            column(
              width = 4,
              actionButton(
                "refreshData",
                NULL,
                icon = icon("sync"),
                class = "btn-secondary",
                title = "Refresh available datasets"
              )
            )
          ),

          tags$hr(),
          fileInput('file1', 'Or choose a local ZIP File', accept = c('.zip')),
          tags$hr(),
          h4("Report"),
          downloadButton('report', 'Generate Report', icon = icon("file-alt"))
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("Map", leafletOutput("mymap", height = 600)),
            tabPanel("On Land",
                     fluidPage(
                       sliderInput("buffer", "Land Buffer Margin (m):", min = 0, max = 100, value = 0, step = 1),
                       helpText("Allowed distance from land before a position is flagged as 'on land'."),
                       checkboxInput("only_bad", "Show only points on land", value = TRUE),
                       leafletOutput("onland_map", height = 600)
                     )
            ),
            tabPanel("Station Distance",
                     fluidPage(
                       checkboxInput("only_bad_distance", "Show only points out of bounds", value = TRUE),
                       leafletOutput("station_distance", height = 600)
                     ),
                     br(),
                     DT::DTOutput("station_distance_table")
            ),
            tabPanel("Station Matching",
                     fluidPage(
                       DT::DTOutput("station_match_table"),
                       br(),
                       DT::DTOutput("station_nominal_table"),
                       br(),
                       DT::DTOutput("station_zero_table")
                     )
            ),
            tabPanel("Depth Validation",
                     fluidPage(
                       fluidRow(
                         column(
                           width = 6,
                           selectInput(
                             inputId = "depth_col",
                             label = "Select depth column:",
                             choices = NULL,
                             selected = NULL
                           )
                         ),
                         column(
                           width = 6,
                           sliderInput("depthmargin", "Depth Margin (m):",
                                       min = 0, max = 100, value = 0, step = 1)
                         )
                       ),
                       helpText("Allowed deviation above bathymetry before a depth is flagged as an error."),
                       DT::DTOutput("depth_table")
                     )
            ),

            tabPanel(
              "Outliers",

              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "threshold_group",
                    label = "Group thresholds by:",
                    choices = c("Parameter", "Sea basin", "Scientific name"),
                    selected = "Sea basin"
                  )
                )
              ),

              # Show the threshold period
              fluidRow(
                column(
                  width = 12,
                  htmlOutput("threshold_period")
                )
              ),

              # Wrap inputs in a fluidRow for horizontal alignment
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "parameter",
                    label = "Select parameter:",
                    choices = NULL,
                    selected = NULL
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = "threshold_col",
                    label = "Select threshold type:",
                    choices = NULL,
                    selected = "extreme_upper"
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = "direction",
                    label = "Select direction:",
                    choices = c("above", "below"),
                    selected = "above"
                  )
                )
              ),

              # DataTable output
              DT::DTOutput("outliers_table")
            ),
            tabPanel(
              "Scatterplot",

              # First row of inputs
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "scatter_parameter",
                    label = "Select parameter:",
                    choices = NULL,
                    selected = NULL
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = "threshold_col_scatter",
                    label = "Select statistical threshold value:",
                    choices = NULL,
                    selected = "P99"
                  )
                )
              ),

              # Second row of inputs
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "threshold_group_scatter",
                    label = "Group thresholds by:",
                    choices = c("Parameter", "Sea basin", "Scientific name"),
                    selected = "Sea basin"
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = "scatter_group_value",
                    label = "Select group:",
                    choices = NULL,
                    selected = NULL
                  )
                )
              ),

              # Plotly output
              plotlyOutput("scatter_plot", height = 600)
            ),
            tabPanel("Dyntaxa", DT::DTOutput("dyntaxa_table")),
            tabPanel("WoRMS", DT::DTOutput("worms_table")),
            tabPanel(
              "Codes",
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "available_code",
                    label = "Select code:",
                    choices = unique(shark_codes$Data_field),
                    selected = "SHIPC"
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = "field",
                    label = "Select code column:",
                    choices = NULL,
                    selected = "platform_code"
                  )
                )
              ),
              DT::DTOutput("codes_table")
            ),
            tabPanel(
              "Required Fields",
              # Dropdown for selecting level
              selectInput(
                inputId = "check_level",
                label = "Select check level:",
                choices = c("error", "warning"),
                selected = "error"
              ),
              DT::DTOutput("fields_table")
            ),
            tabPanel("Parameter rules", DT::DTOutput("parameter_rules_table")),
            tabPanel("Data Table", DT::DTOutput("table")),
            selected = "Map"
          )
        )
      )
    )))

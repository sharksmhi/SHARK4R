library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(leaflet)
library(plotly)
library(SHARK4R)

app_theme <- bs_theme(
  version = 5,
  preset = "cerulean",
  base_font = font_google("Inter"),
  heading_font = font_google("Bangers")
)

sidebar_styles <- tags$style(HTML("
  /* Align navbar items with the end of the sidebar (340px) */
  .navbar .navbar-brand {
    margin-right: 0 !important;
    padding-right: 0 !important;
  }
  /* Prevent horizontal scrolling in the sidebar when option labels are long */
  .bslib-sidebar-layout > .sidebar { overflow-x: hidden; }
  .bslib-sidebar-layout > .sidebar .selectize-input,
  .bslib-sidebar-layout > .sidebar .selectize-input .item { max-width: 100%; }
  .bslib-sidebar-layout > .sidebar .selectize-input .item {
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    display: inline-block;
  }
  /* Allow the dropdown menu (rendered in <body>) to grow wider than the sidebar */
  body > .selectize-dropdown { min-width: 360px; max-width: 90vw; }
  body > .selectize-dropdown .option {
    white-space: normal;
    word-break: break-word;
  }
"))

clipboard_script <- tags$script(HTML("
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
"))

app_brand <- div(
  style = "display: inline-flex; align-items: center; gap: 10px; line-height: 1; width: 340px; margin-right: 0; padding-right: 0; box-sizing: border-box;",
  tags$a(
    href = "https://sharksmhi.github.io/SHARK4R/",
    target = "_blank",
    style = "display: inline-flex; align-items: center;",
    tags$img(src = "logo.png", height = "32px", alt = "SHARK4R",
             style = "display: block;")
  ),
  span(
    "SHARK4R Bio-QC Tool",
    style = "font-family: 'Bangers', cursive; font-size: 1.4rem; letter-spacing: 1px; line-height: 1;"
  ),
  tags$a(
    href = "https://github.com/sharksmhi/SHARK4R/",
    target = "_blank",
    paste0("v", as.character(packageVersion("SHARK4R"))),
    style = "color: #555; font-size: 0.8rem; text-decoration: none; line-height: 1;"
  )
)

app_sidebar <- sidebar(
  width = 340,
  title = "Controls",
  accordion(
    open = c("Environment", "Data input", "Report"),
    accordion_panel(
      "Environment",
      icon = bs_icon("globe2"),
      selectInput(
        inputId = "env",
        label = "Select environment",
        choices = c("PROD", "TEST"),
        selected = "PROD"
      )
    ),
    accordion_panel(
      "Data input",
      icon = bs_icon("cloud-download"),
      p(
        class = "text-muted small",
        "Download a dataset from SHARK or upload a local ZIP."
      ),
      selectizeInput(
        "datatype", "Data type",
        choices = NULL,
        options = list(
          placeholder = "Choose data type...",
          dropdownParent = "body"
        )
      ),
      selectizeInput(
        "dataset", "Dataset",
        choices = NULL,
        options = list(
          placeholder = "Choose dataset...",
          dropdownParent = "body"
        )
      ),
      layout_columns(
        col_widths = c(8, 4),
        actionButton(
          "downloadDataset",
          label = tagList(bs_icon("download"), "Download"),
          class = "btn-primary",
          width = "100%"
        ),
        tooltip(
          actionButton(
            "refreshData",
            label = bs_icon("arrow-clockwise"),
            class = "btn-secondary",
            width = "100%"
          ),
          "Refresh available datasets"
        )
      ),
      tags$hr(),
      fileInput("file1", "Upload local ZIP", accept = c(".zip"))
    ),
    accordion_panel(
      "Report",
      icon = bs_icon("file-earmark-text"),
      downloadButton(
        "report",
        label = tagList(bs_icon("file-earmark-arrow-down"), "Generate report")
      )
    )
  )
)

map_panel <- nav_panel(
  "Map",
  icon = bs_icon("geo-alt"),
  card(
    full_screen = TRUE,
    card_header("Sample positions"),
    leafletOutput("mymap", height = "100%")
  )
)

onland_panel <- nav_panel(
  "On Land",
  icon = bs_icon("globe-europe-africa"),
  layout_sidebar(
    sidebar = sidebar(
      width = 280, open = "open",
      sliderInput("buffer", "Land buffer margin (m)", min = 0, max = 100, value = 0, step = 1),
      p(class = "text-muted small",
        "Allowed distance from land before a position is flagged as 'on land'."),
      input_switch("only_bad", "Show only points on land", value = TRUE)
    ),
    card(
      full_screen = TRUE,
      card_header("On-land flags"),
      leafletOutput("onland_map", height = "100%")
    )
  )
)

station_distance_panel <- nav_panel(
  "Station Distance",
  icon = bs_icon("rulers"),
  card(
    full_screen = TRUE,
    height = 600,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Distance to nominal station",
      input_switch("only_bad_distance", "Show only points out of bounds", value = TRUE)
    ),
    leafletOutput("station_distance", height = "100%")
  ),
  card(
    full_screen = TRUE,
    card_header("Out-of-bounds positions"),
    DT::DTOutput("station_distance_table")
  )
)

station_matching_panel <- nav_panel(
  "Station Matching",
  icon = bs_icon("pin-map"),
  layout_columns(
    col_widths = c(12),
    card(card_header("Matched stations"), DT::DTOutput("station_match_table")),
    card(card_header("Nominal positions"), DT::DTOutput("station_nominal_table")),
    card(card_header("Zero matches"), DT::DTOutput("station_zero_table"))
  )
)

depth_panel <- nav_panel(
  "Depth Validation",
  icon = bs_icon("water"),
  card(
    full_screen = TRUE,
    card_header(
      "Depth validation",
      class = "d-flex justify-content-between"
    ),
    layout_columns(
      col_widths = c(6, 6),
      selectInput("depth_col", "Depth column", choices = NULL, selected = NULL),
      sliderInput("depthmargin", "Depth margin (m)", min = 0, max = 100, value = 0, step = 1)
    ),
    p(class = "text-muted small",
      "Allowed deviation above bathymetry before a depth is flagged as an error."),
    DT::DTOutput("depth_table")
  )
)

outliers_panel <- nav_panel(
  "Outliers",
  icon = bs_icon("exclamation-triangle"),
  card(
    full_screen = TRUE,
    card_header("Threshold-based outliers"),
    layout_columns(
      col_widths = c(4, 4, 4),
      selectInput("threshold_group", "Group thresholds by",
                  choices = c("Parameter", "Sea basin", "Scientific name"),
                  selected = "Sea basin"),
      selectInput("parameter", "Parameter", choices = NULL),
      selectInput("threshold_col", "Threshold type", choices = NULL, selected = "extreme_upper")
    ),
    selectInput("direction", "Direction",
                choices = c("above", "below"), selected = "above",
                width = "200px"),
    DT::DTOutput("outliers_table")
  )
)

scatter_panel <- nav_panel(
  "Scatterplot",
  icon = bs_icon("graph-up"),
  card(
    full_screen = TRUE,
    card_header("Parameter scatterplot"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      selectInput("scatter_parameter", "Parameter", choices = NULL),
      selectInput("threshold_col_scatter", "Statistical threshold",
                  choices = NULL, selected = "P99"),
      selectInput("threshold_group_scatter", "Group thresholds by",
                  choices = c("Parameter", "Sea basin", "Scientific name"),
                  selected = "Sea basin"),
      selectInput("scatter_group_value", "Group", choices = NULL)
    ),
    plotlyOutput("scatter_plot", height = "100%")
  )
)

dyntaxa_panel <- nav_panel(
  "Dyntaxa", icon = bs_icon("tags"),
  card(full_screen = TRUE, card_header("Dyntaxa check"), DT::DTOutput("dyntaxa_table"))
)

worms_panel <- nav_panel(
  "WoRMS", icon = bs_icon("bezier"),
  card(full_screen = TRUE, card_header("WoRMS check"), DT::DTOutput("worms_table"))
)

codes_panel <- nav_panel(
  "Codes", icon = bs_icon("upc"),
  card(
    full_screen = TRUE,
    card_header("Code validation"),
    layout_columns(
      col_widths = c(6, 6),
      selectInput("available_code", "Code",
                  choices = unique(shark_codes$Data_field),
                  selected = "SHIPC"),
      selectInput("field", "Code column", choices = NULL, selected = "platform_code")
    ),
    DT::DTOutput("codes_table")
  )
)

required_panel <- nav_panel(
  "Required Fields", icon = bs_icon("check2-square"),
  card(
    full_screen = TRUE,
    card_header("Required fields"),
    selectInput("check_level", "Check level",
                choices = c("error", "warning"), selected = "error"),
    DT::DTOutput("fields_table")
  )
)

rules_panel <- nav_panel(
  "Parameter Rules", icon = bs_icon("list-check"),
  card(full_screen = TRUE, card_header("Parameter rules"), DT::DTOutput("parameter_rules_table"))
)

table_panel <- nav_panel(
  "Data Table", icon = bs_icon("table"),
  card(full_screen = TRUE, card_header("Raw data"), DT::DTOutput("table"))
)

shinyUI(
  page_navbar(
    title = app_brand,
    theme = app_theme,
    fillable = TRUE,
    sidebar = app_sidebar,
    header = tagList(sidebar_styles, clipboard_script),
    selected = "Map",
    map_panel,
    onland_panel,
    station_distance_panel,
    station_matching_panel,
    depth_panel,
    outliers_panel,
    scatter_panel,
    dyntaxa_panel,
    worms_panel,
    nav_menu(
      "More",
      icon = bs_icon("three-dots"),
      codes_panel,
      required_panel,
      rules_panel,
      table_panel
    )
  )
)

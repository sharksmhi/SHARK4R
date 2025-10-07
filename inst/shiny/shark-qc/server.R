library(shiny)
library(DT)
library(leaflet)
library(SHARK4R)
library(dplyr)
library(htmltools)
library(skimr)
library(plotly)

shinyServer(function(input, output, session) {

  options(shiny.maxRequestSize = 100*1024^2)

  # --- Reactive expression for whether we're in PROD or TEST
  is_prod <- reactive({
    req(input$env)
    input$env == "PROD"
  })

  # --- Reactive values for options, depends on environment
  options_data <- reactiveVal()

  # --- Load options when app starts or when environment changes
  observeEvent(is_prod(), {
    env_label <- ifelse(is_prod(), "PROD", "TEST")

    # Remove previous error notification if it exists
    if (!is.null(session$userData$server_error_nid)) {
      removeNotification(session$userData$server_error_nid)
      session$userData$server_error_nid <- NULL
    }

    showNotification(paste("Loading", env_label, "options..."), type = "message")

    tryCatch({
      opts <- load_shark_options(prod = is_prod())
      options_data(opts)

      # Update Data Type dropdown
      types <- opts$types
      canonical_types <- type_lookup[types]
      updateSelectizeInput(session, "datatype", choices = canonical_types, server = TRUE)

      # Reset dataset dropdown
      updateSelectizeInput(session, "dataset", choices = NULL, server = TRUE)

      showNotification(paste(env_label, "environment loaded ✅"), type = "message")

    }, error = function(e) {
      # Persistent error notification
      nid <- showNotification(
        paste("❌ Could not connect to", env_label, "server:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
      # Store the notification ID so we can remove it later
      session$userData$server_error_nid <- nid

      # Reset options_data to NULL to prevent follow-up errors
      options_data(NULL)

      # Optionally disable dropdowns
      updateSelectizeInput(session, "datatype", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "dataset", choices = NULL, server = TRUE)
    })
  })

  # --- Populate first dropdown (Data Type)
  observe({
    types <- options_data()$types
    canonical_types <- type_lookup[types]

    updateSelectizeInput(session, "datatype",
                         choices = canonical_types,
                         server = TRUE)
  })

  # --- Populate second dropdown (Dataset) based on selected datatype
  observeEvent(input$datatype, {
    req(input$datatype)

    all_datasets <- options_data()$datasets

    # Filter by selected datatype
    filtered <- grep(input$datatype, all_datasets, value = TRUE)

    # Extract and sort by version date (YYYY-MM-DD)
    dates <- sub(".*_version_(\\d{4}-\\d{2}-\\d{2})\\.zip$", "\\1", filtered)
    dates <- suppressWarnings(as.Date(dates))  # avoid NA warnings
    filtered_sorted <- filtered[order(dates, decreasing = TRUE)]

    updateSelectizeInput(session, "dataset",
                         choices = filtered_sorted,
                         server = TRUE)
  })

  # --- Refresh button to reload datasets for the selected environment
  observeEvent(input$refreshData, {
    showNotification("Refreshing dataset list...", type = "message")

    options_data(load_shark_options(prod = is_prod()))

    types <- options_data()$types
    canonical_types <- type_lookup[types]
    updateSelectizeInput(session, "datatype", choices = canonical_types, server = TRUE)

    updateSelectizeInput(session, "dataset", choices = NULL, server = TRUE)

    showNotification("Dataset list refreshed ✅", type = "message")
  })

  # --- Reactive value to store dataset paths
  dataset_path <- reactiveVal(NULL)

  # --- Download dataset when button pressed
  observeEvent(input$downloadDataset, {
    req(input$dataset)

    shiny::withProgress(message = "Downloading dataset...", {
      df_or_path <- get_shark_datasets(
        save_dir = tempdir(),
        dataset_name = input$dataset,
        return_df = FALSE,  # we can switch to TRUE if you want combined dataframe
        unzip_file = TRUE,
        prod = is_prod(),
        verbose = FALSE
      )
      dataset_path(df_or_path[[1]])  # store first dataset path (unzip dir)
    })

    file_path <- file.path(tempdir(), input$dataset)

    # Show a notification with a copy-to-clipboard action
    showNotification(
      ui = paste("Dataset downloaded successfully ✅"),
      action = tags$button(
        "Copy path",
        class = "btn btn-link btn-sm",
        id = "copyBtn"
      ),
      duration = 5,
      type = "message",
      closeButton = TRUE
    )

    # Send JS to copy file path to clipboard when button clicked
    session$sendCustomMessage("setClipboard", list(
      id = "copyBtn",
      text = file_path
    ))
  })

  # --- Reactive dataset (uploaded or downloaded)
  shark_data <- reactive({
    if (!is.null(input$file1)) {
      read_shark(input$file1$datapath, encoding = "latin_1")
    } else if (!is.null(dataset_path())) {
      # Read downloaded dataset folder
      data_file <- file.path(dataset_path(), "shark_data.txt")
      read_shark(data_file, encoding = "latin_1")
    } else {
      return(NULL)
    }
  })

  observe({
    req(shark_data())

    # Columns to check if present
    possible_depth_cols <- c(
      "water_depth_m", "secchi_depth_m", "sample_depth_m",
      "sample_min_depth_m", "sample_max_depth_m",
      "transect_max_depth_m", "transect_min_depth_m",
      "transect_start_depth_m", "transect_stop_depth_m",
      "section_start_depth_m", "section_end_depth_m"
    )

    available_depth_cols <- intersect(possible_depth_cols, colnames(shark_data()))

    updateSelectInput(
      session,
      "depth_col",
      choices = available_depth_cols,
      selected = ifelse(length(available_depth_cols) > 0, available_depth_cols[1], NULL)
    )
  })

  # Update dropdown choices based on shark_data() columns
  observe({
    req(shark_data())

    updateSelectInput(
      session,
      "field",
      choices = colnames(shark_data()),
      selected = "sample_project_name_en"
    )
  })

  # Update dropdown choices based on shark_data() columns
  observe({
    req(shark_data())

    updateSelectInput(
      session,
      "parameter",
      choices = unique(shark_data()$parameter),
      selected = unique(shark_data()$parameter)[1]
    )
  })

  # --- Render map
  output$mymap <- renderLeaflet({
    req(shark_data())
    coord <- shark_data() %>%
      select(station_name, sample_longitude_dd, sample_latitude_dd) %>%
      rename(STATION = station_name, LON = sample_longitude_dd, LAT = sample_latitude_dd) %>%
      distinct()

    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = coord, lng = ~LON, lat = ~LAT, popup = ~STATION)
  })

  # --- Render check fields QC table
  output$fields_table <- DT::renderDT({
    req(shark_data(), input$datatype)

    datatype_input <- names(type_lookup)[match(input$datatype, type_lookup)]

    df <- check_fields(
      data = shark_data(),
      datatype = input$datatype,
      level = input$check_level
    ) %>%
      select(-row) %>%
      distinct()

    DT::datatable(
      df,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      caption = "Issues found"
    )
  })

  # --- Render check codes QC table
  output$codes_table <- DT::renderDT({
    req(shark_data(), input$field, input$available_code)

    datatype_input <- names(type_lookup)[match(input$datatype, type_lookup)]

    df <- check_codes(
      data = shark_data(),
      field = input$field,
      code_type = input$available_code,
      clean_cache_days = 1,
      verbose = FALSE
    )

    DT::datatable(
      df,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      caption = "Issues found"
    )
  })

  # --- Render check codes QC table
  output$outliers_table <- DT::renderDT({
    req(shark_data(), input$parameter, input$datatype)

    datatype_input <- names(type_lookup)[match(input$datatype, type_lookup)]

    df <- check_outliers(data = shark_data(),
                         parameter = input$parameter,
                         datatype = datatype_input,
                         return_df = TRUE,
                         verbose = FALSE)

    if (is.null(df) || nrow(df) == 0) {
      # Empty datatable with a message
      message_text <- if (is.null(df)) {
        paste("Parameter", input$parameter, "not found in the dataset")
      } else {
        paste(input$parameter, "is within the expected range")
      }

      DT::datatable(
        data.frame(Message = message_text),
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Issues found"
      )
    } else {
      # Show the dataframe
      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        caption = "Issues found"
      )
    }
  })

  # --- Check on land map
  output$onland_map <- renderLeaflet({
    req(shark_data(), input$buffer)  # Ensure dataset is available
    check_onland(shark_data(),
                 plot_leaflet = TRUE,
                 buffer = input$buffer,
                 only_bad = input$only_bad)
  })

  # --- Station distance map
  output$station_distance <- renderLeaflet({
    req(shark_data())  # Ensure dataset is available
    nid <- showNotification("Calculating distance...", duration = NULL)

    # Capture leaflet object
    leaf <- check_station_distance(shark_data(),
                                   plot_leaflet = TRUE,
                                   verbose = FALSE,
                                   only_bad = input$only_bad_distance)
    removeNotification(nid)

    # Return the leaflet object
    leaf
  })

  # --- Station distance table
  output$station_distance_table <- DT::renderDT({
    req(shark_data())
    res <- check_station_distance(data = shark_data(),
                                  plot_leaflet = FALSE,
                                  verbose = FALSE) %>%
      filter(!within_limit)

    DT::datatable(
      res,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      caption = "Samples out of bounds from station registry (station.txt)"
    )
  })

  # --- Station matching table
  output$station_match_table <- DT::renderDT({
    req(shark_data())
    res <- match_station(shark_data()$station_name, verbose = FALSE) %>%
      distinct() %>%
      arrange(reported_station_name) %>%
      rename(station_exists = match_type)

    if (!is.null(res)) {
      DT::datatable(
        res,
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE,
        caption = "Station names matched with station.txt"
      )
    }
  })

  # --- Station nominal check table
  output$station_nominal_table <- DT::renderDT({
    req(shark_data())
    res <- check_nominal_station(data = shark_data(), verbose = FALSE)

    if (!is.null(res) && nrow(res) > 0) {
      res <- res %>% distinct()

      DT::datatable(
        res,
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE,
        caption = "Stations suspected to be nominally reported"
      )
    } else {
      # Empty datatable with a message
      DT::datatable(
        data.frame(Message = "No stations are suspected nominally reported"),
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Stations suspected to be nominally reported"
      )
    }
  })

  # --- Station zero position check table
  output$station_zero_table <- DT::renderDT({
    req(shark_data())
    res <- check_zero_positions(data = shark_data(), return_df = TRUE, verbose = FALSE)

    if (!is.null(res)) {
      res <- res %>%
        distinct()

      DT::datatable(
        res,
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE,
        caption = "Samples on NULL-island"
      )
    } else {
      # Empty datatable with a message
      DT::datatable(
        data.frame(Message = "No samples are on NULL-island"),
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Samples on NULL-island"
      )
    }
  })

  # --- Scatterplot tab
  output$scatter_plot <- renderPlotly({
    req(shark_data(), input$datatype)  # ensure dataset is loaded

    datatype_input <- names(type_lookup)[match(input$datatype, type_lookup)]

    thresh <- SHARK4R:::.threshold_values %>%
      filter(datatype == datatype_input)

    threshold_list <- setNames(thresh$extreme_upper, thresh$parameter)

    scatterplot(shark_data(),
                hline = as.list(threshold_list))
  })

  # --- Render logic rules table
  output$parameter_rules_table <- DT::renderDT({
    req(shark_data())

    df <- suppressWarnings(check_parameter_rules(
      shark_data(),
      verbose = FALSE
    ))

    if (is.null(df)) {
      # Empty datatable with a message
      DT::datatable(
        data.frame(Message = "No rules defined for the parameters in the dataset"),
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Issues found"
      )
    } else if (is.list(df) && length(df) == 0) {
      # Empty datatable with a message
      DT::datatable(
        data.frame(Message = "All measurements within expected range"),
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Issues found"
      )
    } else {
      DT::datatable(
        df,
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE,
        caption = "Issues found"
      )
    }

  })

  # --- Render depth QC table
  output$depth_table <- DT::renderDT({
    req(shark_data(), input$depthmargin, input$depth_col)

    if (input$depth_col %in% c("sample_min_depth_m", "sample_max_depth_m")) {
      depth_col <- c("sample_min_depth_m", "sample_max_depth_m")
    } else {
      depth_col <- input$depth_col
    }

    check_depth(
      shark_data(),
      depth_cols = depth_col,
      depthmargin = input$depthmargin
    ) %>%
      DT::datatable(
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE,
        caption = paste("Issues found (depth column:", paste(depth_col, collapse = ", "), ")")
      )
  })

  # --- Dyntaxa table
  output$dyntaxa_table <- DT::renderDT({
    req(shark_data())

    if ("scientific_name" %in% colnames(shark_data())) {
      nid <- showNotification("Matching taxa against Dyntaxa API...", duration = NULL)

      dyntaxa_res <- is_in_dyntaxa(unique(shark_data()$scientific_name),
                                   use_dwca = TRUE,
                                   return_df = TRUE)

      removeNotification(nid)

      shark_dyntaxa_id <- shark_data() %>%
        select(scientific_name, dyntaxa_id) %>%
        rename(shark_dyntaxa_id = dyntaxa_id) %>%
        distinct()

      dyntaxa_res <- dyntaxa_res %>%
        distinct() %>%
        rename(scientific_name = taxon_names) %>%
        left_join(shark_dyntaxa_id, by = "scientific_name") %>%
        mutate(shark_diff = ifelse(shark_dyntaxa_id == dyntaxa_id, FALSE, TRUE)) %>%
        arrange(scientific_name)

      DT::datatable(
        dyntaxa_res,
        options = list(pageLength = 50, autoWidth = TRUE),
        rownames = FALSE,
        caption = "Taxa found in Dyntaxa"
      ) %>%
        DT::formatStyle(
          columns = names(dyntaxa_res),
          valueColumns = "in_dyntaxa",
          color = DT::styleEqual(
            levels = c(TRUE, FALSE),
            values = c("black", "red")
          )
        ) %>%
        DT::formatStyle(
          columns = names(dyntaxa_res),
          valueColumns = "shark_diff",
          color = DT::styleEqual(
            levels = c(TRUE, FALSE),
            values = c("orange", NA)  # NA = leave as is
          )
        )
    } else {
      # Empty datatable with a message
      DT::datatable(
        data.frame(Dyntaxa = "Column 'scientific_name' not found in the input data"),
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Taxa found in Dyntaxa"
      )
    }
  })

  # --- WoRMS table
  output$worms_table <- DT::renderDT({
    req(shark_data())

    if ("scientific_name" %in% colnames(shark_data())) {
      shark_worms_id <- shark_data() %>%
        select(scientific_name, aphia_id) %>%
        rename(shark_aphia_id = aphia_id) %>%
        distinct()

      nid <- showNotification("Matching taxa against WoRMS API...", duration = NULL)

      worms_res <- match_worms_taxa(unique(shark_data()$scientific_name), marine_only = FALSE,
                                    bulk = TRUE, verbose = FALSE) %>%
        distinct() %>%
        arrange(scientificname) %>%
        rename(aphia_id = AphiaID,
               scientific_name = name) %>%
        mutate(in_worms = ifelse(is.na(aphia_id), FALSE, TRUE)) %>%
        select(scientific_name, in_worms, aphia_id) %>%
        left_join(shark_worms_id, by = "scientific_name") %>%
        mutate(shark_diff = ifelse(shark_aphia_id == aphia_id, FALSE, TRUE))

      removeNotification(nid)

      DT::datatable(
        worms_res,
        options = list(pageLength = 50, autoWidth = TRUE),
        rownames = FALSE,
        caption = "Taxa found in WoRMS"
      ) %>%
        DT::formatStyle(
          columns = names(worms_res),
          valueColumns = "in_worms",
          color = DT::styleEqual(
            levels = c(TRUE, FALSE),
            values = c("black", "red")
          )
        ) %>%
        DT::formatStyle(
          columns = names(worms_res),
          valueColumns = "shark_diff",
          color = DT::styleEqual(
            levels = c(TRUE, FALSE),
            values = c("orange", NA)  # NA = leave as is
          )
        )
    } else {
      # Empty datatable with a message
      DT::datatable(
        data.frame(WoRMS = "Column 'scientific_name' not found in the input data"),
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Taxa found in WoRMS"
      )
    }
  })

  # --- Render raw table
  output$table <- DT::renderDT({
    req(shark_data())
    DT::datatable(
      shark_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })

  # --- Render report
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      nid <- showNotification("Rendering report...", duration = NULL)

      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      params <- list(
        targetDataset = if (!is.null(input$file1)) {
          input$file1$datapath
        } else if (!is.null(dataset_path())) {
          dataset_path()
        } else {
          NULL
        },
        depth_margin = input$depthmargin,
        land_buffer = input$buffer,
        field = input$field,
        code_type = input$available_code,
        only_bad = input$only_bad,
        only_bad_distance = input$only_bad_distance,
        depth_col = input$depth_col
      )

      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )

      removeNotification(nid)
    }
  )
})

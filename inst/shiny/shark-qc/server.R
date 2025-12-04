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

  observe({
    req(options_data())                # <- prevent runs with NULL options_data
    types <- options_data()$types      # internal keys, e.g. "Grey seal"
    # display labels (human) come from type_lookup[types], keep values = types
    choices <- setNames(types, type_lookup[types])
    updateSelectizeInput(session, "datatype", choices = choices, server = TRUE)
  })

  observeEvent(input$datatype, {
    req(input$datatype, options_data())

    all_datasets <- options_data()$datasets

    # build tolerant patterns (with/without space, underscore)
    key <- input$datatype                            # e.g. "Grey seal"
    no_space <- gsub(" ", "", key)                   # "Greyseal"
    underscore <- gsub(" ", "_", key)                # "Grey_seal"
    canonical <- type_lookup[key] %||% ""            # "GreySeal" (use %||% from rlang or fallback)
    patterns <- paste(c(key, no_space, underscore, canonical), collapse = "|")

    filtered <- grep(patterns, all_datasets, value = TRUE, ignore.case = TRUE, perl = TRUE)

    # rest unchanged: parse versions and sort
    dates <- sub(".*_version_(\\d{4}-\\d{2}-\\d{2})\\.zip$", "\\1", filtered)
    dates <- suppressWarnings(as.Date(dates))
    filtered_sorted <- filtered[order(dates, decreasing = TRUE)]

    updateSelectizeInput(session, "dataset", choices = filtered_sorted, server = TRUE)
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

  # --- Reactive value to store datasets
  dataset <- reactiveVal(NULL)

  # --- Reactive value to store dataset paths
  dataset_path <- reactiveVal(NULL)

  # --- Download dataset when button pressed
  observeEvent(input$downloadDataset, {
    req(input$dataset)

    file_path <- file.path(tempdir(), gsub(".zip", ".txt", basename(input$dataset)))

    shiny::withProgress(message = "Downloading dataset...", {
      df <- get_shark_data(
        tableView = "sharkweb_all",
        save_data = TRUE,
        encoding = "latin_1",
        file_path = file_path,
        datasets = input$dataset,
        hideEmptyColumns = TRUE,
        prod = is_prod(),
        verbose = FALSE
      ) %>%
        select(where(~ any(!is.na(.))))

      # Convert types
      df <- suppressWarnings(type.convert(df))

      dataset(df)

      dataset_path(file_path)

    })

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
    } else if (!is.null(dataset()) && is.data.frame(dataset())) {
      # Read downloaded dataset folder
      dataset()
    } else {
      return(NULL)
    }
  })

  observe({
    req(shark_data())

    possible_depth_cols <- c(
      "water_depth_m", "secchi_depth_m", "sample_depth_m",
      "sample_min_depth_m", "sample_max_depth_m",
      "transect_max_depth_m", "transect_min_depth_m",
      "transect_start_depth_m", "transect_stop_depth_m",
      "section_start_depth_m", "section_end_depth_m"
    )

    available_depth_cols <- intersect(possible_depth_cols, colnames(shark_data()))

    if (length(available_depth_cols) > 0) {
      selected_col <- available_depth_cols[1]
    } else {
      selected_col <- NULL
    }

    updateSelectInput(session,
                      "depth_col",
                      choices = available_depth_cols,
                      selected = selected_col)
  })

  # Update dropdown choices based on shark_data() columns
  observe({
    req(shark_data())

    updateSelectInput(
      session,
      "field",
      choices = colnames(shark_data()),
      selected = "platform_code"
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

  # Update dropdown choices based on shark_data() columns
  observe({
    req(shark_data())

    updateSelectInput(
      session,
      "scatter_parameter",
      choices = unique(shark_data()$parameter),
      selected = unique(shark_data()$parameter)[1]
    )
  })

  # Load thresholds
  thresholds <- reactive({
    req(input$threshold_group)

    # Map dropdown choices to specific RDS files
    rds_file <- switch(input$threshold_group,
                       "Parameter" = "parameter.rds",
                       "Sea basin" = "sea_basin.rds",
                       "Scientific name" = "scientific_name.rds",
                       "parameter.rds"  # fallback (default)
    )

    load_shark4r_stats(file_name = rds_file,
                       verbose = FALSE)
  })

  # Update dropdown choices based on shark_data() columns
  observe({
    req(shark_data(), input$threshold_group_scatter)

    # Map dropdown choices to specific RDS files
    group_col <- switch(input$threshold_group_scatter,
                        "Parameter" = "parameter",
                        "Sea basin" = "location_sea_basin",
                        "Scientific name" = "scientific_name",
                        "parameter"  # fallback (default)
    )

    unique(shark_data()[[group_col]]) %>%
      na.omit() %>%
      as.character() %>%
      sort() -> choices

    updateSelectInput(
      session,
      "scatter_group_value",
      choices = c("All", choices),
      selected = "All"
    )
  })

  # Update threshold_col dropdown dynamically
  observe({
    req(thresholds())

    # Columns to use
    cols <- setdiff(
      colnames(thresholds()),
      c("parameter", "datatype", "location_sea_basin", "n", "fromYear", "toYear")
    )

    updateSelectInput(
      session,
      "threshold_col",
      choices = cols,
      selected = "P99"
    )
  })

  # Load thresholds for scatterplot
  thresholds_scatter <- reactive({
    req(input$threshold_group_scatter)

    # Map dropdown choices to specific RDS files
    rds_file <- switch(input$threshold_group_scatter,
                       "Parameter" = "parameter.rds",
                       "Sea basin" = "sea_basin.rds",
                       "Scientific name" = "scientific_name.rds",
                       "parameter.rds"  # fallback (default)
    )

    load_shark4r_stats(file_name = rds_file,
                       verbose = FALSE)
  })

  # Update threshold_col dropdown dynamically
  observe({
    req(thresholds_scatter())

    # Columns to use
    cols <- setdiff(
      colnames(thresholds()),
      c("parameter", "datatype", "location_sea_basin", "n", "fromYear", "toYear")
    )

    updateSelectInput(
      session,
      "threshold_col_scatter",
      choices = cols,
      selected = "P99"
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

    datatype_key <- input$datatype
    datatype_canonical <- type_lookup[datatype_key]

    if (is.na(datatype_canonical)) {
      m <- match(tolower(gsub("\\s+", "", input$datatype)), tolower(gsub("\\s+", "", type_lookup)))
      if (!is.na(m)) datatype_key <- names(type_lookup)[m]
      datatype_canonical <- type_lookup[datatype_key]
    }

    df <- check_fields(
      data = shark_data(),
      datatype = datatype_canonical,
      level = input$check_level,
      field_definitions = shark_fields
    ) %>%
      select(-row) %>%
      distinct()

    DT::datatable(
      df,
      style = "bootstrap",
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      caption = "Issues found"
    )
  })

  # --- Render check codes QC table
  output$codes_table <- DT::renderDT({
    req(shark_data(), input$field, input$available_code)

    df <- check_codes(
      data = shark_data(),
      field = input$field,
      code_type = input$available_code,
      clean_cache_days = 1,
      verbose = FALSE
    )

    DT::datatable(
      df,
      style = "bootstrap",
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      caption = "Issues found"
    )
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
      style = "bootstrap",
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
        style = "bootstrap",
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
        style = "bootstrap",
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE,
        caption = "Stations suspected to be nominally reported"
      )
    } else {
      # Empty datatable with a message
      DT::datatable(
        data.frame(Message = "No stations are suspected nominally reported"),
        style = "bootstrap",
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
        style = "bootstrap",
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE,
        caption = "Samples on NULL-island"
      )
    } else {
      # Empty datatable with a message
      DT::datatable(
        data.frame(Message = "No samples are on NULL-island"),
        style = "bootstrap",
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Samples on NULL-island"
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
        style = "bootstrap",
        rownames = FALSE,
        caption = paste("Issues found (depth column:", paste(depth_col, collapse = ", "), ")")
      )
  })

  # --- Render check outliers QC table
  output$outliers_table <- DT::renderDT({
    req(shark_data(), thresholds(), input$parameter, input$datatype, input$threshold_col, input$direction, input$threshold_group)

    # Map dropdown choices to specific RDS files
    group_col <- switch(input$threshold_group,
                        "Parameter" = "parameter",
                        "Sea basin" = "location_sea_basin",
                        "Scientific name" = "scientific_name",
                        "parameter"  # fallback (default)
    )

    # Determine datatype safely
    datatype_input <- input$datatype

    if (!datatype_input %in% thresholds()$datatype) {
      alt_form <- gsub("\\s+", "", datatype_input)
      match_idx <- match(tolower(alt_form),
                         tolower(gsub("\\s+", "", thresholds()$datatype)))
      if (!is.na(match_idx)) {
        datatype_input <- thresholds()$datatype[match_idx]
      }
    }

    # --- Capture warnings from check_outliers()
    warning_msg <- NULL
    df <- withCallingHandlers(
      check_outliers(
        data = shark_data(),
        parameter = input$parameter,
        datatype = datatype_input,
        thresholds = thresholds(),
        threshold_col = input$threshold_col,
        custom_group = group_col,
        direction = input$direction,
        return_df = TRUE,
        verbose = FALSE
      ),
      warning = function(w) {
        warning_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")  # Prevent console warning
      }
    )

    # --- Notify user if there was a warning
    if (!is.null(warning_msg)) {
      showNotification(
        paste("check_outliers warning:", warning_msg),
        type = "warning",
        duration = 10
      )
    }

    # --- Render results
    if (is.null(df) || nrow(df) == 0) {
      message_text <- if (is.null(df)) {
        paste("Parameter", input$parameter, "not found in the dataset or in the threshold dataset")
      } else {
        paste(input$parameter, "is within the expected range")
      }

      DT::datatable(
        data.frame(Message = message_text),
        style = "bootstrap",
        options = list(dom = 't', paging = FALSE),
        rownames = FALSE,
        caption = "Issues found"
      )
    } else {
      DT::datatable(
        df,
        style = "bootstrap",
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

  # --- Scatterplot tab
  output$scatter_plot <- renderPlotly({
    req(shark_data(), input$datatype, input$scatter_parameter, input$threshold_col_scatter, input$threshold_group_scatter, input$scatter_group_value)

    datatype_input <- input$datatype

    # If not directly found in the thresholds, try a space-insensitive match
    if (!datatype_input %in% thresholds_scatter()$datatype) {
      alt_form <- gsub("\\s+", "", datatype_input)
      match_idx <- match(
        tolower(alt_form),
        tolower(gsub("\\s+", "", thresholds_scatter()$datatype))
      )
      if (!is.na(match_idx)) {
        datatype_input <- thresholds_scatter()$datatype[match_idx]
      }
    }

    # Map dropdown choices to specific RDS files
    group_col <- switch(input$threshold_group_scatter,
                        "Parameter" = "parameter",
                        "Sea basin" = "location_sea_basin",
                        "Scientific name" = "scientific_name",
                        "parameter"  # fallback (default)
    )

    # Filter thresholds
    thresh <- thresholds_scatter() %>%
      dplyr::filter(datatype == datatype_input) %>%
      dplyr::filter(parameter == input$scatter_parameter)

    threshold_col <- input$threshold_col_scatter
    threshold_group <- input$threshold_group_scatter

    # Build a smaller threshold tibble with only the relevant columns
    thresh_subset <- thresh %>%
      dplyr::select(
        parameter,
        !!sym(group_col),
        !!sym(threshold_col)
      )

    data <- shark_data() %>%
      filter(parameter == input$scatter_parameter)

    available_groups <- unique(data[[group_col]])

    thresh_subset <- thresh_subset %>%
      filter((.data[[group_col]] %in% available_groups))

    if (input$scatter_group_value != "All") {
      data <- data %>%
        filter((.data[[group_col]] == input$scatter_group_value))

      thresh_subset <- thresh_subset %>%
        filter((.data[[group_col]] == input$scatter_group_value))
    }

    scatterplot(
        data,
        hline = thresh_subset,
        hline_group_col = group_col,
        hline_value_col = threshold_col
      )
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
        style = "bootstrap",
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Issues found"
      )
    } else if (is.list(df) && length(df) == 0) {
      # Empty datatable with a message
      DT::datatable(
        data.frame(Message = "All measurements within expected range"),
        style = "bootstrap",
        options = list(dom = 't', paging = FALSE),  # hide search/pagination
        rownames = FALSE,
        caption = "Issues found"
      )
    } else {
      DT::datatable(
        df,
        style = "bootstrap",
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE,
        caption = "Issues found"
      )
    }

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
        rename(scientific_name = taxon_name) %>%
        left_join(shark_dyntaxa_id, by = "scientific_name") %>%
        mutate(shark_diff = ifelse(shark_dyntaxa_id == dyntaxa_id, FALSE, TRUE)) %>%
        arrange(scientific_name)

      DT::datatable(
        dyntaxa_res,
        style = "bootstrap",
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
        style = "bootstrap",
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

      # Find unique names and remove NA
      scientific_names <- unique(shark_data()$scientific_name)[!is.na(unique(shark_data()$scientific_name))]

      worms_res <- match_worms_taxa(scientific_names, marine_only = FALSE,
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
        style = "bootstrap",
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
        style = "bootstrap",
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
      style = "bootstrap",
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
        depth_col = input$depth_col,
        threshold_col = input$threshold_col,
        parameter = input$parameter,
        direction = input$direction,
        threshold_group = input$threshold_group
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

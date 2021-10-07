options(shiny.maxRequestSize=100*1024^2)

library(SHARK4R)
library(shiny)
library(htmltools)
library(leaflet)
library(DT)
library(rmarkdown)

shinyServer(function(input, output) {
    output$table <- DT::renderDT({

        # input$file1 will be NULL initially. After the user selects and uploads a
        # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
        # columns. The 'datapath' column will contain the local filenames where the
        # data can be found.

        inFile <- input$file1

        if (is.null(inFile))
            return(NULL)

        SHARK4R::shark_read_zip(inFile$datapath)

    })
    
    output$mymap <- renderLeaflet({
        
        # input$file1 will be NULL initially. After the user selects and uploads a
        # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
        # columns. The 'datapath' column will contain the local filenames where the
        # data can be found.
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        coord = SHARK4R::shark_read_zip(inFile$datapath) %>% 
            select(station_name, sample_longitude_dd, sample_latitude_dd) %>% 
            rename(STATION = station_name, LON = sample_longitude_dd, LAT = sample_latitude_dd) %>% 
            distinct()
        
        leaflet() %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data = coord, popup = ~STATION)
    })

    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        
        content = function(file) {
            
            withProgress(message = 'Rendering report, please wait!', {
            
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            inFile <- input$file1
            
            if (is.null(inFile))
                return(NULL)            
            
            params <- list(targetDataset = inFile$datapath)
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
            })
        }
        
    )
    
})
library(SHARK4R)
library(shiny)
library(htmltools)
library(leaflet)
library(DT)
library(rmarkdown)

shinyUI(pageWithSidebar(
    headerPanel("SHARK4R QC"),
    sidebarPanel(
        helpText(),
        fileInput('file1', 'Choose ZIP File',
                  accept=c('.zip')),

        tags$hr(),
        
        leafletOutput("mymap"),
        
        tags$hr(),
        
        downloadButton('report', 'Generate report')
        

    ),
    mainPanel(
        DT::DTOutput(outputId = "table")
    )
))
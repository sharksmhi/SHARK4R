#' Scatterplot of user-defined variables and parameters with x- and y-axes automatic selection
#' Interactive plotly plot
#' @param data for tibble be be checked
#' @param ... Additional parameters
#' @return plotly plot 
#' @export scatterplot
#' @importFrom plotly plot_ly layout
## TODO:changing axis layout to logarithmic scale would be useful 

scatterplot <- function(data, ...) {
  
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = TRUE,
    showgrid = TRUE
  )
  
  plot_ly(data, x = ~station_name, y = ~value, type = "scatter", mode = "markers", visible = T) %>%
    layout(
      xaxis = ax,
      yaxis = ax,
      updatemenus = list(
        ## Y-AXIS
        list(
          y = 0.7,
          buttons = list(
            list(method = "restyle",
                 args = list("y", list(data$value)),  
                 label = "Y = VALUE"))),
        ## X-AXIS
        list(
          x = 0.7,
          buttons = list(
            list(method = "restyle",
                 args = list("x", list(data$station_name)),  
                 label = "X = STATION_NAME"),
            list(method = "restyle",
                 args = list("x", list(data$sample_date)),  
                 label = "X = SAMPLE_DATE")))
      ))
}

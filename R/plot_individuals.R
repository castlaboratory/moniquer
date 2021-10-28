# Author: Gabriel Teotonio
# Title: Function to plot Individuals control chart
# Date: 2021-10-14

#' Plot control graphic Individuals
#'
#' @param x Variable of measurements
#' @param k Variable for sample index
#' @param data Data set with the variables n, d, and k
#' @return A Plotly graphic
#' @export
plot_individuals <- function(x, k, data) {
  # Filters -----
  if (dim(data)[1] == 0)  {
    stop("data must be a non-empty dataframe.")
  }

  # if (!(n %in% colnames(data)) || !(d %in% colnames(data)) || !(k %in% colnames(data))) {
  #   stop("all columns (n, d, k) must be in the dataframe.")
  # }

  # Calculation -----
  data$k <- data[[k]]
  data$x <- data[[x]]
  data$x_bar <- sum(data$x)/dim(data)[1]
  data$mr <- abs(data$x - lag(data$x, n = 1L, order_by = data$k, default = 0))
  data$mr_bar <- mean(data$mr)
  data$mr_bar_for_x <- mean(data[-1,]$mr)

  data$x_ucl <- data$x_bar + (2.66 * data$mr_bar_for_x)
  data$x_lcl <- data$x_bar - (2.66 * data$mr_bar_for_x)

  data$mr_ucl <- 3.27 * data$mr_bar_for_x
  data$mr_lcl <- 0

  # Build graphic -----
  plot_x <- data %>%
    plot_ly(x = ~k,
            y = ~x,
            mode = "lines",
            source = "plot_x") %>%
    add_trace(y = ~x, name = 'Valor individual', mode = 'lines+markers') %>%
    add_trace(y = ~x_bar,
              name = TeX("\\bar{x}"),
              type = 'scatter',
              mode = 'lines',
              color = I("green"),
              line = list(shape = 'linear', width= 3, dash = 'dot'),
              connectgaps = TRUE) %>%
    add_trace(y = ~x_ucl,
              name = 'LSC',
              mode = 'lines',
              color = I("red"),
              line = list(shape = "hv")) %>%
    add_trace(y = ~x_lcl,
              name = 'LIC',
              mode = 'lines',
              color = I("red"),
              line = list(shape = "hv")) %>%
    layout(title = paste0("Gráfico Individual de ", names(data[x])),
           xaxis = list(title = 'Amostra'),
           yaxis = list(title = 'Valor individual'),
           hovermode = "x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                      'hoverClosestCartesian', 'hoverCompareCartesian',
                                      'select2d','lasso2d', 'toggleSpikelines'),
           displayModeBar = TRUE,
           displaylogo = FALSE ,
           locale = 'pt-BR',
           mathjax = 'cdn')

  plot_mr <- data[-1,] %>%
    plot_ly(x = ~k,
            y = ~mr,
            mode = "lines",
            source = "plot_mr") %>%
    add_trace(y = ~mr, name = 'Amplitude móvel', mode = 'lines+markers') %>%
    add_trace(y = ~mr_bar_for_x,
              name = TeX("\\bar{mr}"),
              type = 'scatter',
              mode = 'lines',
              color = I("green"),
              line = list(shape = 'linear', width= 3, dash = 'dot'),
              connectgaps = TRUE) %>%
    add_trace(y = ~mr_ucl,
              name = 'LSC',
              mode = 'lines',
              color = I("red"),
              line = list(shape = "hv")) %>%
    add_trace(y = ~mr_lcl,
              name = 'LIC',
              mode = 'lines',
              color = I("red"),
              line = list(shape = "hv")) %>%
    layout(title = paste0("Gráfico Amplitude móvel de ", names(data[x])),
           xaxis = list(title = 'Amostra'),
           yaxis = list(title = 'Amplitude móvel'),
           hovermode = "x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                      'hoverClosestCartesian', 'hoverCompareCartesian',
                                      'select2d','lasso2d', 'toggleSpikelines'),
           displayModeBar = TRUE,
           displaylogo = FALSE ,
           locale = 'pt-BR',
           mathjax = 'cdn')

  return(list("plot_x" = plot_x,
              "plot_mr" = plot_mr))
}

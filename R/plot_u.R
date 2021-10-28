# Author: Gabriel Teotonio
# Title: Function to plot U control chart
# Date: 2021-10-14

#' Plot control graphic U
#'
#' @param n Variable for sample size (opportunity area)
#' @param c Variable for number of occurrences
#' @param k Variable for sample index
#' @param data Data set with the variables n, d, and k
#' @return A Plotly graphic
#' @export
plot_u <- function(n, c, k, data) {
  # Filters -----
  if (dim(data)[1] == 0)  {
    stop("data must be a non-empty dataframe.")
  }

  # if (!(n %in% colnames(data)) || !(d %in% colnames(data)) || !(k %in% colnames(data))) {
  #   stop("all columns (n, d, k) must be in the dataframe.")
  # }

  # Calculation -----
  data$k <- data[[k]]
  data$u <- 100 * (data[[c]]/data[[n]])
  data$u_bar <- 100 * (sum(data[[c]])/sum(data[[n]]))

  data$ucl <- data$u_bar + (3 * (sqrt(data$u_bar/data[[n]])))
  data$lcl <- data$u_bar - (3 * (sqrt(data$u_bar/data[[n]])))

  # Build graphic -----
  data %>%
    plot_ly(x = ~k,
            y = ~u,
            mode = "lines",
            source = "plot_u") %>%
    add_trace(y = ~u, name = 'Contagem por unidade', mode = 'lines+markers') %>%
    add_trace(y = ~u_bar,
              name = TeX("\\bar{u}"),
              type = 'scatter',
              mode = 'lines',
              color = I("green"),
              line = list(shape = 'linear', width= 3, dash = 'dot'),
              connectgaps = TRUE) %>%
    add_trace(y = ~ucl,
              name = 'LSC',
              mode = 'lines',
              color = I("red"),
              line = list(shape = "hv")) %>%
    add_trace(y = ~lcl,
              name = 'LIC',
              mode = 'lines',
              color = I("red"),
              line = list(shape = "hv")) %>%
    layout(title = paste0("Gráfico U de ", names(data[c])),
           xaxis = list(title = 'Amostra'),
           yaxis = list(title = 'Contagem por unidade'),
           hovermode = "x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                      'hoverClosestCartesian', 'hoverCompareCartesian',
                                      'select2d','lasso2d', 'toggleSpikelines'),
           displayModeBar = TRUE,
           displaylogo = FALSE ,
           locale = 'pt-BR',
           mathjax = 'cdn')
}

# Author: Gabriel Teotonio
# Title: Function to plot U control chart
# Date: 2021-10-14

#' Plot control graphic U
#'
#' @param n Variable for sample size (opportunity area)
#' @param c Variable for number of occurrences
#' @param k Variable for sample index
#' @param phase Value to indicate phases range
#' @param data Data set with the variables n, d, and k
#' @return A Plotly graphic
#' @export
plot_u <- function(n, c, k, phase, data) {
  # Filters -----
  if (dim(data)[1] == 0)  {
    stop("data must be a non-empty dataframe.")
  }

  if (is.na(phase)) {
    stop("phase must be a valid index for data observations.")
  }

  # if (!(n %in% colnames(data)) || !(d %in% colnames(data)) || !(k %in% colnames(data))) {
  #   stop("all columns (n, d, k) must be in the dataframe.")
  # }

  # Calculation -----
  data$k <- data[[k]]
  data_phase_1 <- data[1:phase, ]
  data_phase_2 <- data[phase:nrow(data), ]

  data_phase_1$u <- 100 * (data_phase_1[[c]]/data_phase_1[[n]])
  data_phase_2$u <- 100 * (data_phase_2[[c]]/data_phase_2[[n]])
  data_phase_1$u_bar <- 100 * (sum(data_phase_1[[c]])/sum(data_phase_1[[n]]))

  data_phase_1$ucl <- data_phase_1$u_bar + (3 * (sqrt(data_phase_1$u_bar/data_phase_1[[n]])))
  data_phase_1$lcl <- data_phase_1$u_bar - (3 * (sqrt(data_phase_1$u_bar/data_phase_1[[n]])))

  # Build graphic -----
  data_phase_1 %>%
    plot_ly(x = ~k,
            y = ~u,
            mode = "lines",
            source = "plot_u") %>%
    add_trace(y = ~u,
              name = 'Contagem por unidade',
              mode = 'lines+markers',
              color = I("blue")) %>%
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
    layout(title = paste0("Gráfico U de ", names(data_phase_1[c])),
           xaxis = list(title = 'Amostra'),
           yaxis = list(title = 'Contagem por unidade'),
           hovermode = "x unified",
           showlegend = FALSE) %>%
    add_trace(data = data_phase_2,
              name = "Contagem por unidade",
              x = ~k,
              y = ~u,
              color = I("blue"),
              mode = 'lines+markers') %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                      'hoverClosestCartesian', 'hoverCompareCartesian',
                                      'select2d','lasso2d', 'toggleSpikelines'),
           displayModeBar = TRUE,
           displaylogo = FALSE ,
           locale = 'pt-BR',
           mathjax = 'cdn')
}

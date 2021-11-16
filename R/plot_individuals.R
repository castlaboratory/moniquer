# Author: Gabriel Teotonio
# Title: Function to plot Individuals control chart
# Date: 2021-10-14

#' Plot control graphic Individuals
#'
#' @param x Variable of measurements
#' @param k Variable for sample index
#' @param phase Value to indicate phases range
#' @param append Indicator to generate append at base plot
#' @param dt_append Value to indicate append phase range
#' @param base_plot Plotly object from base plot
#' @param data Data set with the variables n, d, and k
#' @return A Plotly graphic
#' @export
plot_individuals <- function(x, k, phase, append = FALSE, index_append = NULL, base_plot = NULL,  data) {
  # Filters -----
  if (dim(data)[1] == 0)  {
    stop("data must be a non-empty dataframe.")
  }

  if (is.na(phase)) {
    stop("phase must be a valid index for data observations.")
  }

  # Calculation -----
  data$k <- data[[k]]
  data$x <- data[[x]]

  data_phase_1 <- data[1:phase, ]
  data_phase_2 <- data[(phase - 1):nrow(data), ]

  if (append == TRUE) {
    data_phase_1 <- data[phase:index_append, ]
  }

  data_phase_1$x_bar <- sum(data_phase_1$x)/dim(data_phase_1)[1]

  data_phase_1$mr <- abs(data_phase_1$x - lag(data_phase_1$x, n = 1L, order_by = data_phase_1$k, default = 0))
  data_phase_2$mr <- abs(data_phase_2$x - lag(data_phase_2$x, n = 1L, order_by = data_phase_2$k, default = 0))

  data_phase_1$mr_bar <- mean(data_phase_1$mr)
  data_phase_1$mr_bar_for_x <- mean(data_phase_1[-1,]$mr)

  data_phase_1$x_ucl <- data_phase_1$x_bar + (2.66 * data_phase_1$mr_bar_for_x)
  data_phase_1$x_lcl <- data_phase_1$x_bar - (2.66 * data_phase_1$mr_bar_for_x)

  data_phase_1$mr_ucl <- 3.27 * data_phase_1$mr_bar_for_x
  data_phase_1$mr_lcl <- 0

  # Build graphic -----
  if (append == FALSE) {
    plot_x <- data_phase_1 %>%
      plot_ly(x = ~k,
              y = ~x,
              mode = "lines",
              source = "plot_x") %>%
      add_trace(y = ~x,
                name = 'Valor individual',
                mode = 'lines+markers',
                color = I("blue")) %>%
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
      layout(title = paste0("Gráfico Individual de ", names(data_phase_1[x])),
             xaxis = list(title = 'Amostra'),
             yaxis = list(title = 'Valor individual'),
             hovermode = "x unified",
             showlegend = FALSE) %>%
      add_trace(data = data_phase_2,
                name = "Valor individual",
                x = ~k,
                y = ~x,
                color = I("blue"),
                mode = 'lines+markers') %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                        'hoverClosestCartesian', 'hoverCompareCartesian',
                                        'select2d','lasso2d', 'toggleSpikelines'),
             displayModeBar = TRUE,
             displaylogo = FALSE ,
             locale = 'pt-BR',
             mathjax = 'cdn')

    plot_mr <- data_phase_1[-1,] %>%
      plot_ly(x = ~k,
              y = ~mr,
              mode = "lines",
              source = "plot_mr") %>%
      add_trace(y = ~mr,
                name = 'Amplitude móvel',
                mode = 'lines+markers',
                color = I("blue")) %>%
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
      layout(title = paste0("Gráfico Amplitude móvel de ", names(data_phase_1[x])),
             xaxis = list(title = 'Amostra'),
             yaxis = list(title = 'Amplitude móvel'),
             hovermode = "x unified",
             showlegend = FALSE) %>%
      add_trace(data = data_phase_2[-1,],
                name = "Amplitude móvel",
                x = ~k,
                y = ~mr,
                color = I("blue"),
                mode = 'lines+markers') %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                        'hoverClosestCartesian', 'hoverCompareCartesian',
                                        'select2d','lasso2d', 'toggleSpikelines'),
             displayModeBar = TRUE,
             displaylogo = FALSE ,
             locale = 'pt-BR',
             mathjax = 'cdn')
  } else {
    plot_x <- base_plot %>%
      add_trace(data = data_phase_1,
                y = ~x_bar,
                name = TeX("\\bar{x}"),
                type = 'scatter',
                mode = 'lines',
                color = I("green"),
                line = list(shape = 'linear', width= 3, dash = 'dot'),
                connectgaps = TRUE) %>%
      add_trace(data = data_phase_1,
                y = ~x_ucl,
                name = 'LSC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      add_trace(data = data_phase_1,
                y = ~x_lcl,
                name = 'LIC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv"))

    plot_mr <- base_plot %>%
      add_trace(data = data_phase_1,
                y = ~mr_bar_for_x,
                name = TeX("\\bar{mr}"),
                type = 'scatter',
                mode = 'lines',
                color = I("green"),
                line = list(shape = 'linear', width= 3, dash = 'dot'),
                connectgaps = TRUE) %>%
      add_trace(data = data_phase_1,
                y = ~mr_ucl,
                name = 'LSC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      add_trace(data = data_phase_1,
                y = ~mr_lcl,
                name = 'LIC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv"))
  }


  return(list("plot_x" = plot_x,
              "plot_mr" = plot_mr))
}

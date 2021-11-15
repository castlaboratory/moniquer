# Author: Gabriel Teotonio
# Title: Function to plot P control chart
# Date: 2021-10-14

#' Plot control graphic P
#'
#' @param n Variable for sample size
#' @param d Variable for non-expected observations
#' @param k Variable for sample index
#' @param phase Value to indicate phases range
#' @param append Indicator to generate append at base plot
#' @param dt_append Value to indicate append phase range
#' @param base_plot Plotly object from base plot
#' @param data Data set with the variables n, d, and k
#' @return A Plotly graphic
#' @export
plot_p <- function(n, d, k, phase, append = FALSE, index_append = NULL, base_plot = NULL, data) {
  # Filters -----
  if (dim(data)[1] == 0)  {
    stop("data must be a non-empty dataframe.")
  }

  if (unique(data[[n]] < data[[d]]) %in% c("TRUE")) {
    stop("d values must be equal or less than n values.")
  }

  if (is.na(phase)) {
    stop("phase must be a valid index for data observations.")
  }

  # Calculation -----
  data$k <- data[[k]]
  data_phase_1 <- data[1:phase, ]
  data_phase_2 <- data[phase:nrow(data), ]

  if (append == TRUE) {
    data_phase_1 <- data[phase:index_append, ]
  }


  if (length(unique(data_phase_1[[n]])) == 1) {
    data_phase_1$p <- 100 * (data_phase_1[[d]]/data_phase_1[[n]])
    data_phase_2$p <- 100 * (data_phase_2[[d]]/data_phase_2[[n]])

    data_phase_1$p_bar <- sum(data_phase_1$p)/dim(data_phase_1)[1]
    data_phase_1$sigma_hat <- sqrt((data_phase_1$p_bar * (100 - data_phase_1$p_bar))/data_phase_1[[n]])

    data_phase_1$ucl <- data_phase_1$p_bar + (3 * data_phase_1$sigma_hat)
    data_phase_1$lcl <- data_phase_1$p_bar - (3 * data_phase_1$sigma_hat)
  } else {
    data_phase_1$p <- 100 * (data_phase_1[[d]]/data_phase_1[[n]])
    data_phase_2$p <- 100 * (data_phase_2[[d]]/data_phase_2[[n]])

    data_phase_1$p_bar <- 100 * (sum(data_phase_1[[d]])/sum(data_phase_1[[n]]))
    data_phase_1$sigma_hat <- sqrt((data_phase_1$p_bar * (100 - data_phase_1$p_bar)))/sqrt(data_phase_1[[n]])

    data_phase_1$ucl <- data_phase_1$p_bar + (3 * data_phase_1$sigma_hat)
    data_phase_1$lcl <- data_phase_1$p_bar - (3 * data_phase_1$sigma_hat)
  }

  # Build graphic -----
  if (append == FALSE) {
    data_phase_1 %>%
      plot_ly(x = ~k,
              y = ~p,
              mode = "lines",
              source = "plot_p") %>%
      add_trace(y = ~p,
                name = 'Proporção',
                mode = 'lines+markers',
                color = I("blue")) %>%
      add_trace(y = ~p_bar,
                name = TeX("\\bar{p}"),
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
      layout(title = paste0("Gráfico P de ", names(data_phase_1[d])),
             xaxis = list(title = 'Amostra'),
             yaxis = list(title = 'Proporção'),
             hovermode = "x unified",
             showlegend = FALSE) %>%
      add_trace(data = data_phase_2,
                name = "Proporção",
                x = ~k,
                y = ~p,
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
    base_plot %>%
      add_trace(data = data_phase_1,
                y = ~p_bar,
                name = TeX("\\bar{p}"),
                type = 'scatter',
                mode = 'lines',
                color = I("green"),
                line = list(shape = 'linear', width= 3, dash = 'dot'),
                connectgaps = TRUE) %>%
      add_trace(data = data_phase_1,
                y = ~ucl,
                name = 'LSC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      add_trace(data = data_phase_1,
                y = ~lcl,
                name = 'LIC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv"))
  }

}

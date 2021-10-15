# Author: Gabriel Teotonio
# Title: Function to plot P control chart
# Date: 2021-10-14

#' Plot control graphic P
#'
#' @param n Variable for sample size
#' @param d Variable for non-expected observations
#' @param k Variable fot sample index
#' @param data Data set with the variables n, d, and k
#' @return A Plotly graphic
#' @export
plot_p <- function(n, d, k, data) {
  # Filters -----
  if (dim(data)[1] == 0)  {
    stop("data must be a non-empty dataframe.")
  }

  # if (!(n %in% colnames(data)) || !(d %in% colnames(data)) || !(k %in% colnames(data))) {
  #   stop("all columns (n, d, k) must be in the dataframe.")
  # }

  if (unique(data[[n]] < data[[d]]) %in% c("TRUE")) {
    stop("d values must be equal or less than n values.")
  }

  # Calculation -----
  data$k <- data[[k]]

  if (length(unique(data[[n]])) == 1) {
    data$p <- 100 * (data[[d]]/data[[n]])
    data$p_bar <- sum(data[[p]])/dim(data)[1]
    data$sigma_hat <- sqrt((data[[p_bar]] * (100 - data[[p_bar]]))/dim(data)[1])

    data$ucl <- data[[p_bar]] + (3 * data[[sigma_hat]])
    data$lcl <- data[[p_bar]] - (3 * data[[sigma_hat]])
  } else {
    data$p <- 100 * (data[[d]]/data[[n]])
    data$p_bar <- 100 * (sum(data[[d]])/sum(data[[n]]))
    data$sigma_hat <- sqrt((data$p_bar * (100 - data$p_bar)))/sqrt(dim(data)[1])

    data$ucl <- data$p_bar + (3 * data$sigma_hat)
    data$lcl <- data$p_bar - (3 * data$sigma_hat)
  }

  # Build graphic -----
  data %>%
    plot_ly(x = ~k,
            y = ~p,
            mode = "lines") %>%
    add_trace(y = ~p, name = 'Proporção', mode = 'lines+markers') %>%
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
              color = I("red")) %>%
    add_trace(y = ~lcl,
              name = 'LIC',
              mode = 'lines',
              color = I("red")) %>%
    layout(xaxis = list(title = 'Amostra'),
           yaxis = list(title = 'Proporção'),
           hovermode = "x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                      'hoverClosestCartesian', 'hoverCompareCartesian',
                                      'select2d','lasso2d', 'toggleSpikelines'),
           displayModeBar = TRUE,
           displaylogo = FALSE ,
           locale = 'pt-BR',
           mathjax = 'cdn')

}




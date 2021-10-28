# Author: Gabriel Teotonio
# Title: Function to plot X bar control chart
# Date: 2021-10-14

#' Plot control graphic X bar
#'
#' @param x Variable of measurements
#' @param k Variable for sample index
#' @param data Data set with the variables n, d, and k
#' @return A Plotly graphic
#' @export
plot_xbar <- function(x, k, data) {
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

  A2 <- c(1.88, 1.02, 0.73, 0.58, 0.48, 0.42, 0.37, 0.34, 0.31)
  D3 <- c(0, 0, 0, 0, 0, 0, 00.08, 0.14, 0.18, 0.22)
  D4 <- c(3.27, 2.57, 2.28, 2.11, 2, 1.92, 1.86, 1.82, 1.78)
  A3 <- c(2.66, 1.95, 1.63, 1.43, 1.29, 1.18, 1.10, 1.03, 0.98)
  B3 <- c(0, 0, 0, 0, 0.03, 0.12, 0.18, 0.24, 0.28)
  B4 <- c(3.27, 2.57, 2.27, 2.09, 1.97, 1.88, 1.82, 1.76, 1.72)

  index_factors <- nrow(data)/length(unique(data$k))
  index_sample <- length(unique(data$k))

  if (nrow(data)%%length(unique(data$k)) != 0 || nrow(data)/length(unique(data$k)) > 10) {
    x_bar_sample <- data %>%
      group_by(k) %>%
      summarise(x_bar_sample = mean(x),
                size_sample = n())

    s_sample <- data %>%
      group_by(k) %>%
      summarise(s_sample = sd(x),
                size_sample_minus = n() - 1)

    print(s_sample$s_sample)

    x_bar <- sum(x_bar_sample$size_sample * x_bar_sample$x_bar_sample)/sum(x_bar_sample$size_sample)
    s_bar <- sqrt(sum(s_sample$size_sample_minus * s_sample$s_sample^2)/(nrow(data) - length(unique(data$k))))

    x_bar_ucl <- x_bar[[1]] + (A3[index_factors-1] * s_bar[[1]])
    x_bar_lcl <- x_bar[[1]] - (A3[index_factors-1] * s_bar[[1]])
    s_bar_ucl <- B4[index_factors-1] * s_bar[[1]]
    s_bar_lcl <- B3[index_factors-1] * s_bar[[1]]

    # Build graphic -----
    plot_x_bar <- data %>%
      plot_ly(x = ~unique(k),
              y = ~x_bar_sample$x_bar_sample,
              mode = "lines",
              source = "plot_x_bar") %>%
      add_trace(y = ~x_bar_sample$x_bar_sample, name = 'Valor individual', mode = 'lines+markers') %>%
      add_trace(y = ~x_bar[[1]],
                name = TeX("\\bar{\\bar{x}}"),
                type = 'scatter',
                mode = 'lines',
                color = I("green"),
                line = list(shape = 'linear', width= 3, dash = 'dot'),
                connectgaps = TRUE) %>%
      add_trace(y = ~x_bar_ucl[[1]],
                name = 'LSC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      add_trace(y = ~x_bar_lcl[[1]],
                name = 'LIC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      layout(title = paste0("Gráfico X-barra de ", names(data[x])),
             xaxis = list(title = 'Amostra'),
             yaxis = list(title = 'Média amostral'),
             hovermode = "x unified") %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                        'hoverClosestCartesian', 'hoverCompareCartesian',
                                        'select2d','lasso2d', 'toggleSpikelines'),
             displayModeBar = TRUE,
             displaylogo = FALSE ,
             locale = 'pt-BR',
             mathjax = 'cdn')

    plot_s_bar <- data %>%
      plot_ly(x = ~unique(k),
              y = ~s_sample$s_sample,
              mode = "lines",
              source = "plot_s_bar") %>%
      add_trace(y = ~s_sample$s_sample, name = 'DP amostral', mode = 'lines+markers') %>%
      add_trace(y = ~s_bar[[1]],
                name = TeX("\\bar{S}"),
                type = 'scatter',
                mode = 'lines',
                color = I("green"),
                line = list(shape = 'linear', width= 3, dash = 'dot'),
                connectgaps = TRUE) %>%
      add_trace(y = ~s_bar_ucl[[1]],
                name = 'LSC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      add_trace(y = ~s_bar_lcl[[1]],
                name = 'LIC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      layout(title = paste0("Gráfico S de ", names(data[x])),
             xaxis = list(title = 'Amostra'),
             yaxis = list(title = 'DP amostral'),
             hovermode = "x unified") %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                        'hoverClosestCartesian', 'hoverCompareCartesian',
                                        'select2d','lasso2d', 'toggleSpikelines'),
             displayModeBar = TRUE,
             displaylogo = FALSE ,
             locale = 'pt-BR',
             mathjax = 'cdn')

    return(list("plot_x_bar" = plot_x_bar,
                "plot_rs_bar" = plot_s_bar))

  } else {
    x_bar_sample <- data %>%
      group_by(k) %>%
      summarise(x_bar_sample = mean(x)) %>%
      select(x_bar_sample)

    r_sample <- data %>%
      group_by(k) %>%
      summarise(r_sample = abs(max(x) - min(x))) %>%
      select(r_sample)

    x_bar <- sum(x_bar_sample$x_bar_sample)/length(unique(data$k))
    r_bar <- sum(r_sample$r_sample)/length(unique(data$k))

    x_bar_ucl <- x_bar[[1]] + (A2[index_factors-1] * r_bar[[1]])
    x_bar_lcl <- x_bar[[1]] - (A2[index_factors-1] * r_bar[[1]])
    r_bar_ucl <- D4[index_factors-1] * r_bar[[1]]
    r_bar_lcl <- D3[index_factors-1] * r_bar[[1]]

    # Build graphic -----
    plot_x_bar <- data %>%
      plot_ly(x = ~unique(k),
              y = ~x_bar_sample$x_bar_sample,
              mode = "lines",
              source = "plot_x_bar") %>%
      add_trace(y = ~x_bar_sample$x_bar_sample, name = 'Valor individual', mode = 'lines+markers') %>%
      add_trace(y = ~x_bar[[1]],
                name = TeX("\\bar{\\bar{x}}"),
                type = 'scatter',
                mode = 'lines',
                color = I("green"),
                line = list(shape = 'linear', width= 3, dash = 'dot'),
                connectgaps = TRUE) %>%
      add_trace(y = ~x_bar_ucl[[1]],
                name = 'LSC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      add_trace(y = ~x_bar_lcl[[1]],
                name = 'LIC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      layout(title = paste0("Gráfico X-barra de ", names(data[x])),
             xaxis = list(title = 'Amostra'),
             yaxis = list(title = 'Média amostral'),
             hovermode = "x unified") %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                        'hoverClosestCartesian', 'hoverCompareCartesian',
                                        'select2d','lasso2d', 'toggleSpikelines'),
             displayModeBar = TRUE,
             displaylogo = FALSE ,
             locale = 'pt-BR',
             mathjax = 'cdn')

    plot_r_bar <- data %>%
      plot_ly(x = ~unique(k),
              y = ~r_sample$r_sample,
              mode = "lines",
              source = "plot_r_bar") %>%
      add_trace(y = ~r_sample$r_sample, name = 'Amplitude amostral', mode = 'lines+markers') %>%
      add_trace(y = ~r_bar[[1]],
                name = TeX("\\bar{R}"),
                type = 'scatter',
                mode = 'lines',
                color = I("green"),
                line = list(shape = 'linear', width= 3, dash = 'dot'),
                connectgaps = TRUE) %>%
      add_trace(y = ~r_bar_ucl[[1]],
                name = 'LSC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      add_trace(y = ~r_bar_lcl[[1]],
                name = 'LIC',
                mode = 'lines',
                color = I("red"),
                line = list(shape = "hv")) %>%
      layout(title = paste0("Gráfico R de ", names(data[x])),
             xaxis = list(title = 'Amostra'),
             yaxis = list(title = 'Amplitude amostral'),
             hovermode = "x unified") %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                        'hoverClosestCartesian', 'hoverCompareCartesian',
                                        'select2d','lasso2d', 'toggleSpikelines'),
             displayModeBar = TRUE,
             displaylogo = FALSE ,
             locale = 'pt-BR',
             mathjax = 'cdn')

    return(list("plot_x_bar" = plot_x_bar,
                "plot_rs_bar" = plot_r_bar))
  }
}

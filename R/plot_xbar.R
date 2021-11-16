# Author: Gabriel Teotonio
# Title: Function to plot X bar control chart
# Date: 2021-10-14

#' Plot control graphic X bar
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
plot_xbar <- function(x, k, phase, append = FALSE, index_append = NULL, base_plot = NULL, data) {
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

  phase_value <- unique(data$k)[phase]
  append_value <- unique(data$k)[index_append]
  data_phase_1 <- data %>% filter(k <= phase_value)
  data_phase_2 <- data %>% filter(k >= phase_value)

  if (append == TRUE) {
    data_phase_1 <- data %>% filter(k >= phase_value, k <= append_value)
  }

  A2 <- c(1.88, 1.02, 0.73, 0.58, 0.48, 0.42, 0.37, 0.34, 0.31)
  D3 <- c(0, 0, 0, 0, 0, 0, 00.08, 0.14, 0.18, 0.22)
  D4 <- c(3.27, 2.57, 2.28, 2.11, 2, 1.92, 1.86, 1.82, 1.78)
  A3 <- c(2.66, 1.95, 1.63, 1.43, 1.29, 1.18, 1.10, 1.03, 0.98)
  B3 <- c(0, 0, 0, 0, 0.03, 0.12, 0.18, 0.24, 0.28)
  B4 <- c(3.27, 2.57, 2.27, 2.09, 1.97, 1.88, 1.82, 1.76, 1.72)

  index_factors <- nrow(data_phase_1)/length(unique(data_phase_1$k))
  index_sample <- length(unique(data_phase_1$k))

  if (nrow(data_phase_1)%%length(unique(data_phase_1$k)) != 0 || nrow(data_phase_1)/length(unique(data_phase_1$k)) > 10) {
    x_bar_sample <- data_phase_1 %>%
      group_by(k) %>%
      summarise(x_bar_sample = mean(x),
                size_sample = n())

    x_bar_sample_phase_2 <- data_phase_2 %>%
      group_by(k) %>%
      summarise(x_bar_sample = mean(x),
                size_sample = n())

    s_sample <- data_phase_1 %>%
      group_by(k) %>%
      summarise(s_sample = sd(x),
                size_sample_minus = n() - 1)

    s_sample_phase_2 <- data_phase_2 %>%
      group_by(k) %>%
      summarise(s_sample = sd(x),
                size_sample_minus = n() - 1)

    x_bar <- sum(x_bar_sample$size_sample * x_bar_sample$x_bar_sample)/sum(x_bar_sample$size_sample)
    s_bar <- sqrt(sum(s_sample$size_sample_minus * s_sample$s_sample^2)/(nrow(data_phase_1) - length(unique(data_phase_1$k))))

    x_bar_ucl <- x_bar[[1]] + (A3[index_factors-1] * s_bar[[1]])
    x_bar_lcl <- x_bar[[1]] - (A3[index_factors-1] * s_bar[[1]])
    s_bar_ucl <- B4[index_factors-1] * s_bar[[1]]
    s_bar_lcl <- B3[index_factors-1] * s_bar[[1]]

    # Build graphic -----
    if (append == FALSE) {
      plot_x_bar <- data_phase_1 %>%
        plot_ly(x = ~unique(k),
                y = ~x_bar_sample$x_bar_sample,
                mode = "lines",
                source = "plot_x_bar") %>%
        add_trace(y = ~x_bar_sample$x_bar_sample,
                  name = 'Valor individual',
                  mode = 'lines+markers',
                  color = I("blue")) %>%
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
        layout(title = paste0("Gráfico X-barra de ", names(data_phase_1[x])),
               xaxis = list(title = 'Amostra'),
               yaxis = list(title = 'Média amostral'),
               hovermode = "x unified",
               showlegend = FALSE) %>%
        add_trace(y = ~x_bar_sample_phase_2$x_bar_sample,
                  x = ~unique(data_phase_2$k),
                  name = 'Valor individual',
                  mode = 'lines+markers',
                  color = I("blue")) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                          'hoverClosestCartesian', 'hoverCompareCartesian',
                                          'select2d','lasso2d', 'toggleSpikelines'),
               displayModeBar = TRUE,
               displaylogo = FALSE ,
               locale = 'pt-BR',
               mathjax = 'cdn')

      plot_s_bar <- data_phase_1 %>%
        plot_ly(x = ~unique(k),
                y = ~s_sample$s_sample,
                mode = "lines",
                source = "plot_s_bar") %>%
        add_trace(y = ~s_sample$s_sample,
                  name = 'DP amostral',
                  mode = 'lines+markers',
                  color = I("blue")) %>%
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
        layout(title = paste0("Gráfico S de ", names(data_phase_1[x])),
               xaxis = list(title = 'Amostra'),
               yaxis = list(title = 'DP amostral'),
               hovermode = "x unified",
               showlegend = FALSE) %>%
        add_trace(y = ~s_sample_phase_2$s_sample,
                  x = ~unique(data_phase_2$k),
                  name = 'DP amostral',
                  mode = 'lines+markers',
                  color = I("blue")) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                          'hoverClosestCartesian', 'hoverCompareCartesian',
                                          'select2d','lasso2d', 'toggleSpikelines'),
               displayModeBar = TRUE,
               displaylogo = FALSE ,
               locale = 'pt-BR',
               mathjax = 'cdn')
    } else {
      plot_x_bar <- base_plot %>%
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
                  line = list(shape = "hv"))

      plot_s_bar <- base_plot %>%
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
                  line = list(shape = "hv"))
    }


    return(list("plot_x_bar" = plot_x_bar,
                "plot_s_bar" = plot_s_bar))

  } else {
    x_bar_sample <- data_phase_1 %>%
      group_by(k) %>%
      summarise(x_bar_sample = mean(x)) %>%
      select(x_bar_sample)

    x_bar_sample_phase_2 <- data_phase_2 %>%
      group_by(k) %>%
      summarise(x_bar_sample = mean(x)) %>%
      select(x_bar_sample)

    r_sample <- data_phase_1 %>%
      group_by(k) %>%
      summarise(r_sample = abs(max(x) - min(x))) %>%
      select(r_sample)

    r_sample_phase_2 <- data_phase_2 %>%
      group_by(k) %>%
      summarise(r_sample = abs(max(x) - min(x))) %>%
      select(r_sample)

    x_bar <- sum(x_bar_sample$x_bar_sample)/length(unique(data_phase_1$k))
    r_bar <- sum(r_sample$r_sample)/length(unique(data_phase_1$k))

    x_bar_ucl <- x_bar[[1]] + (A2[index_factors-1] * r_bar[[1]])
    x_bar_lcl <- x_bar[[1]] - (A2[index_factors-1] * r_bar[[1]])
    r_bar_ucl <- D4[index_factors-1] * r_bar[[1]]
    r_bar_lcl <- D3[index_factors-1] * r_bar[[1]]

    # Build graphic -----
    if (append == FALSE) {
      plot_x_bar <- data_phase_1 %>%
        plot_ly(x = ~unique(k),
                y = ~x_bar_sample$x_bar_sample,
                mode = "lines",
                source = "plot_x_bar") %>%
        add_trace(y = ~x_bar_sample$x_bar_sample,
                  name = 'Valor individual',
                  mode = 'lines+markers',
                  color = I("blue")) %>%
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
        layout(title = paste0("Gráfico X-barra de ", names(data_phase_1[x])),
               xaxis = list(title = 'Amostra'),
               yaxis = list(title = 'Média amostral'),
               hovermode = "x unified",
               showlegend = FALSE) %>%
        add_trace(y = ~x_bar_sample_phase_2$x_bar_sample,
                  x = ~unique(data_phase_2$k),
                  name = 'Valor individual',
                  mode = 'lines+markers',
                  color = I("blue")) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                          'hoverClosestCartesian', 'hoverCompareCartesian',
                                          'select2d','lasso2d', 'toggleSpikelines'),
               displayModeBar = TRUE,
               displaylogo = FALSE ,
               locale = 'pt-BR',
               mathjax = 'cdn')

      plot_r_bar <- data_phase_1 %>%
        plot_ly(x = ~unique(k),
                y = ~r_sample$r_sample,
                mode = "lines",
                source = "plot_r_bar") %>%
        add_trace(y = ~r_sample$r_sample,
                  name = 'Amplitude amostral',
                  mode = 'lines+markers',
                  color = I("blue")) %>%
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
        layout(title = paste0("Gráfico R de ", names(data_phase_1[x])),
               xaxis = list(title = 'Amostra'),
               yaxis = list(title = 'Amplitude amostral'),
               hovermode = "x unified",
               showlegend = FALSE) %>%
        add_trace(y = ~r_sample_phase_2$r_sample,
                  x = ~unique(data_phase_2$k),
                  name = 'Amplitude amostral',
                  mode = 'lines+markers',
                  color = I("blue")) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                          'hoverClosestCartesian', 'hoverCompareCartesian',
                                          'select2d','lasso2d', 'toggleSpikelines'),
               displayModeBar = TRUE,
               displaylogo = FALSE ,
               locale = 'pt-BR',
               mathjax = 'cdn')
    } else {
      plot_x_bar <- base_plot %>%
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
                  line = list(shape = "hv"))

      plot_r_bar <- base_plot %>%
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
                  line = list(shape = "hv"))
    }


    return(list("plot_x_bar" = plot_x_bar,
                "plot_r_bar" = plot_r_bar))
  }
}

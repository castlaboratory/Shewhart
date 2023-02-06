
color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' Get Shewhard phases using 7 points rule
#' @param data A tibble white de
#' @param index_col Column with sequence of dates or intergers
#' @param values_col Column with values to analyze
#' @param phase_changes Vector with dates por positions for changes of phase
#' @param start_base Number of points in start of the series to use as base
#' @param model One of log (default) and Gompetz.
#' @export
shewhart <- function(data, values_col, index_col,
                     start_base = 10, type = "ggplot",
                     rule = "7 points",
                     model = "log",
                     locale = "pt_BR",
                     phase_changes = NULL, ...){
  stopifnot(type %in% c("ggplot", "plotly"))

  locale_sys <- Sys.getlocale("LC_TIME")
  Sys.setenv("LC_TIME" = if_else(str_detect(locale, "en"), "en_US.UTF-8", "pt_BR.UTF-8"))


  if(!all(c("phase", "phase_string", "CL", "UL_EXP", "UL_EXP", "CONL_1", "fit", "fitted") %in% names(data))) {
    if(is.null(phase_changes)) {
      phase_changes <- shewhart_7points(data = data,
                                        index_col =  {{index_col}},
                                        values_col = {{values_col}},
                                        start_base = {{start_base}},
                                        model = model)
      }

    plot_data <- shewhart_model(data = data,
                                index_col =  {{index_col}},
                                values_col = {{values_col}},
                                start_base = {{start_base}},
                                phase_changes = phase_changes,
                                model = model,
                                locale = locale)

  } else {
    phase_changes <- data %>% filter(change == TRUE) %>% pull({{index_col}})
    plot_data <- data
  }

  pal_fases <- color_hue(length(phase_changes) + 1)

  y_max = data %>% pull({{values_col}}) %>% max(na.rm = TRUE)

  p <- NULL

  if(type == "ggplot"){
    p <- plot_data  %>%
        ggplot() +
        theme_light() +
        geom_line(aes(x = {{index_col}}, y = LL_EXP, color = phase_string),
                  linewidth = 1.25, alpha = .25) +
        geom_line(aes(x = {{index_col}}, y = UL_EXP, color = phase_string),
                  linewidth = 1.25, alpha = .25) +
        geom_ribbon(aes(x = {{index_col}}, ymin = LL_EXP, ymax = UL_EXP, fill = phase_string),
                    alpha = .25) +
        geom_line(aes(x = {{index_col}}, y = CL, color = phase_string),
                linewidth = 1.25) +
        geom_point(aes({{index_col}}, {{values_col}})) +
        geom_line(aes({{index_col}}, {{values_col}}), alpha = .25) +
        theme(legend.direction = "horizontal",
              legend.position = "bottom") +
        labs(color = if_else(str_detect(locale, "en"), "Phase", "Fase"),
             fill = if_else(str_detect(locale, "en"), "Phase", "Fase")) +
        coord_cartesian(ylim = c(0, 1.1*y_max)) +
         labs(y = enquo(values_col), x = enquo(index_col)) +
        scale_x_date(date_labels = "%b/%y")
  }



  if(type == "plotly") {
    p <- plot_data %>%
      mutate(day = str_to_title(wday({{index_col}},
                                     label = TRUE,
                                     abbr = FALSE,
                                     locale =  if_else(str_detect(locale, "en"), "en_US.UTF-8", "pt_BR.UTF-8"))))%>%
      plot_ly(colors = pal_fases, source = "A")  %>%
      add_ribbons(
        hoverinfo = 'none'
        ,x = enquo(index_col)
        ,ymin = ~LL_EXP
        ,ymax = ~UL_EXP
        ,fillcolor= ~phase_string
        ,color = ~phase_string
        ,opacity = 0.25    ) %>%
      add_trace(
        x = enquo(index_col),
        y = ~CL,
        type = "scatter",
        mode = "markers+lines",
        color = ~phase_string,
        showlegend = FALSE,
        text = ~day,
        hovertemplate = if_else(str_detect(locale, "en"),
                                paste("<extra><b>Value</b>: %{y:d}<br><b>Date</b>: %{x}<br><b>Day</b>: %{text}</extra>"),
                                paste("<extra><b>Valor</b>: %{y:d}<br><b>Data</b>: %{x}<br><b>Dia</b>: %{text}</extra>"))
      ) %>%
      add_trace(
        x = enquo(index_col),
        y = enquo(values_col),
        type = "scatter",
        mode = "markers+lines",
        showlegend = FALSE,
        line = list(color = 'rgb(0.25, 0.25, 0.25, .75)'),
        marker = list(color = 'rgb(0.25, 0.25, 0.25, .75)',
                      line = list(
                        color = 'rgb(0.25, 0.25, 0.25, .75)',
                        width = 0)
        ),
        hoverinfo = 'text',
        text = ~day,
        hovertemplate = if_else(str_detect(locale, "en"),
                                "<extra><b>Value</b>: %{y:d}<br><b>Date</b>: %{x}<br><b>Day</b>: %{text}</extra>",
                                "<extra><b>Valor</b>: %{y:d}<br><b>Data</b>: %{x}<br><b>Dia</b>: %{text}</extra>",
        )
      )  %>%
      layout(
        margin = list(
          l = 5,
          r = 5,
          b = 5,
          t = 5,
          pad = 5),
        legend = list(orientation = "h",
                      x = 0.05,
                      y = 1.05,
                      bgcolor = 'rgba(0,0,0,0)'),
        yaxis = list(title = quo_name(enquo(values_col)),
                     range = c(0, 1.2*y_max)
                     ),
        xaxis = list(title = quo_name(enquo(index_col)),
                     type = 'date',
                     tickformat = "%b/%y",
                     dtick = 86400000.0*30
        )
      ) %>% config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                              'hoverClosestCartesian', 'hoverCompareCartesian',
                                              'select2d','lasso2d', 'toggleSpikelines'),
                   displayModeBar = TRUE,
                   displaylogo = FALSE ,
                   toImageButtonOptions = list(
                     format = "png",
                     width = 1200,
                     height = 600,
                     scale = 1
                   ))
    if(str_detect(locale, "pt"))
      p <- p %>% config(locale = "pt-br")
  }
  Sys.setenv("LC_TIME" = locale_sys)
  return(p)
}

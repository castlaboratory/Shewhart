

#' Get Shewhard phases using 7 points rule
#' @param data A tibble white de
#' @param index_col Column with sequence of dates or intergers
#' @param values_col Column with values to analyze
#' @param phase_changes Vector with dates por positions for changes of phase
#' @param start_base Number of points in start of the series to use as base
#' @param model One of log (default) and Gompetz.
#' @export
shewhart <- function(data, values_col, index_col, start_base = 10, type = "ggplot", rule = "7 points", method = "log", locale = "pt_BR.UTF-8", ...){
  phase_changes <- shewhart_7points(data = data,
                                    index_col =  {{index_col}},
                                    values_col = {{values_col}},
                                    start_base = {{start_base}})
  plot_data <- shewhart_model(data = data,
                               index_col =  {{index_col}},
                               values_col = {{values_col}},
                               start_base = {{start_base}},
                               phase_changes = phase_changes)
  pal_fases <- c(
    rev(brewer.set2(8)),
    rev(brewer.set3(8)),
    rev(brewer.set1(8))
  )

  p <- plot_data %>%
    mutate(day = str_to_title(wday({{index_col}},
                                   label = TRUE,
                                   abbr = FALSE,
                                   # week_start = getOption("lubridate.week.start", 7),
                                   locale = "pt_BR.UTF-8")))%>%
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
      hovertemplate = "<extra><b>Valor</b>: %{y:d}<br><b>Data</b>: %{x}<br><b>Dia</b>: %{text}</extra>"
    ) %>%
    add_trace(
      x = enquo(index_col),
      y = enquo(values_col),
      type = "scatter",
      mode = "markers+lines",
      showlegend = FALSE,
      line = list(color = 'rgb(0, 0, 0)'),
      marker = list(
        color = ~if_else(phase != 0, rgb(1, 0, 0, 1),
                         if_else(day == "Domingo", rgb(0, 1, 0, 1),
                                 if_else(day == "Sábado", rgb(0, 0, 1, 1),
                                         rgb(0, 0, 0, 1)))),
        line = list(
          color = 'rgb(0, 0, 0)',
          width = 1)
      ),
      hoverinfo = 'text',
      text = ~day,
      hovertemplate = "<extra><b>Valor</b>: %{y:d}<br><b>Data</b>: %{x}<br><b>Dia</b>: %{text}</extra>"
    )  %>%
    layout(#hovermode="y unified",
      #hoverdistance = 100,
      title = list(
        text = "teste",
        x = 0,
        y = 1,
        xanchor = 'left',
        yanchor = 'top',
        font = list(
          family = 'Montserrat, sans-serif',
          size = 18,
          color = '#000000')
      ),
      margin = list(
        l = 20,
        r = 20,
        b = 20,
        t = 30,
        pad = 20),
      # images = list(
      #   list(source = "castv2i.svg",
      #      xref = "paper",
      #      yref = "paper",
      #      x= .8,
      #      y= 1,
      #      sizex = 0.3,
      #      sizey = 0.3,
      #      opacity = 0.25
      # )),
      # images = list(
      #   list(source = "logo.final.CC_text_2b.svg",
      #        xref = "paper",
      #        yref = "paper",
      #        x= 1,
      #        y= 1,
      #        xanchor = "right",
      #        yanchor = "top",
      #        sizex = .2,
      #        sizey = .2,
      #        opacity = 0.25
      #   )),
      legend = list(orientation = "h",
                    x = 0.05,
                    y = .95,
                    bgcolor = 'rgba(0,0,0,0)'),
      yaxis = list(title = "Óbitos diários"#,
                   #range = c(-1, 1.25*y_max)#,
                   #showspikes = TRUE,
                   #spikethickness = 1,
                   #spikecolor = "rgb(0,0,0,.5)",
                   #spikesnap = "cursor",
                   #hoverdistance = -1,
                   #spikemode = "across+marker"
      ),
      xaxis = list(title = "Data",
                   type = 'date',
                   tickformat = "%d %b",
                   dtick = 86400000.0*15
                   # showspikes = TRUE,
                   # spikethickness = 1,
                   # spikecolor = "rgb(0,0,0,.5)",
                   # spikesnap = "cursor",
                   # hoverdistance = -1,
                   # spikemode = "across+marker"
      )
    ) %>% config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",  'zoom2d', 'pan2d', 'resetScale2d',
                                            'hoverClosestCartesian', 'hoverCompareCartesian',
                                            'select2d','lasso2d', 'toggleSpikelines'),
                 displayModeBar = TRUE,
                 displaylogo = FALSE ,
                 locale = 'pt-BR',
                 toImageButtonOptions = list(
                   format = "png",
                   #filename = paste("shewhart:", values$file_name),
                   width = 1200,
                   height = 600,
                   scale = 1
                 ))

  return(p)
}

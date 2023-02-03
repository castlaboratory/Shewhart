library(tidyverse)
cvdbr_ms <- read_rds("../covid19controlprocess/app/cvd19cp/data/cvdbr_ms.rds")
#lista_estados <- read_rds("data/lista_estados.rds")
lista_estados_codes <- read_rds("../covid19controlprocess/app/cvd19cp/data/lista_estados_codes.rds")
lista_estados <- setNames(lista_estados_codes$code, lista_estados_codes$nome)
lista_municipios <-  read_rds("../covid19controlprocess/app/cvd19cp/data/lista_municipios.rds")

write_rds(cvdbr_ms, "inst/extdata/cvd19br_ms.rds", compress = 'bz')
write_rds(lista_estados, "inst/extdata/list_br_states.rds")
write_rds(lista_municipios, "inst/extdata/list_br_cities.rds")

cvdbr_ms <- read_rds("inst/extdata/cvd19br_ms.rds")
base <- cvdbr_ms %>% filter(coduf == '26', municipio == 'Recife')

write_rds(recife, "inst/extdata/recife.rds")




library(tidyverse)
library(lubridate)
library(tidymodels)
base %>% filter(year(data) == 2022) %>%
  mutate(obitosNovos = obitosAcumulado - lag(obitosAcumulado, 1L, order_by = data, default = 0L)) %>%
  select(data, obitosAcumulado) %>%
  left_join(tibble(data = c(min(.$data + 10), recife$`Término`[-1]), change = TRUE), by = "data") %>%
  replace_na(list(change = FALSE)) %>%
  arrange(data) %>%
  mutate(phase = cumsum(change),
         log_var = log(obitosAcumulado + 1)) %>%
  group_by(phase) %>%
  mutate(N = row_number()) %>%
  ungroup() %>%
  nest(data = -phase) %>%
  mutate(
    fit = map(data , ~ lm(log_var ~ N, data = .x)),
    tidied = map(fit, tidy)
  ) %>%
  mutate(fit = if_else(phase == max(phase), lag(fit), fit),
         tidied = if_else(phase == max(phase), lag(tidied), tidied),
         fitted = map2(data, fit, ~ predict(.y, newdata = .x, se.fit = FALSE))) %>%
  unnest(c(data, fitted)) %>%
  mutate(residuals = log_var - fitted) %>%
  group_by(phase) %>%
  mutate(CONL_1 = 2.66*mean(abs(residuals - lag(residuals)), na.rm = TRUE),
         UCL = pmax(0, fitted + CONL_1, na.rm = TRUE),
         LCL = pmax(0, fitted - CONL_1, na.rm = TRUE))   %>%
  mutate(
    CL = pmax(exp(fitted) - 1, 0, na.rm = TRUE),
    UL_EXP = pmax(exp(UCL) - 1, na.rm = TRUE),
    LL_EXP = pmax(exp(LCL) - 1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(phase_string = if_else(phase == 0, "Base",
                                if_else(phase == max(phase), "Monitoring",
                                        paste0("Phase ", phase))))





  unnest(augmented)

  group_split(phase) %>%
  map_dfr( ~ .x %>% mutate(N = row_number()) %>% transmute(model = list = model = lm(var ~ N, data = .)))


teste <- base %>% filter(year(data) == 2022) %>%
    mutate(obitosNovos = obitosAcumulado - lag(obitosAcumulado, 1L, order_by = data, default = 0L)) %>%
    select(data, obitosNovos) %>%
  mutate(obitosNovos = if_else(obitosNovos >= 0, obitosNovos, 0L))


Shewhart::shewhard_model(data = teste, index_col = data, values_col = obitosNovos, phase_changes = recife$`Término`[-1])



shewhart_model(data = teste, index_col = data, values_col = obitosNovos,
               phase_changes = c("2022-01-23", "2022-02-17"))  %>%
  select(data, obitosNovos, fitted, CL, phase) %>%
  filter(phase == max(phase)) %>%
  mutate(
    upper = if_else(obitosNovos > CL, 1, 0),
    lower = if_else(obitosNovos <  CL, 1, 0),
    upper_cs = rolling_sum(upper),
    lower_cs = rolling_sum(lower)) %>%
  filter(upper_cs == 7 | lower_cs == 7) %>%
  slice_min(n = 1, order_by = data) %>%
  pull(data)

pal_fases <- c(
  rev(brewer.set2(8)),
  rev(brewer.set3(8)),
  rev(brewer.set1(8))
)

y_max <- max(values$plot_base[,'obitosNovos'], na.rm = TRUE)

#str(values$plot_base)
teste$obitosNovos[1] = 0

p <-  shewhart(data = teste, index_col = data, values_col = obitosNovos) %>%
  mutate(day = str_to_title(wday(data,
                                 label = TRUE,
                                 abbr = FALSE,
                                 # week_start = getOption("lubridate.week.start", 7),
                                 locale = "pt_BR.UTF-8"))) %>%
  plot_ly(colors = pal_fases, source = "A")  %>%
  add_ribbons(
    hoverinfo = 'none'
    ,x = ~data
    ,ymin = ~LL_EXP
    ,ymax = ~UL_EXP
    ,fillcolor= ~phase_string
    ,color = ~phase_string
    ,opacity = 0.25
  )   %>%
  add_trace(
    x = ~data,
    y = ~CL,
    type = "scatter",
    mode = "markers+lines",
    color = ~phase_string,
    showlegend = FALSE,
    text = ~day,
    hovertemplate = "<extra><b>Valor</b>: %{y:d}<br><b>Data</b>: %{x}<br><b>Dia</b>: %{text}</extra>"
  ) %>%
  add_trace(
    x = ~data,
    y = ~obitosNovos,
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
    legend = list(orientation = "v",
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

values$has_plotly <- TRUE
values$p







base <- read_rds("inst/extdata/cvd19br_ms.rds")


pe <- base %>% filter(year(data) == 2020, coduf == "26", is.na(codmun)) %>%
  mutate(obitosNovos = obitosAcumulado - lag(obitosAcumulado, 1L, order_by = data, default = 0L)) %>%
  select(data, obitosAcumulado) %>%
  mutate(obitosNovos = if_else(obitosNovos >= 0, obitosNovos, 0L))




shewhart(data = recife,
         index_col = data,
         values_col = obitosNovos,
         model = "log",
         locale = "en_US")


phase_dates <- shewhart_7points(data = recife,
                                index_col = data,
                                values_col = obitosNovos,
                                model =  "loglog")

shewhart_model(data = recife,
               index_col = data,
               values_col = obitosNovos,
               phase_changes = phase_dates,
               model =  "loglog")


shewhart(data = pe,
         index_col = data,
         values_col = obitosNovos,
         model = "log",
         locale = "en_US")

library(tidyverse)
cvdbr_ms <- read_rds("../covid19controlprocess/app/cvd19cp/data/cvdbr_ms.rds")
#lista_estados <- read_rds("data/lista_estados.rds")
lista_estados_codes <- read_rds("../covid19controlprocess/app/cvd19cp/data/lista_estados_codes.rds")
lista_estados <- setNames(lista_estados_codes$code, lista_estados_codes$nome)
lista_municipios <-  read_rds("../covid19controlprocess/app/cvd19cp/data/lista_municipios.rds")

write_rds(cvdbr_ms, "inst/extdata/cvd19br_ms.rds", compress = 'bz')
write_rds(lista_estados, "inst/extdata/list_br_states.rds")
write_rds(lista_municipios, "inst/extdata/list_br_cities.rds")

base <- cvdbr_ms %>% filter(coduf == '26', municipio == 'Recife')

write_rds(recife, "inst/extdata/recife.rds")

library(lubridate)
base %>% filter(year(data) == 2022) %>%
  mutate(obitosNovos = obitosAcumulado - lag(obitosAcumulado, 1L, order_by = data, default = 0L)) %>%
  select(data, obitosAcumulado) %>%
  left_join(tibble(data = c(min(.$data + 10), recife$`Término`[-1]), change = TRUE), by = "data") %>%
  replace_na(list(change = FALSE)) %>%
  arrange(data) %>%
  mutate(phase = cumsum(change) + 1,
         log_var = log(obitosAcumulado + 1)) %>%
  group_split(phase) %>%
  map_dfr( ~ .x %>% mutate(N = row_number()) %>% transmute(model = list = model = lm(var ~ N, data = .)))


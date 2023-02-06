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

base <- read_rds(file = "inst/extdata/recife_2002_covid19.rds")


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

base <- read_rds("inst/extdata/cvd19br_ms.rds")  %>% filter(coduf == '26', municipio == 'Recife')

teste <- base %>% filter(year(data) == 2022) %>%
    mutate(obitosNovos = obitosAcumulado - lag(obitosAcumulado, 1L, order_by = data, default = 0L)) %>%
    select(data, obitosNovos) %>%
  mutate(obitosNovos = if_else(obitosNovos >= 0, obitosNovos, 0L))

#teste$obitosNovos[1] = 0

write_rds(teste, "inst/extdata/recife_2002_covid19.rds")

library(epifitter)
teste

Shewhart::shewhart_model(data = gptz, index_col = data, values_col = obitosNovos, model = "gompertz") %>% View

shewhart(data = gptz, values_col = y, index_col = data)

Gompertz <- function(x, a, b, g){
  result <- a*exp(-exp(b - g*x))
  return(result)
}

teste <- readRDS()

gptz <- teste %>%
  mutate(N = row_number(),
         y = obitosNovos,
         ay = cumsum(obitosNovos) + 1)

ggplot(teste) + geom_point(aes(x = data, y = obitosNovos))

Gomp1 <-  nls(ay ~ Gompertz(N, a, b, g),
              data = gptz)


fit = nls(ay ~ SSgompertz(N, Asym, b2, b3), data = gptz %>%
            mutate(N = row_number(),
                   y = obitosNovos,
                   ay = cumsum(obitosNovos) + 1) %>% filter(row_number() <= 10))

fitlm <- lm(y ~ N, gptz %>%
  mutate(N = row_number(),
         y = obitosNovos,
         ay = cumsum(obitosNovos) + 1) %>% filter(row_number() <= 10))

length(fitlm$fitted.values)

length(fit$m$fitted())
shewhart_fit(data = gptz %>% filter(row_number() <= 10), index_col = N, values_col = y)





ymax <- max(dbs$obitosNovos, na.rm = TRUE)

opt_locale <- Sys.getlocale()



Sys.setenv(LC_COLLATE = "pt", LC_TIME="pt", LANGUAGE = "pt")

Sys.getenv("LANGUAGE")
Sys.setenv("LANGUAGE"="pt_BR")

Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

Sys.getenv("LC_TIME")
Sys.setlocale("LC_TIME", "")
locale_sys <- Sys.getlocale("LC_TIME")

ggplot(data = dbs) +
  geom_point(aes(data, obitosNovos)) +
  theme_light() +
  geom_line(aes(x = data, y = CL, color = phase_string),
            linewidth = 1.25) +
  geom_line(aes(x = data, y = LL_EXP, color = phase_string),
            linewidth = 1.25, alpha = .25) +
  geom_line(aes(x = data, y = UL_EXP, color = phase_string),
            linewidth = 1.25, alpha = .25) +
  geom_ribbon(aes(x = data, ymin = LL_EXP, ymax = UL_EXP, fill = phase_string),
              alpha = .25) +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(color = if_else(str_detect(locale, "en"), "Phase", "Fase"),
       fill = if_else(str_detect(locale, "pt"), "Phase", "Fase")) +
  coord_cartesian(ylim = c(0, 1.1*ymax))


shewhart(data = base, values_col = obitosNovos, index_col = data, model = "loglog", phase_changes = datas)



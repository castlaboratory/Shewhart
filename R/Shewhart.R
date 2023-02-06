# Shewhart Functions
# André Leite
# leite@castlab.org
# 2023/02/02

rolling_sum <- tibbletime::rollify(sum, window = 7, na_value = 0)


# Gompertz function
Gompertz <- function(x, y0, ymax, k, lag){
  result <- y0 + (ymax -y0)*exp(-exp(k*(lag-x)/(ymax-y0) + 1) )
  return(result)
}

loglog <- function(x, alpha = 1){
  return(log(log(x/alpha + 1) + 1))
}

iloglog <- function(x, alpha = 1){
  return(alpha*(exp(exp(x) - 1) - 1))
}

#' Get Shewhard phases using 7 points rule
#' @param data A tibble white de
#' @param index_col Column with sequence of dates or intergers
#' @param values_col Column with values to analyze
#' @param start_base Number of points in start of the series to use as base
#' @param model One of log (default) and Gompetz.
#' @export
shewhart_7points <- function(data, index_col, values_col, start_base = 10, model = "log", ...){
  stopifnot(nrow(data) > start_base)
  subdata <- data %>%
    select({{index_col}}, {{values_col}})
  #cat("\n Subdata: ", nrow(subdata)) # DEBUG:
  running_model <- shewhart_model(data = subdata,
                                  index_col = {{index_col}},
                                  values_col = {{values_col}},
                                  start_base,
                                  model = model)

  # running_model %>% head() %>% print() # DEBUG:

  found_phase_dates <- running_model%>% filter(change) %>% pull({{index_col}})


  #for (i in 1:(ceiling(nrow(subdata)/7))) {
  for (i in 1:((ceiling(nrow(subdata)/7)))) {
    #cat("\n Iteração: ", i)

    autodate <- running_model %>%
      select({{index_col}}, {{values_col}}, fitted, CL, phase) %>%
      filter(phase == max(phase)) %>%
      mutate(
        upper = if_else({{values_col}} > CL, 1, 0),
        lower = if_else({{values_col}} <  CL, 1, 0),
        upper_cs = rolling_sum(upper),
        lower_cs = rolling_sum(lower)) %>%
      filter(upper_cs == 7 | lower_cs == 7) %>%
      slice_min(n = 1, order_by = {{index_col}}) %>%
      pull({{index_col}})

    if(length(autodate) > 0) {
      found_phase_dates <- unique(c(found_phase_dates, autodate + 1))
    } else
      break

    running_model <- shewhart_model(data = subdata,
                                    index_col = {{index_col}},
                                    values_col = {{values_col}},
                                    phase_changes = found_phase_dates,
                                    start_base,
                                    model = model)


  }
  return(found_phase_dates)
 }

#' Get Shewhard phases using 7 points rule
#' @import tidyverse
#' @param data A tibble white de
#' @param index_col Column with sequence of dates or intergers
#' @param values_col Column with values to analyze
#' @param model One of log (default) and Gompetz.
#' @export
shewhart_fit <- function(data, index_col, values_col, model = "log", ...){
  if (model == "log") {
    fit <- data %>% mutate(log_var := log({{values_col}} + 1)) %>%
      lm(log_var ~ N, data = .)
  }
  if (model == "loglog") {
    fit <- data %>% mutate(llvar := loglog({{values_col}}, 1)) %>%
      lm(llvar ~ N, data = .)
  }
  if (model == "gompertz"){
    fit <- NA
    tryCatch({
      fit <- data %>% mutate(avar := cumsum({{values_col}}) + 1,
                             var := {{values_col}}) %>%
        nls(avar ~ SSgompertz(N, Asym, b2, b3),
            data = . ,
            start = list(Asym=10000, b2=2, b3=.02),
            control = nls.control(maxiter = 500))
    }, error=function(e) NA_real_)
    stopifnot(!is.na(fit))
  }
  return(fit)
}


#' Get Shewhard phases using 7 points rule
#' @import tidyverse
#' @import tidymodels
#' @param data A tibble white de
#' @param index_col Column with sequence of dates or intergers
#' @param values_col Column with values to analyze
#' @param phase_changes Vector with dates por positions for changes of phase
#' @param start_base Number of points in start of the series to use as base
#' @param model One of log (default) and Gompetz.
#' @export
shewhart_model <- function(data, values_col, index_col,
                           start_base = 10,
                           model = "log",
                           phase_changes = character(),
                           locale = "pt_BR", ...){
stopifnot(model %in% c("log", "loglog", "gompertz"))
   data %>%
    select({{index_col}}, {{values_col}}) %>%
     left_join(tibble({{index_col}} := unique(c(min(pull(data, {{index_col}})) + start_base, phase_changes)), change = TRUE),
               by = quo_name(enquo(index_col)))  %>%
     replace_na(list(change = FALSE)) %>%
     arrange({{index_col}}) %>%
     mutate(phase = cumsum(change),
            model = model,
            flag = if_else(phase == max(phase), phase - 1L, phase)) %>%
     group_by(flag) %>%
     mutate(N = row_number()) %>%
     ungroup() %>%
     #select(-flag) %>%
     nest(data = -phase) %>%
     mutate(
       fit = map(data , ~ shewhart_fit(data = .x, index_col = {{index_col}}, values_col = {{values_col}}, model = model)), #lm(log_var ~ N, data = .x)),
       tidied = map(fit, tidy)
     ) %>%
     mutate(fit = if_else(phase == max(phase), lag(fit), fit),
            tidied = if_else(phase == max(phase), lag(tidied), tidied),
            fitted = map2(data, fit, ~ predict(object = .y,
                                               newdata = .x,
                                               se.fit = FALSE))) %>%
     unnest(c(data, fitted)) %>%
     # unnest(data) %>%
     # mutate(fitted = if_else(phase == max(phase),
     #                         predict(fit, newdata = tibble({{index_col}} := {{index_col}},
     #                                                      {{values_col}} := {{values_col}}), se.fit = FALSE),
     #                         predict(fit, newdata = tibble({{index_col}} := {{index_col}},
     #                                                       {{values_col}} := {{values_col}}, se.fit = FALSE))
     #        )
     # ) %>%
     mutate(residuals = if_else(model == "log",
                                log({{values_col}} + 1) - fitted,
                                if_else(model == "loglog",
                                        #log(log({{values_col}} + 1) + 1) - fitted,
                                        loglog({{values_col}}, 1) - fitted,
                                        {{values_col}} - (fitted - lag(fitted, default = 1))))) %>%
     group_by(flag) %>%
     mutate(CONL_1 = 2.66*mean(abs(residuals - lag(residuals)), na.rm = TRUE),
            UCL = pmax(0, fitted + CONL_1, na.rm = TRUE),
            LCL = pmax(0, fitted - CONL_1, na.rm = TRUE))   %>%
     mutate(
       CL = case_when(
         model == "log" ~ pmax(exp(fitted) - 1, 0, na.rm = TRUE),
         model == "loglog" ~ pmax(iloglog(fitted, 1), 0, na.rm = TRUE),
         model == "gompertz" ~ pmax(exp(fitted), 0, na.rm = TRUE)
       ),
       UL_EXP = case_when(
         model == "log" ~ pmax(exp(UCL) - 1, na.rm = TRUE),
         model == "loglog" ~ pmax(iloglog(UCL, 1), na.rm = TRUE),
         model == "gompertz" ~ pmax(exp(UCL) - 1, na.rm = TRUE),
       ),
       LL_EXP = case_when(
         model == "log" ~ pmax(exp(LCL) - 1, na.rm = TRUE),
         model == "loglog" ~ pmax(iloglog(LCL, 1), na.rm = TRUE),
         model == "gompertz" ~ pmax(exp(LCL), na.rm = TRUE),
       )
     ) %>%
     ungroup() %>%
     mutate(phase_string = case_when(
       (str_detect(locale, "en") & (phase == 0)) ~ "Base",
       (str_detect(locale, "en") & (phase == max(phase, na.rm = TRUE))) ~ "Monitoring",
       (str_detect(locale, "en") & (phase > 0)&(phase != max(phase))) ~ paste0("Phase ", phase),
       (str_detect(locale, "pt") & (phase == 0)) ~ "Base",
       (str_detect(locale, "pt") & (phase  == max(phase, na.rm = TRUE))) ~ "Monitorando",
       (str_detect(locale, "pt") & (phase > 0)&(phase != max(phase))) ~ paste0("Fase ", phase)
     )
            # if_else(phase == 0, "Base",
            #                        if_else(phase == max(phase), "Monitoring",
            #                                paste0("Phase ", phase)))
            )
}

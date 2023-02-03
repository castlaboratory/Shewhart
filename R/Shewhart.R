# Shewhart Functions
# André Leite
# leite@castlab.org
# 2023/02/02

rolling_sum <- tibbletime::rollify(sum, window = 7, na_value = 0)


#' Get Shewhard phases using 7 points rule
#' @param data A tibble white de
#' @param index_col Column with sequence of dates or intergers
#' @param values_col Column with values to analyze
#' @param start_base Number of points in start of the series to use as base
#' @param model One of log (default) and Gompetz.
#' @export
shewhart_7points <- function(data, index_col, values_col, start_base = 10, model = c("log", "competz"), ...){
  stopifnot(nrow(data) > start_base)
  subdata <- data %>%
    select({{index_col}}, {{values_col}})
  #cat("\n Subdata: ", nrow(subdata)) # DEBUG:
  running_model <- shewhart_model(data = subdata,
                                  index_col = {{index_col}},
                                  values_col = {{values_col}},
                                  start_base)

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
                                    start_base)


  }
  return(found_phase_dates)
 }




#' Get Shewhard phases using 7 points rule
#' @param data A tibble white de
#' @param index_col Column with sequence of dates or intergers
#' @param values_col Column with values to analyze
#' @param phase_changes Vector with dates por positions for changes of phase
#' @param start_base Number of points in start of the series to use as base
#' @param model One of log (default) and Gompetz.
#' @export
shewhart_model <- function(data, values_col, index_col, start_base = 10, model = c("log", "competz"), phase_changes = character(), ...){

   data %>%
    select({{index_col}}, {{values_col}}) %>%
     left_join(tibble({{index_col}} := unique(c(min(pull(data, {{index_col}})) + start_base, phase_changes)), change = TRUE),
               by = quo_name(enquo(index_col)))  %>%
     replace_na(list(change = FALSE)) %>%
     arrange({{index_col}}) %>%
     mutate(phase = cumsum(change),
            log_var := log({{values_col}} + 1)) %>%
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
}

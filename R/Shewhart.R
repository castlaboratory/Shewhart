# Shewhart Functions
# André Leite
# leite@castlab.org
# 2023/02/02


# Locate phases change in 7 points rule

#' Get Shewhard phases using 7 points rule
#'
#' @param data A tibble white de
shewhard_model <- function(data, ...){
  primeiro_dia <- min(base$data, na.rm = TRUE)
  ultimo_dia <- max(base$data, na.rm = TRUE)

  #cat("datas: ", values$datas_escolhidas)

  if (length(datas_escolhidas) == 0) {
    data_df <- tibble(data = c(primeiro_dia, base$data[11]),
                      fase_indicador = c(0L, 1L),
                      label = c(paste("Amostra de base (até ", base$data[11],")", sep = ""), "Monitoramento"),
                      fase  = c(0L, 1L))
  } else {
    data_df <- tibble(data = c(primeiro_dia, base$data[11], datas_escolhidas),
                      fase_indicador = c(0L, 1L, rep(1L, length(datas_escolhidas))),
                      label = c(paste("Amostra de base (até ", base$data[11],")", sep = ""), #"Amostra de base",
                                paste("Fase ",
                                      1:(length(datas_escolhidas)),
                                      " (até ",
                                      datas_escolhidas,
                                      ")", sep = ""),
                                "Monitoramento"),
                      fase = cumsum(fase_indicador))
  }
  #cat('\n2:')
  model_base <-  base %>%
    left_join(data_df %>% select(-label), by = 'data') %>%
    mutate(fase_indicador = replace_na(fase_indicador, 0)) %>%
    mutate(fase = cumsum(fase_indicador)) %>%
    left_join(data_df %>% select(-data, -fase_indicador), by = c("fase"))

  ultima_fase <- max(model_base$fase, na.rm = TRUE)
  db <- model_base %>% mutate(fase_model = if_else(fase == ultima_fase,  fase - 1L, fase)) %>%
    group_split(fase_model) %>%
    map_dfr(
      function(x){
        model_1 <- lm(obitosNovos_log ~ N, data = x %>% filter(fase != ultima_fase))
        fitted <-predict(object = model_1, newdata = x, se.fit = FALSE)
        return(x %>% mutate(fitted = fitted))
      }) %>%
    mutate(residuals = obitosNovos_log - fitted) %>%
    group_by(fase_model) %>%
    mutate(CONL_1 = 2.66*mean(abs(residuals - lag(residuals)), na.rm = TRUE),
           UCL = pmax(0, fitted + CONL_1, na.rm = TRUE),
           LCL = pmax(0, fitted - CONL_1, na.rm = TRUE))   %>%
    mutate(
      CL = pmax(exp(fitted) - 1, 0, na.rm = TRUE),
      UL_EXP = pmax(exp(UCL) - 1, na.rm = TRUE),
      LL_EXP = pmax(exp(LCL) - 1, na.rm = TRUE)
    ) %>% ungroup() %>%
    mutate(fase_string = paste0("Fase ", fase),
           fase_model_string = paste0("Fase ", fase_model))
 return(db)
}

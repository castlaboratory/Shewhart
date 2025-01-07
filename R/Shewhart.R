# Shewhart Functions
# Author: André Leite
# Contact: leite@castlab.org
# Date: 2023/02/02

# Packages used directly in the functions below.
# You may adjust or remove the imports according to your preferences.
#' @importFrom rlang .data
#' @importFrom stats lm predict na.omit
#' @importFrom utils head
NULL


# rolling_sum ------------------------------------------------------------------

#' Rolling sum function with a 7-point window
#'
#' This function uses [slider::slide_dbl()] from the **slider** package to create
#' a rolling sum with a window size of 7. It treats `NA` values as zero via
#' [tidyr::replace_na()].
#'
#' @param x A numeric vector.
#'
#' @details
#' The function slides over the input \code{x} with a window of size 7,
#' summing the available values. At the beginning of the vector (where fewer than
#' 7 elements exist), it sums however many elements are present. This avoids
#' discarding data at the edges.
#'
#' @return A numeric vector of the same length as \code{x}, where each element
#'   is the sum of the last 7 (or fewer) elements, including the current one.
#'
#' @examples
#' \dontrun{
#'  library(slider)
#'  x <- c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10)
#'  rolling_sum(x)
#' }
#'
#' @export
rolling_sum <- function(x) {
  slider::slide_dbl(
    .x = x,
    .f = ~ sum(tidyr::replace_na(.x, 0)),
    .before = 6,   # 6 previous + 1 current = 7 total
    .after   = 0,
    .complete = FALSE
  )
}


# Gompertz ---------------------------------------------------------------------

#' Gompertz Function
#'
#' Computes the value of the Gompertz function parameterized by \code{(y0, ymax, k, lag)}.
#'
#' @param x A numeric vector (the x-axis).
#' @param y0 Numeric. The initial value of the curve.
#' @param ymax Numeric. The asymptotic maximum value of the curve.
#' @param k Numeric. The growth rate parameter.
#' @param lag Numeric. The horizontal shift (lag).
#'
#' @details
#' The Gompertz function is often used in growth curve modeling, where:
#' \deqn{ G(x) = y0 + (ymax - y0)\exp\Bigl[-\exp\bigl(k(\mathrm{lag}-x)/(ymax-y0)+1\bigr)\Bigr]. }
#'
#' @return A numeric vector of length \code{length(x)}, with the Gompertz function
#'   values for each element of \code{x}.
#'
#' @examples
#' x_seq <- seq(0, 20, by = 0.5)
#' y_gomp <- Gompertz(x_seq, y0 = 1, ymax = 10, k = 0.3, lag = 10)
#' plot(x_seq, y_gomp, type = "l", main = "Gompertz Curve")
#'
#' @export
Gompertz <- function(x, y0, ymax, k, lag){
  stopifnot(is.numeric(x), is.numeric(y0), is.numeric(ymax),
            is.numeric(k), is.numeric(lag))

  result <- y0 + (ymax - y0) * exp(-exp(k * (lag - x)/(ymax - y0) + 1))
  return(result)
}


# loglog and iloglog -----------------------------------------------------------

#' Log-log transformation
#'
#' Applies the transformation:
#' \deqn{ \log\!\bigl(\log\bigl(\tfrac{x}{\alpha} + 1\bigr) + 1\bigr). }
#'
#' @param x A numeric vector.
#' @param alpha A numeric parameter that is subtracted before taking the log.
#'   Default is \code{1}.
#'
#' @details
#' Useful in certain scenarios where a "double log" scale is appropriate.
#'
#' @return A numeric vector with the log-log transformation applied.
#'
#' @examples
#' x <- 1:10
#' transformed <- loglog(x)
#' transformed
#'
#' @export
loglog <- function(x, alpha = 1){
  stopifnot(is.numeric(x), is.numeric(alpha))
  return(log(log(x / alpha + 1) + 1))
}

#' Inverse log-log transformation
#'
#' Applies the inverse of \code{\link{loglog}}:
#' \deqn{ \alpha \,\bigl[\exp(\exp(x) - 1) - 1\bigr]. }
#'
#' @param x A numeric vector that was transformed via \code{\link{loglog}}.
#' @param alpha The same \code{alpha} used in the original loglog transformation.
#'   Default is \code{1}.
#'
#' @return A numeric vector transformed back to the original scale.
#'
#' @examples
#' original <- 1:10
#' transformed <- loglog(original)
#' inverted <- iloglog(transformed)
#' all.equal(original, inverted)  # Should be close if alpha=1
#'
#' @export
iloglog <- function(x, alpha = 1){
  stopifnot(is.numeric(x), is.numeric(alpha))
  return(alpha * (exp(exp(x) - 1) - 1))
}


# shewhart_7points -------------------------------------------------------------

#' Detects phase changes in a time series using the "7 consecutive points" rule
#'
#' Applies a simplified Shewhart criterion to identify phases. If there are
#' 7 consecutive points above or below the central limit (CL),
#' it flags a phase change and refits the model in subsequent iterations.
#'
#' @param data A \code{data.frame} or \code{tibble} containing the data.
#' @param index_col Unquoted column name representing the index (dates, time, or integer).
#' @param values_col Unquoted numeric column with the values to be analyzed.
#' @param start_base Integer. Number of initial points used to define the base phase.
#'   Defaults to 10.
#' @param model A character string specifying the model used for fitting: one of
#'   \code{"log"} (default), \code{"loglog"}, or \code{"gompertz"}.
#' @param dummy_col (Optional) Unquoted column name of a dummy variable (if
#'   applicable). If \code{NULL}, no dummy variable is used.
#' @param ... Additional parameters (currently unused).
#'
#' @details
#' The function repeatedly checks new phases in blocks of 7 points:
#' \enumerate{
#'   \item Fits an initial model (via \code{shewhart_model}),
#'   \item Finds if 7 consecutive points lie entirely above or below the CL,
#'   \item If so, flags a new phase change at \code{(index + 1)} and refits,
#'   \item Continues until no further changes are detected or data is exhausted.
#' }
#'
#' @return A vector of index positions (or dates) where a phase change was detected.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Simulated example
#' set.seed(123)
#' df <- tibble(
#'   time = 1:100,
#'   value = c(rpois(50, 5), rpois(50, 10))
#' )
#'
#' changes <- shewhart_7points(
#'   data = df,
#'   index_col = time,
#'   values_col = value,
#'   start_base = 10,
#'   model = "log"
#' )
#' print(changes)
#' }
#'
#' @export
shewhart_7points <- function(data, index_col, values_col,
                             start_base = 10,
                             model = "log",
                             dummy_col = NULL,
                             ...){

  # Basic input checks
  stopifnot(is.data.frame(data),
            nrow(data) > start_base,
            model %in% c("log", "loglog", "gompertz"))

  # Verify the existence of the specified columns
  index_name  <- rlang::quo_name(rlang::enquo(index_col))
  values_name <- rlang::quo_name(rlang::enquo(values_col))
  dummy_name  <- rlang::quo_name(rlang::enquo(dummy_col))

  if (!all(c(index_name, values_name) %in% names(data))) {
    stop("The specified index_col or values_col were not found in `data`.")
  }

  # If dummy_col was provided, check it
  if ("NULL" != dummy_name) {
    if (!all(dummy_name %in% names(data))) {
      stop("The specified dummy_col was not found in `data`.")
    }
  }

  # 1) Subset the data to the needed columns
  if ("NULL" != dummy_name) {
    subdata <- data %>%
      dplyr::select({{index_col}}, {{values_col}}, {{dummy_col}})
  } else {
    subdata <- data %>%
      dplyr::select({{index_col}}, {{values_col}})
  }

  # 2) Build initial model
  if ("NULL" == dummy_name) {
    running_model <- shewhart_model(
      data       = subdata,
      index_col  = {{index_col}},
      values_col = {{values_col}},
      start_base = start_base,
      model      = model
    )
  } else {
    running_model <- shewhart_model(
      data       = subdata,
      index_col  = {{index_col}},
      values_col = {{values_col}},
      start_base = start_base,
      dummy_col  = {{ dummy_col }},
      model      = model
    )
  }

  # 3) Collect initial phase changes
  found_phase_dates <- running_model %>%
    dplyr::filter(.data$change) %>%
    dplyr::pull({{index_col}})

  # 4) Iterate to find further changes
  for (i in seq_len(ceiling(nrow(subdata) / 7))) {

    # Only look at the last (most recent) phase
    autodate <- running_model %>%
      dplyr::select({{index_col}}, {{values_col}}, .data$fitted, .data$CL, .data$phase) %>%
      dplyr::filter(.data$phase == max(.data$phase))

    # If that phase has > 7 points, check for consecutive above/below
    if (nrow(autodate) > 7) {
      autodate_vector <- autodate %>%
        dplyr::mutate(
          upper = dplyr::if_else({{values_col}} > .data$CL, 1, 0),
          lower = dplyr::if_else({{values_col}} < .data$CL, 1, 0),
          upper_cs = rolling_sum(.data$upper),
          lower_cs = rolling_sum(.data$lower)
        ) %>%
        dplyr::filter(.data$upper_cs == 7 | .data$lower_cs == 7) %>%
        dplyr::slice_min(n = 1, order_by = {{index_col}}) %>%
        dplyr::pull({{index_col}})

      # If any date found, add date+1
      if (length(autodate_vector) > 0) {
        found_phase_dates <- unique(c(found_phase_dates, autodate_vector + 1))
      }
    } else {
      break
    }

    # 5) Refit the model with updated phase changes
    if ("NULL" == dummy_name) {
      running_model <- shewhart_model(
        data          = subdata,
        index_col     = {{index_col}},
        values_col    = {{values_col}},
        phase_changes = found_phase_dates,
        start_base    = start_base,
        model         = model
      )
    } else {
      running_model <- shewhart_model(
        data          = subdata,
        index_col     = {{index_col}},
        values_col    = {{values_col}},
        phase_changes = found_phase_dates,
        start_base    = start_base,
        dummy_col     = {{ dummy_col }},
        model         = model
      )
    }
  }

  return(found_phase_dates)
}


# shewhart_fit -----------------------------------------------------------------

#' Fits a model (log, loglog, or modified gompertz) for Shewhart analysis
#'
#' Extends the original Shewhart fitting function to optionally include a dummy
#' variable in the model. For "log" and "loglog" models, it uses \code{lm(...)}
#' with or without the dummy; for "gompertz", it uses a custom \code{nls(...)}
#' formula.
#'
#' @param data A \code{data.frame} or \code{tibble} containing the data.
#' @param index_col Unquoted column name representing the index (dates, time, or integer).
#' @param values_col Unquoted numeric column with the values to be analyzed.
#' @param model A character string: \code{"log"} (default), \code{"loglog"}, or \code{"gompertz"}.
#' @param dummy_col (Optional) Unquoted column name of a dummy variable. If
#'   \code{NULL}, no dummy is added. If provided, that column must exist in \code{data}.
#' @param ... Additional parameters (currently unused).
#'
#' @details
#' \enumerate{
#'   \item For \code{model = "log"}, it fits \eqn{\log(values + 1) \sim N (+ \mathrm{dummy})}.
#'   \item For \code{model = "loglog"}, it fits \eqn{\mathrm{loglog}(values) \sim N (+ \mathrm{dummy})}.
#'   \item For \code{model = "gompertz"}, it uses an \code{nls(...)} call with
#'         \code{SSgompertz}, ignoring \code{dummy_col} in this implementation.
#' }
#'
#' @return The fitted model object (e.g., an \code{lm} or \code{nls} object).
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Example data with a dummy
#' df <- tibble(
#'   time = 1:20,
#'   value = rpois(20, lambda = 5),
#'   dummy_var = rep(c(0, 1), each = 10)
#' )
#'
#' # Fit log model with dummy
#' fit_log <- shewhart_fit(
#'   data       = df,
#'   index_col  = time,
#'   values_col = value,
#'   model      = "log",
#'   dummy_col  = dummy_var
#' )
#' summary(fit_log)
#'
#' # Fit gompertz model (ignoring dummy)
#' fit_gomp <- shewhart_fit(
#'   data       = df,
#'   index_col  = time,
#'   values_col = value,
#'   model      = "gompertz"
#' )
#' summary(fit_gomp)
#' }
#'
#' @export
shewhart_fit <- function(data, index_col, values_col,
                         model = "log",
                         dummy_col = NULL,
                         ...) {

  stopifnot(
    is.data.frame(data),
    model %in% c("log", "loglog", "gompertz")
  )

  # Collect names
  index_name  <- rlang::quo_name(rlang::enquo(index_col))
  values_name <- rlang::quo_name(rlang::enquo(values_col))
  dummy_name  <- rlang::quo_name(rlang::enquo(dummy_col))

  # Basic checks
  if (!all(c(index_name, values_name) %in% names(data))) {
    stop("The specified index_col or values_col were not found in `data`.")
  }

  if ("NULL" != dummy_name) {
    if (!all(dummy_name %in% names(data))) {
      stop("The specified dummy_col was not found in `data`.")
    }
  }

  # Helper to build formula
  add_dummy_to_formula <- function(response, dummy) {
    if ("NULL" != dummy) {
      # e.g., "log_var ~ N + dummy_var"
      as.formula(paste0(response, " ~ N + ", dummy))
    } else {
      as.formula(paste0(response, " ~ N"))
    }
  }

  if (model == "log") {
    # For the log model
    data <- data %>%
      dplyr::mutate(log_var = log({{values_col}} + 1))

    form <- add_dummy_to_formula("log_var", dummy_name)
    fit  <- stats::lm(form, data = data)

  } else if (model == "loglog") {
    # For the loglog model
    data <- data %>%
      dplyr::mutate(llvar = loglog({{values_col}}, 1))

    form <- add_dummy_to_formula("llvar", dummy_name)
    fit  <- stats::lm(form, data = data)

  } else if (model == "gompertz") {
    # Currently ignoring dummy for Gompertz
    # e.g., avar = Asym * exp(-b2 * exp(-b3*N))
    if ("NULL" != dummy_name) {
      warning("Ignoring dummy_col in Gompertz model.")
    }

    fit <- NA
    tryCatch({
      fit <- data %>%
        dplyr::mutate(
          avar = cumsum({{values_col}}) + 1
        ) %>%
        stats::nls(
          avar ~ SSgompertz(N, Asym, b2, b3),
          data    = .,
          start   = list(Asym = 10000, b2 = 2, b3 = 0.02),
          control = nls.control(maxiter = 500)
        )
    }, error = function(e) {
      warning("Failed to fit the Gompertz model. Check your data or try a different model.")
    })

    if (is.null(fit) || !inherits(fit, "nls")) {
      stop("Gompertz model could not be fitted.")
    }
  }

  return(fit)
}


# shewhart_model ---------------------------------------------------------------

#' Builds a Shewhart phase model (supports dummy variable)
#'
#' Constructs a data frame annotated with phase information, control limits,
#' and fitted values for each time/index point. Optionally incorporates a dummy
#' variable (\code{dummy_col}) in the model if \code{model} is "log" or "loglog".
#'
#' @param data A data.frame or tibble containing the data.
#' @param values_col Unquoted numeric column with the values to be analyzed.
#' @param index_col Unquoted column representing the index (dates, time, or integer).
#' @param start_base Integer. Number of initial points used to define the base phase.
#'   Defaults to 10.
#' @param model A character string specifying which model to fit. Must be one of
#'   \code{"log"}, \code{"loglog"}, or \code{"gompertz"}.
#' @param phase_changes (Optional) A vector of dates/positions where phase changes
#'   have already been identified. If \code{NULL}, assumes only one initial change
#'   after \code{start_base} points.
#' @param locale A character string for the language setting ("pt_BR" by default).
#'   Determines the labeling of phases in the output (e.g., "Base", "Monitorando").
#' @param dummy_col (Optional) Unquoted column name for a dummy variable. If \code{NULL},
#'   no dummy is used. If provided, that column must exist in \code{data}.
#' @param ... Additional parameters (currently unused).
#'
#' @details
#' 1. Identifies phases by marking \code{(min(index) + start_base)} and any
#'    \code{phase_changes} as breakpoints.
#' 2. Splits the data by phase, fits a model (\code{\link{shewhart_fit}}),
#'    and calculates residuals and control limits.
#' 3. Converts the fitted values back to the original data scale in \code{CL},
#'    \code{UL_EXP}, and \code{LL_EXP}.
#' 4. Labels each phase string in Portuguese or English, based on \code{locale}.
#'
#' @return A tibble containing:
#' \itemize{
#'   \item \code{index_col}, \code{values_col}: Original columns,
#'   \item \code{phase}: Numeric phase identifier,
#'   \item \code{model}: The model type used,
#'   \item \code{change}: Logical indicator of where a new phase begins,
#'   \item \code{fitted}: Fitted values (on the model scale),
#'   \item \code{residuals}: Residuals,
#'   \item \code{UCL}, \code{LCL}, \code{CL}: Control limits,
#'   \item \code{UL_EXP}, \code{LL_EXP}: Control limits mapped back to the original scale,
#'   \item \code{phase_string}: Text label for the phase ("Base", "Monitorando", etc.).
#' }
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Example data
#' set.seed(123)
#' df <- tibble(
#'   time = 1:30,
#'   value = c(rpois(10, 5), rpois(10, 10), rpois(10, 15))
#' )
#'
#' # Build a Shewhart model with default "log"
#' shew_model <- shewhart_model(
#'   data          = df,
#'   values_col    = value,
#'   index_col     = time,
#'   start_base    = 10,
#'   model         = "log"
#' )
#' head(shew_model)
#'
#' # Incorporate a dummy:
#' df2 <- tibble(
#'   time = 1:20,
#'   value = rpois(20, 5),
#'   dummy_var = rep(c(0, 1), each = 10)
#' )
#' shew_model2 <- shewhart_model(
#'   data       = df2,
#'   values_col = value,
#'   index_col  = time,
#'   model      = "log",
#'   dummy_col  = dummy_var
#' )
#' head(shew_model2)
#' }
#'
#' @export
shewhart_model <- function(data, values_col, index_col,
                           start_base = 10,
                           model = "log",
                           phase_changes = NULL,
                           locale = "pt_BR",
                           dummy_col = NULL,
                           ...) {

  stopifnot(
    is.data.frame(data),
    model %in% c("log", "loglog", "gompertz")
  )

  # Collect names
  index_name  <- rlang::quo_name(rlang::enquo(index_col))
  values_name <- rlang::quo_name(rlang::enquo(values_col))
  dummy_name  <- rlang::quo_name(rlang::enquo(dummy_col))

  # Verify existence of index_col and values_col
  if (!all(c(index_name, values_name) %in% names(data))) {
    stop("The specified index_col or values_col were not found in `data`.")
  }

  # If dummy_col is provided, check it
  if ("NULL" != dummy_name) {
    if (!all(dummy_name %in% names(data))) {
      stop("The specified dummy_col was not found in `data`.")
    }
  }

  # 1) Select columns
  if ("NULL" != dummy_name) {
    data_subset <- data %>%
      dplyr::select({{index_col}}, {{values_col}}, {{dummy_col}})
  } else {
    data_subset <- data %>%
      dplyr::select({{index_col}}, {{values_col}})
  }

  # 2) Create 'change' = TRUE at (min(index_col) + start_base) and at each phase_changes
  #    Must ensure the types match (numeric vs. Date, etc.).
  min_index <- min(dplyr::pull(data, {{ index_col }})) + start_base

  data_out <- data_subset %>%
    dplyr::left_join(
      tibble::tibble(
        {{index_col}} := unique(
          c(
            min_index,
            phase_changes
          )
        ),
        change = TRUE
      ),
      by = index_name
    ) %>%
    tidyr::replace_na(list(change = FALSE)) %>%
    dplyr::arrange({{index_col}}) %>%
    # Label phases
    dplyr::mutate(
      phase = cumsum(.data$change),
      model = model,
      # 'flag' picks which subset to fit
      flag = dplyr::if_else(.data$phase == max(.data$phase), .data$phase - 1L, .data$phase)
    ) %>%
    dplyr::group_by(.data$flag) %>%
    dplyr::mutate(N = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidyr::nest(data = -c(.data$phase)) %>%
    # 3) Fit model for each phase
    dplyr::mutate(
      fit = purrr::map(
        .data$data,
        ~ shewhart_fit(
          data       = .x,
          index_col  = {{index_col}},
          values_col = {{values_col}},
          model      = model,
          dummy_col  = {{dummy_col}}
        )
      ),
      tidied = purrr::map(.data$fit, broom::tidy)
    ) %>%
    # For the final phase, reuse previous fit
    dplyr::mutate(
      fit = dplyr::if_else(.data$phase == max(.data$phase), dplyr::lag(.data$fit), .data$fit),
      tidied = dplyr::if_else(.data$phase == max(.data$phase), dplyr::lag(.data$tidied), .data$tidied),
      fitted = purrr::map2(
        .data$data,
        .data$fit,
        ~ if (inherits(.y, "lm") || inherits(.y, "nls")) {
          stats::predict(.y, newdata = .x)
        } else {
          rep(NA, nrow(.x))
        }
      )
    ) %>%
    tidyr::unnest(c(.data$data, .data$fitted)) %>%
    # 4) Residuals by model
    dplyr::mutate(
      residuals = dplyr::if_else(
        model == "log",
        log({{values_col}} + 1) - .data$fitted,
        dplyr::if_else(
          model == "loglog",
          loglog({{values_col}}, 1) - .data$fitted,
          # Gompertz approach
          {{values_col}} - (.data$fitted - dplyr::lag(.data$fitted, default = 1))
        )
      )
    ) %>%
    dplyr::group_by(.data$flag) %>%
    dplyr::mutate(
      # 2.66 is Shewhart constant for ~95% control
      CONL_1 = 2.66 * mean(abs(.data$residuals - dplyr::lag(.data$residuals)), na.rm = TRUE),
      UCL = pmax(0, .data$fitted + .data$CONL_1, na.rm = TRUE),
      LCL = pmax(0, .data$fitted - .data$CONL_1, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    # 5) Convert to original scale
    dplyr::mutate(
      CL = dplyr::case_when(
        model == "log"      ~ pmax(exp(.data$fitted) - 1, 0, na.rm = TRUE),
        model == "loglog"   ~ pmax(iloglog(.data$fitted, 1), 0, na.rm = TRUE),
        model == "gompertz" ~ pmax(exp(.data$fitted), 0, na.rm = TRUE)
      ),
      UL_EXP = dplyr::case_when(
        model == "log"      ~ pmax(exp(.data$UCL) - 1, 0, na.rm = TRUE),
        model == "loglog"   ~ pmax(iloglog(.data$UCL, 1), 0, na.rm = TRUE),
        model == "gompertz" ~ pmax(exp(.data$UCL) - 1, 0, na.rm = TRUE)
      ),
      LL_EXP = dplyr::case_when(
        model == "log"      ~ pmax(exp(.data$LCL) - 1, 0, na.rm = TRUE),
        model == "loglog"   ~ pmax(iloglog(.data$LCL, 1), 0, na.rm = TRUE),
        model == "gompertz" ~ pmax(exp(.data$LCL), 0, na.rm = TRUE)
      )
    ) %>%
    # 6) Phase string in PT or EN
    dplyr::mutate(
      phase_string = dplyr::case_when(
        stringr::str_detect(locale, "en") & (phase == 0) ~ "Base",
        stringr::str_detect(locale, "en") & (phase == max(phase, na.rm = TRUE)) ~ "Monitoring",
        stringr::str_detect(locale, "en") & (phase > 0) & (phase != max(phase, na.rm = TRUE)) ~
          paste0("Phase ", phase),

        stringr::str_detect(locale, "pt") & (phase == 0) ~ "Base",
        stringr::str_detect(locale, "pt") & (phase == max(phase, na.rm = TRUE)) ~ "Monitorando",
        stringr::str_detect(locale, "pt") & (phase > 0) & (phase != max(phase, na.rm = TRUE)) ~
          paste0("Fase ", phase)
      )
    )

  return(data_out)
}

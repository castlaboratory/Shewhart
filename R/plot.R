# color_hue --------------------------------------------------------------------

#' Generates a qualitative color palette using HCL
#'
#' This utility function creates a sequence of distinct hues via the HCL
#' color space, returning a vector of colors. It is often useful for
#' categorical palettes in plots.
#'
#' @param n An integer specifying the number of colors to generate.
#'
#' @details
#' The function starts from hue = 15 and goes up to hue = 375 degrees,
#' returning \code{n} colors equally spaced in hue, with a constant
#' luminance (\code{l = 65}) and chroma (\code{c = 100}). Internally,
#' it uses \code{hcl()} to produce valid hex color codes.
#'
#' @return A character vector of length \code{n}, each element being a
#' valid color in hex format (e.g., \code{"#RRGGBB"}).
#'
#' @examples
#' # Generate a palette of 5 colors:
#' pal <- color_hue(5)
#' pal
#'
#' # You can then use 'pal' for plotting:
#' # barplot(1:5, col = pal)
#'
#' @export
color_hue <- function(n) {
  stopifnot(is.numeric(n), length(n) == 1, n > 0)
  hues <- seq(15, 375, length.out = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


# shewhart ---------------------------------------------------------------------

#' Plots Shewhart phases based on the 7-points rule
#'
#' Generates a control chart-like plot (either **ggplot2** or **plotly**) for a
#' time series, identifying potential phase changes according to the "7
#' consecutive points" rule.
#'
#' @param data A data frame or tibble containing at least two columns:
#'   one representing time/index (dates or integers) and one representing
#'   the measured values.
#' @param values_col Unquoted column name with the numeric values to be analyzed.
#' @param index_col Unquoted column name representing the index (could be dates
#'   or integers).
#' @param start_base An integer specifying how many initial points are treated
#'   as the base phase. Defaults to \code{10}.
#' @param type A character string, either \code{"ggplot"} (default) or
#'   \code{"plotly"}, indicating which plotting system to use.
#' @param rule A character string indicating the control chart rule used.
#'   Currently defaults to \code{"7 points"}. (Not actively used in the code,
#'   but reserved for future extension.)
#' @param model A character string. One of \code{"log"}, \code{"loglog"}, or
#'   \code{"gompertz"}. Passed to underlying modeling functions to determine
#'   which transformation or curve to fit.
#' @param locale A character string specifying the language/locale
#'   (e.g., \code{"pt_BR"} or \code{"en_US"}) for date/time labels and
#'   text. Defaults to \code{"pt_BR"}.
#' @param phase_changes (Optional) A vector of index positions or dates where
#'   known phase changes occur. If \code{NULL}, the function calls
#'   \code{\link{shewhart_7points}} internally to detect them.
#' @param dummy_col (Optional) Unquoted column name representing a dummy variable
#'   for advanced modeling. If \code{NULL}, no dummy variable is used. This is
#'   typically used by \code{\link{shewhart_model}} or \code{\link{shewhart_fit}}
#'   if you want to incorporate an additional factor in the model (e.g., a
#'   step shift or grouping).
#' @param ... Additional parameters (currently unused).
#'
#' @details
#' \enumerate{
#'   \item If the input \code{data} does not yet have columns like
#'         \code{phase}, \code{phase_string}, \code{CL}, etc., then the
#'         function will:
#'         \itemize{
#'           \item call \code{\link{shewhart_7points}} to determine where phase
#'                 changes occur (unless \code{phase_changes} is provided),
#'           \item call \code{\link{shewhart_model}} to fit the chosen model
#'                 (\code{model}) and produce the data needed for plotting
#'                 (limits, fitted values, etc.).
#'         }
#'   \item If \code{data} already has those columns from \code{\link{shewhart_model}},
#'         it uses them directly for plotting.
#'   \item The \code{type} argument decides whether a static \strong{ggplot2} chart
#'         or an interactive \strong{plotly} chart is returned.
#' }
#'
#' The "7 consecutive points" rule is a simplified Shewhart-like rule that flags a
#' potential process change if seven successive points lie either entirely above
#' or below the central limit line.
#'
#' @return A plot object:
#' \itemize{
#'   \item If \code{type = "ggplot"}, returns a \code{ggplot} object.
#'   \item If \code{type = "plotly"}, returns a \code{plotly} object.
#' }
#'
#' @seealso
#' \code{\link{shewhart_7points}}, \code{\link{shewhart_model}}
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Example data
#' set.seed(123)
#' df <- tibble(
#'   time = seq.Date(from = as.Date("2021-01-01"), by = "day", length.out = 30),
#'   value = c(rpois(15, 5), rpois(15, 10))
#' )
#'
#' # Basic usage with ggplot output
#' p <- shewhart(
#'   data       = df,
#'   values_col = value,
#'   index_col  = time,
#'   start_base = 5,
#'   type       = "ggplot",
#'   model      = "log",
#'   locale     = "en_US"
#' )
#' print(p)
#'
#' # Or get a plotly interactive chart
#' p2 <- shewhart(
#'   data       = df,
#'   values_col = value,
#'   index_col  = time,
#'   start_base = 5,
#'   type       = "plotly",
#'   model      = "log"
#' )
#' p2
#' }
#'
#' @export
shewhart <- function(data, values_col, index_col,
                     start_base = 10,
                     type = "ggplot",
                     rule = "7 points",
                     model = "log",
                     locale = "pt_BR",
                     phase_changes = NULL,
                     dummy_col = NULL,
                     ...) {

  # Basic checks
  stopifnot(is.data.frame(data))
  stopifnot(type %in% c("ggplot", "plotly"))
  stopifnot(is.numeric(start_base), start_base >= 1)
  stopifnot(model %in% c("log", "loglog", "gompertz"))

  # Temporarily change locale for date/time if locale is recognized
  locale_sys <- Sys.getlocale("LC_TIME")
  tryCatch(
    {
      Sys.setenv("LC_TIME" = dplyr::if_else(
        stringr::str_detect(locale, "en"),
        "en_US.UTF-8",
        "pt_BR.UTF-8"
      ))
    },
    error = function(e) {
      warning("Could not set LC_TIME locale. Using system default.")
    }
  )
  # Verify that index_col and values_col exist in data
  index_name  <- rlang::quo_name(rlang::enquo(index_col))
  values_name <- rlang::quo_name(rlang::enquo(values_col))
  dummy_name <- rlang::quo_name(rlang::enquo(dummy_col))

  if (!all(c(index_name, values_name) %in% names(data))) {
    stop("The specified index_col or values_col were not found in `data`.")
  }

  # If dummy_col is provided, check it
  if ("NULL" != dummy_name) {
    if (!all(dummy_name %in% names(data))) {
      stop("The specified dummy_col were not found in `data`.")
    }
  }

  # Check if 'data' already has the needed columns
  needed_cols <- c("phase", "phase_string", "CL", "UL_EXP", "LL_EXP",
                   "CONL_1", "fit", "fitted", "change")
  # If the columns are missing, we build them
  if (!all(needed_cols %in% names(data))) {

    if (is.null(phase_changes)) {

      if ("NULL" == dummy_name) {
        # Identify phase changes using 7-points rule
        phase_changes <- shewhart_7points(
          data         = data,
          index_col    = {{index_col}},
          values_col   = {{values_col}},
          start_base   = start_base,
          model        = model
        )
      } else {
        # Identify phase changes using 7-points rule
        phase_changes <- shewhart_7points(
          data         = data,
          index_col    = {{index_col}},
          values_col   = {{values_col}},
          start_base   = start_base,
          dummy_col = {{ dummy_col }},
          model        = model
        )
      }
    }

    if ("NULL" == dummy_name) {
      # Fit the model and annotate data for plotting

      plot_data <- shewhart_model(
        data          = data,
        index_col     = {{index_col}},
        values_col    = {{values_col}},
        start_base    = start_base,
        phase_changes = phase_changes,
        model         = model,
        locale        = locale
      )
    } else {
      # Fit the model and annotate data for plotting
      plot_data <- shewhart_model(
        data          = data,
        index_col     = {{index_col}},
        values_col    = {{values_col}},
        start_base    = start_base,
        phase_changes = phase_changes,
        dummy_col = {{ dummy_col }},
        model         = model,
        locale        = locale
      )
    }



  } else {
    # If the columns are already present, just use that
    phase_changes <- data %>%
      dplyr::filter(.data$change == TRUE) %>%
      dplyr::pull({{index_col}})
    plot_data <- data
  }

  # Prepare color palette
  pal_fases <- color_hue(length(phase_changes) + 1)

  # Find max y for chart limits
  y_max <- data %>%
    dplyr::pull({{values_col}}) %>%
    max(na.rm = TRUE)

  p <- NULL

  # GGplot version
  if (type == "ggplot") {
    p <- plot_data %>%
      ggplot2::ggplot() +
      ggplot2::theme_light() +
      ggplot2::geom_line(
        ggplot2::aes(x = {{index_col}}, y = LL_EXP, color = .data$phase_string),
        linewidth = 1.25, alpha = 0.25
      ) +
      ggplot2::geom_line(
        ggplot2::aes(x = {{index_col}}, y = UL_EXP, color = .data$phase_string),
        linewidth = 1.25, alpha = 0.25
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(x = {{index_col}}, ymin = .data$LL_EXP, ymax = .data$UL_EXP,
                     fill = .data$phase_string),
        alpha = 0.25
      ) +
      ggplot2::geom_line(
        ggplot2::aes(x = {{index_col}}, y = CL, color = .data$phase_string),
        linewidth = 1.25
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = {{index_col}}, y = {{values_col}})
      ) +
      ggplot2::geom_line(
        ggplot2::aes(x = {{index_col}}, y = {{values_col}}),
        alpha = 0.25
      ) +
      ggplot2::theme(
        legend.direction = "horizontal",
        legend.position  = "bottom"
      ) +
      ggplot2::labs(
        color = dplyr::if_else(
          stringr::str_detect(locale, "en"), "Phase", "Fase"
        ),
        fill  = dplyr::if_else(
          stringr::str_detect(locale, "en"), "Phase", "Fase"
        )
      ) +
      ggplot2::coord_cartesian(ylim = c(0, 1.1 * y_max)) +
      ggplot2::labs(
        y = rlang::as_label(rlang::enquo(values_col)),
        x = rlang::as_label(rlang::enquo(index_col))
      ) +
      ggplot2::scale_x_date(date_labels = "%b/%y") +
      ggplot2::scale_color_manual(values = pal_fases) +
      ggplot2::scale_fill_manual(values = pal_fases)
  }

  # Plotly version
  if (type == "plotly") {
    # Prepare day labels (in Portuguese or English)
    plot_data <- plot_data %>%
      dplyr::mutate(
        day = stringr::str_to_title(
          lubridate::wday({{ index_col }}, label = TRUE, abbr = FALSE,
                          locale = dplyr::if_else(
                            stringr::str_detect(locale, "en"),
                            "en_US.UTF-8", "pt_BR.UTF-8"
                          ))
        )
      )

    p <- plot_data %>%
      plotly::plot_ly(colors = pal_fases, source = "A") %>%
      plotly::add_ribbons(
        hoverinfo  = "none",
        x          = rlang::enquo(index_col),
        ymin       = ~LL_EXP,
        ymax       = ~UL_EXP,
        fillcolor  = ~phase_string,
        color      = ~phase_string,
        opacity    = 0.25
      ) %>%
      plotly::add_trace(
        x           = rlang::enquo(index_col),
        y           = ~CL,
        type        = "scatter",
        mode        = "markers+lines",
        color       = ~phase_string,
        showlegend  = FALSE,
        text        = ~day,
        hovertemplate = dplyr::if_else(
          stringr::str_detect(locale, "en"),
          "<extra><b>Value</b>: %{y:d}<br><b>Date</b>: %{x}<br><b>Day</b>: %{text}</extra>",
          "<extra><b>Valor</b>: %{y:d}<br><b>Data</b>: %{x}<br><b>Dia</b>: %{text}</extra>"
        )
      ) %>%
      plotly::add_trace(
        x           = rlang::enquo(index_col),
        y           = rlang::enquo(values_col),
        type        = "scatter",
        mode        = "markers+lines",
        showlegend  = FALSE,
        line        = list(color = "rgb(0.25, 0.25, 0.25, 0.75)"),
        marker      = list(
          color = "rgb(0.25, 0.25, 0.25, 0.75)",
          line  = list(color = "rgb(0.25, 0.25, 0.25, 0.75)", width = 0)
        ),
        hoverinfo   = "text",
        text        = ~day,
        hovertemplate = dplyr::if_else(
          stringr::str_detect(locale, "en"),
          "<extra><b>Value</b>: %{y:d}<br><b>Date</b>: %{x}<br><b>Day</b>: %{text}</extra>",
          "<extra><b>Valor</b>: %{y:d}<br><b>Data</b>: %{x}<br><b>Dia</b>: %{text}</extra>"
        )
      ) %>%
      plotly::layout(
        margin = list(l = 5, r = 5, b = 5, t = 5, pad = 5),
        legend = list(
          orientation = "h",
          x = 0.05,
          y = 1.05,
          bgcolor = "rgba(0,0,0,0)"
        ),
        yaxis = list(
          title = rlang::as_label(rlang::enquo(values_col)),
          range = c(0, 1.2 * y_max)
        ),
        xaxis = list(
          title = rlang::as_label(rlang::enquo(index_col)),
          type = "date",
          tickformat = "%b/%y",
          dtick = 86400000.0 * 30
        )
      ) %>%
      plotly::config(
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d",
                                   "resetScale2d", "hoverClosestCartesian",
                                   "hoverCompareCartesian", "select2d", "lasso2d",
                                   "toggleSpikelines"),
        displayModeBar = TRUE,
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          width  = 1200,
          height = 600,
          scale  = 1
        )
      )

    # Set locale for plotly (if needed for translations)
    if (stringr::str_detect(locale, "pt")) {
      p <- plotly::config(p, locale = "pt-br")
    }
  }

  # Restore original locale
  Sys.setenv("LC_TIME" = locale_sys)

  return(p)
}

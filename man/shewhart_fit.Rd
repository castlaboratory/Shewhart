% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Shewhart.R
\name{shewhart_fit}
\alias{shewhart_fit}
\title{Fits a model (log, loglog, or modified gompertz) for Shewhart analysis}
\usage{
shewhart_fit(data, index_col, values_col, model = "log", dummy_col = NULL, ...)
}
\arguments{
\item{data}{A \code{data.frame} or \code{tibble} containing the data.}

\item{index_col}{Unquoted column name representing the index (dates, time, or integer).}

\item{values_col}{Unquoted numeric column with the values to be analyzed.}

\item{model}{A character string: \code{"log"} (default), \code{"loglog"}, or \code{"gompertz"}.}

\item{dummy_col}{(Optional) Unquoted column name of a dummy variable. If
\code{NULL}, no dummy is added. If provided, that column must exist in \code{data}.}

\item{...}{Additional parameters (currently unused).}
}
\value{
The fitted model object (e.g., an \code{lm} or \code{nls} object).
}
\description{
Extends the original Shewhart fitting function to optionally include a dummy
variable in the model. For "log" and "loglog" models, it uses \code{lm(...)}
with or without the dummy; for "gompertz", it uses a custom \code{nls(...)}
formula.
}
\details{
\enumerate{
  \item For \code{model = "log"}, it fits \eqn{\log(values + 1) \sim N (+ \mathrm{dummy})}.
  \item For \code{model = "loglog"}, it fits \eqn{\mathrm{loglog}(values) \sim N (+ \mathrm{dummy})}.
  \item For \code{model = "gompertz"}, it uses an \code{nls(...)} call with
        \code{SSgompertz}, ignoring \code{dummy_col} in this implementation.
}
}
\examples{
\dontrun{
library(dplyr)

# Example data with a dummy
df <- tibble(
  time = 1:20,
  value = rpois(20, lambda = 5),
  dummy_var = rep(c(0, 1), each = 10)
)

# Fit log model with dummy
fit_log <- shewhart_fit(
  data       = df,
  index_col  = time,
  values_col = value,
  model      = "log",
  dummy_col  = dummy_var
)
summary(fit_log)

# Fit gompertz model (ignoring dummy)
fit_gomp <- shewhart_fit(
  data       = df,
  index_col  = time,
  values_col = value,
  model      = "gompertz"
)
summary(fit_gomp)
}

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Shewhart.R
\name{shewhart_model}
\alias{shewhart_model}
\title{Get Shewhard phases using 7 points rule}
\usage{
shewhart_model(
  data,
  values_col,
  index_col,
  start_base = 10,
  model = c("log", "competz"),
  phase_changes = character(),
  ...
)
}
\arguments{
\item{data}{A tibble white de}

\item{values_col}{Column with values to analyze}

\item{index_col}{Column with sequence of dates or intergers}

\item{start_base}{Number of points in start of the series to use as base}

\item{model}{One of log (default) and Gompetz.}

\item{phase_changes}{Vector with dates por positions for changes of phase}
}
\description{
Get Shewhard phases using 7 points rule
}

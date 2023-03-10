% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Shewhart.R
\name{shewhart_7points}
\alias{shewhart_7points}
\title{Get Shewhard phases using 7 points rule}
\usage{
shewhart_7points(
  data,
  index_col,
  values_col,
  start_base = 10,
  model = c("log", "competz"),
  ...
)
}
\arguments{
\item{data}{A tibble white de}

\item{index_col}{Column with sequence of dates or intergers}

\item{values_col}{Column with values to analyze}

\item{start_base}{Number of points in start of the series to use as base}

\item{model}{One of log (default) and Gompetz.}
}
\description{
Get Shewhard phases using 7 points rule
}

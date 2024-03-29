% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{filterOne}
\alias{filterOne}
\title{Filter a single dataframe}
\usage{
filterOne(data, data_name, filter_cols, case_sensitive, silent)
}
\arguments{
\item{data}{A tibble of pine data}

\item{data_name}{The name of the data table (see \code{names(GetColSpec())} for valid options)}

\item{filter_cols}{Named vector where names are column names and values are values to filter on. This is created in the filterPine function.}

\item{case_sensitive}{Should non-numeric filters be treated as case-sensitive?}

\item{silent}{Suppress informational messages?}
}
\value{
A tibble of filtered data
}
\description{
Helper function for filterPine, not to be used outside that function
}

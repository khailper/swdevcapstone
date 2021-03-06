% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Clean location Column}
\usage{
eq_location_clean(column_id)
}
\arguments{
\item{column_id}{The column to clean up.  Uses notation data_frame$column}
}
\value{
The cleaned up column. Note that the function does not return the entire dataframe
}
\description{
Cleans up a column by removiewing everything up to and including the rightmost column, removing excess whitespace, and transforming the text to title case.
}

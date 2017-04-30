#' Information on ~6000 earthquakes
#'
#' A dataset with information about almost 6,000 earthquakes, including time and place.
#'
#' @format A data frame with 5934 rows and 47 variables, including:
#' \describe{
#'   \item{YEAR}{what year the earthquake happened}
#'   \item{MONTH}{numeric value value of month of earthquake (i.e. 2, not "february" or "Feb")}
#'   \item{DAY}{day of the month when the earthquake happened}
#'   \item{EQ_PRIMARY}{magnitude of the earthquake}
#'   \item{COUNTRY}{country where earthquake happened}
#'   \item{LOCATON_NAME}{where in COUNTRY earthquake happends (also includes country name)}
#'   \item{LATITUDE}{latitude of earthquake}
#'   \item{LONGITUDE}{longitude of earthquake}
#'   \item{DEATHS}{number of people killed in the earthquake}
#' }
#' @source \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
"earthquakes"

#' Calculate survey duration
#'
#' Loads raw HSS datafile and performs some basic cleaning: proper encoding of
#' Arabic, dates formatted as date-time objects, calculate survey duration.
#'
#' @param data The most recent iteration of the data set
#' @param soft Software input to determine how duration is calculated (KOBO vs ODK)
#'
#' @return A dataframe with an added duration column
#' @export
#' @rdname C_hss_surveyduration
#'
C_hss_surveyduration <- function(data, soft){
  if (soft == "KOBO") {.duration_KOBO(data)}
  else if (soft == "ODK") {.duration_ODK(data)}
}





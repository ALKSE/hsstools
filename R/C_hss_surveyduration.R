#' Calculate survey duration
#'
#' Loads raw HSS datafile and performs some basic cleaning: proper encoding of
#' Arabic, dates formatted as date-time objects, calculate survey duration.
#'
#' @param data The most recent iteration of the data set
#'
#' @return A dataframe with an added duration column
#' @export
#' @rdname C_hss_surveyduration
C_hss_surveyduration <- function(data) {
  output <- data %>%
    dplyr::mutate(submission_date = lubridate::mdy_hms(submission_date)) %>%
    dplyr::mutate(start = lubridate::mdy_hms(start), end = lubridate::mdy_hms(end)) %>%
    dplyr::mutate(duration = end - start) %>%
    dplyr::mutate(duration = lubridate::as.duration(duration)) %>%
    dplyr::relocate(duration, .after = end)
  return(output)
}



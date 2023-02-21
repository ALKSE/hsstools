#' Calculate survey duration
#'
#' Loads raw HSS datafile and performs some basic cleaning: proper encoding of
#' Arabic, dates formatted as date-time objects, calculate survey duration.
#'
#' @param data The most recent iteration of the data set
#'
#' @return A dataframe with an added SurveyDuration column
#' @export
#' @rdname hss_surveyduration
hss_surveyduration <- function(data) {
  output <- data %>%
    dplyr::mutate(submission_date = lubridate::mdy_hms(submission_date)) %>%
    dplyr::mutate(start = lubridate::mdy_hms(start), end = lubridate::mdy_hms(end)) %>%
    dplyr::mutate(SurveyDuration = end - start) %>%
    dplyr::mutate(SurveyDuration = lubridate::as.duration(SurveyDuration)) %>%
    dplyr::relocate(SurveyDuration, .after = end)
  data_w_date <<- output
  return(data_w_date)
}



#' Load raw HSS Data
#' @description Loads raw HSS datafile and performs some basic cleaning: proper encoding of Arabic, dates formatted as date-time objects, calculate survey duration.
#' @param path Path to the HSS data file. Expects a .csv file
#' @param skip Number of rows to skip. Default is 0. Use this if you know how many rows contain test data.
#'
#' @return A dataframe with an added SurveyDuration column
#' @export
#'
hss_surveyduration <- function(path, skip = 0) {
  df <- readr::read_csv(path, skip = skip)
  # format date-time columns and add surveyduration column
  if (!"start" %in% names(df) | !"end" %in% names(df)) {
    stop("Column 'start' or 'end' missing")
  }
  df <- df %>%
    dplyr::mutate(
      SubmissionDate = lubridate::dmy_hms(SubmissionDate),
      start = lubridate::dmy_hms(start),
      end = lubridate::dmy_hms(end)
    ) %>%
    dplyr::mutate(SurveyDuration = end - start)
  df <- df %>%
    dplyr::mutate(SurveyDuration = lubridate::as.duration(SurveyDuration)) %>%
    dplyr::relocate(SurveyDuration, .after = end)
  return(df)
}

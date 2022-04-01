#' Convert columns to date-time and calculate survey duration
#'
#' @param df A dataframe containing the Kobo survey results
#'
#' @return A dataframe with columns 'SubmissionDate', 'end' and 'start' converted to ymd_hms format; and a new column SurveyDuration as a duration in seconds.
#' @export
#'
#' @examples
#' WIP
hss_surveyduration <- function(df) {
  df <- df %>% mutate(
    SubmissionDate = dmy_hms(SubmissionDate),
    start = dmy_hms(start),
    end = dmy_hms(end)
  ) %>%
    mutate(SurveyDuration = end - start)
  df <- df %>% mutate(SurveyDuration = as.duration(SurveyDuration)) %>%
    relocate(SurveyDuration, .after = end)
  df
}

# KOBO duration utility function ------------------------------------------------
# Due to issues with date-formating, seperate functions exist for ODK and KOBO
# hosted surveys. This initial function assists in the analysis of KOBO-hosted surveys.
.duration_KOBO <- function(data) {
  output <- data %>%
    dplyr::mutate(submission_date = lubridate::mdy_hms(submission_date)) %>%
    dplyr::mutate(start = lubridate::mdy_hms(start), end = lubridate::mdy_hms(end)) %>%
    dplyr::mutate(duration = end - start) %>%
    dplyr::mutate(duration = lubridate::as.duration(duration)) %>%
    dplyr::relocate(duration, .after = end)
  return(output)
}

# ODK duration utility function ------------------------------------------------
# Due to issues with date-formating, seperate functions exist for ODK and KOBO
# hosted surveys. This initial function assists in the analysis of KOBO-hosted surveys.
.duration_ODK <- function(data) {
  output_1 <- data %>%
    dplyr::mutate(date_simple = submission_date) %>%
    dplyr::relocate(date_simple, .after = submission_date)

  output_1$date_simple <- as.POSIXct(output_1$date_simple)
  output_1$date_simple <- gsub(" .*", "", output_1$date_simple)
  output_1$submission_date <- as.POSIXct(output_1$submission_date,format="%Y-%m-%dT%H:%M:%S")
  output_1$start <- as.POSIXct(output_1$start,format="%Y-%m-%dT%H:%M:%S")
  output_1$end <- as.POSIXct(output_1$end,format="%Y-%m-%dT%H:%M:%S")
  output_2 <- output_1 %>%
    dplyr::mutate(SurveyDuration = end - start) %>%
    dplyr::mutate(SurveyDuration = lubridate::as.duration(SurveyDuration)) %>%
    dplyr::relocate(SurveyDuration, .after = end)
  return(output_2)}

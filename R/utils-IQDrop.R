#' Utility function for hss_drop_surveys - Iraq
#' Drop invalid surveys based on selection criteria
#'
#' HSS surveys are considered valid when they meet a number of selection criteria.
#' First, the respondent must give informed consent both at the beginning and the
#' end of the survey. Second, the survey should be excluded if the respondent seemed
#' uncomfortable and someone else interfered in the survey. Third, the survey must
#' meet some duration criteria: in general the survey should not be shorter than
#' 22 minutes; for surveys with 3 or more incidents reported, it should not be shorter
#' than 27 minutes; for surveys with 4 or more incidents reported, it should not be
#' shorter than 32 minutes.

.drop_IQ <- function(dat) {
  # add duration col (move to other function?) NB: duration is in seconds
  if (!inherits(dat$start, "POSIXct")) {
    dat <- dat %>%
      dplyr::mutate(
        start = lubridate::mdy_hms(start),
        end = lubridate::mdy_hms(end)
      )
  }
  dat <- dplyr::mutate(dat, duration = end - start)

  # add col with total no. of incidents, with consideration for varying incident variables
  incidents_IQ_1 <- data.frame(
    rob=(1),
    bomb=(2),
    assault=(3),
    abuse=(4),
    prison=(5),
    kidnap=(6),
    kill=(7),
    protest=(8),
    secinc_oth=(9)
  )

  result <- janitor::compare_df_cols(dat, incidents_IQ_1, return = "mismatch")
  incidents_2 <- result$column_name

  dat <- dat %>%
    dplyr::mutate(
      total_incidents = rowSums(dplyr::across(dplyr::all_of(incidents_2), ~ .x == 1), na.rm = TRUE)
    )

  # filter surveys
  dat <- dat %>%
    # filter for surveys with first and second consent given
    dplyr::filter(consent == 1 & consent2 == 1) %>%
    # exclude surveys with atmosphere: uncomfortable & interference
    dplyr::filter(!(atmosphere_uncomf == 1 & atmosphere_interfered == 1)) %>%
    mutate_at(c("atmosphere_uncomf", "atmosphere_interfered"), as.numeric) %>%
    mutate_at(c("atmosphere_uncomf", "atmosphere_interfered"),~replace_na(.,0)) %>%
    dplyr::filter(!(atmosphere_uncomf == 1 & atmosphere_interfered == 1)) %>%
    # filter for surveys with total duration > 22 mins
    dplyr::filter(duration > (22 * 60)) %>%
    # exclude suveys with 3+ incidents and total duration < 27 mins
    dplyr::filter(!(total_incidents > 2 & duration < (27 * 60))) %>%
    # # exclude surveys with 4+ incidents and total duration < 32 mins
    dplyr::filter(!(total_incidents > 3 & duration < (32 * 60)))

  # clean up
  dat <- dat %>% dplyr::select(!total_incidents)
  return(dat)
}

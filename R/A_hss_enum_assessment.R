#' Perform enumerator assessment on HSS dataframe
#'
#' This function is used to perform the first step in the enumerator assessment.
#' It groups output by device ID and shows the total surveys collected and the
#' number of surveys remained after each of the selection criteria is applied. The
#' function accepts an optional argument of an audit dataframe created with
#' `hss_load_audit()`. If this argument is left empty the final selection criterion is skipped.
#'
#' @param dat A dataframe containing the HSS survey data.
#' @param audit An audit file created with `hss_load_audit()`
#'
#' @return Returns a list containing an overview dataframe & a list of individual
#' dataframes for all criteria.
#' @export
#'
A_hss_enum_assessment <- function(dat, audit = NULL) {
  incidents <- c(
    "catt", "rob", "prison", "recruit", "kidnap", "assault", "kill",
    "bomb", "fmarr", "sex", "secinc_oth"
  )

  auditer <- if (!is.null(audit)) {
    audit %>%
      filter(instance_id %in%
        filter(dat, consent == 1 & consent2 == 1 &
          !(atmosphere_uncomf == 1 & atmosphere_interfered == 1))$instance_id) %>%
      filter(!(diff_min > 10 & grepl("Q.+", node))) %>%
      group_by(instance_id) %>%
      summarise(duration = sum(diff_min, na.rm = TRUE)) %>%
      filter(duration > 22)
  } else {
    NULL
  }

  surveys <- list(
    surv_all = dat %>%
      group_by(deviceid) %>%
      summarise(surveys_all = n()),
    surv_cons = dat %>%
      filter(consent == 1 & consent2 == 1 &
        !(atmosphere_uncomf == 1 & atmosphere_interfered == 1)) %>%
      group_by(deviceid) %>%
      summarise(surveys_consent = n()),
    surv_dur = dat %>%
      filter(consent == 1 & consent2 == 1 &
        !(atmosphere_uncomf == 1 & atmosphere_interfered == 1) &
        duration > (22 * 60)) %>%
      group_by(deviceid) %>%
      summarise(surveys_22 = n()),
    # criterion for dropping surveys with 3+ incidents (27 mins)
    surv_dur_2 = dat %>%
      mutate(across(all_of(incidents), ~ if_else(.x == 1, 1, 0)),
        tot_inc = rowSums(across(all_of(incidents)))
      ) %>%
      filter(consent == 1 & consent2 == 1 &
        !(atmosphere_uncomf == 1 & atmosphere_interfered == 1) &
        duration > (22 * 60) &
        !(tot_inc > 2 & duration < (27 * 60))) %>%
      group_by(deviceid) %>%
      summarise(surveys_27 = n()),
    # criterion for dropping surveys with 4+ incidents (32 mins)
    surv_dur_3 = dat %>%
      mutate(across(all_of(incidents), ~ if_else(.x == 1, 1, 0)),
        tot_inc = rowSums(across(all_of(incidents)))
      ) %>%
      filter(consent == 1 & consent2 == 1 &
        !(atmosphere_uncomf == 1 & atmosphere_interfered == 1) &
        duration > (22 * 60) &
        !(tot_inc > 2 & duration < (27 * 60)) &
        !(tot_inc > 3 & duration < (32 * 60))) %>%
      group_by(deviceid) %>%
      summarise(surveys_32 = n()),
    surv_dur_strict = if (!is.null(audit)) {
      dat %>%
        mutate(across(all_of(incidents), ~ if_else(.x == 1, 1, 0)),
          tot_inc = rowSums(across(all_of(incidents)))
        ) %>%
        filter(consent == 1 & consent2 == 1 &
          !(atmosphere_uncomf == 1 & atmosphere_interfered == 1) &
          duration > (22 * 60) &
          !(tot_inc > 2 & duration < (27 * 60)) &
          !(tot_inc > 3 & duration < (32 * 60))) %>%
        filter(instance_id %in% auditer[[1]]) %>%
        group_by(deviceid) %>%
        summarise(surveys_5_22 = n())
    }
  )

  survey_overview <- cbind(surveys$surv_all,
    surveys_consent = surveys$surv_cons$surveys_consent,
    surveys_22 = surveys$surv_dur$surveys_22,
    surveys_27 = surveys$surv_dur_2$surveys_27,
    surveys_32 = surveys$surv_dur_3$surveys_32
  )


  if (!is.null(audit)) {
    survey_overview <- cbind(survey_overview,
      surveys_5_22 = surveys$surv_dur_strict$surveys_5_22
    )
  }

  out <- list(
    overview = survey_overview,
    detailed = surveys
  )

  return(out)
}

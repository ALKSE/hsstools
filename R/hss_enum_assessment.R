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
hss_enum_assessment <- function(dat, audit = NULL) {
  # filter based on audit file: skip all questions with duration > 5 mins from total
  # duration calculations.select all surveys w duration > 22mins.
  audit2 <- if (!is.null(audit)) {
    audit %>%
      filter(instance_id %in%
               filter(dat, consent == 1 & consent2 == 1 &
                        !(atmosphere_uncomf == 1 & atmosphere_interfered == 1))$instance_id) %>%
      filter(!(diff_min > 5 & grepl("Q.+", node))) %>%
      group_by(instance_id) %>%
      summarise(duration = sum(diff_min, na.rm = TRUE)) %>%
      filter(duration > 22)
  } else {
    NULL
  }
  #create dataframes for survey criteria
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
    surv_dur_strict = if (!is.null(audit)) {
      dat %>%
        filter(consent == 1 & consent2 == 1 &
                 !(atmosphere_uncomf == 1 & atmosphere_interfered == 1) &
                 duration > (22 * 60)) %>%
        filter(instance_id %in% audit2$instance_id) %>%
        group_by(deviceid) %>%
        summarise(surveys_5_22 = n())
    }
  )

  survey_overview <- cbind(surveys$surv_all,
                           surveys_consent = surveys$surv_cons$surveys_consent,
                           surveys_22 = surveys$surv_dur$surveys_22
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

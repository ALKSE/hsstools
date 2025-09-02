#' Produce a duration assessment for individual enumerators
#'
#'
#' This function generates three lists that provide clarity on: (1) The screen-time
#' duration + instance_ids of all audited surveys, (2) the screen-time
#' duration + instance_ids of valid surveys, and (3) the screen-time duration +
#' instance_ids of invalid surveys.
#'
#' @param dat dataset that is prepared for the audit procedure
#' @param audit audit files (media export)
#' @param name the actual name of the enumerator in question
#' @param max a numerical value that is used to exclude lengthy question nodes
#'
#' @return three lists containing a detailed (duration) overview of clean surveys
#' @export
#'
#'
#' @rdname A_hss_details
#'
A_hss_details <- function(dat, audit, name, max){


  #Incidents grouped to ensure exclusions can be made with account to our incident calculations.
  incidents <- c(
    "catt", "rob", "prison", "recruit", "kidnap", "assault", "kill",
    "bomb", "fmarr", "sex", "narco", "suicide", "secinc_oth"
  )


  #Make sure that data is cleaned with the audit in-mind
  a <- subset(dat, deviceid == as.character(name))
  b <- mutate(duration_temp = as.numeric(duration, "minutes"))
  c <- b %>% dplyr::mutate(across(all_of(incidents), ~ if_else(.x == 1, 1, 0)),
                           tot_inc = rowSums(across(all_of(incidents)))) %>%
    filter(consent == 1 & consent2 == 1 & !(atmosphere_uncomf == 1 & atmosphere_interfered == 1) &
             duration > (22 * 60) &
             !(tot_inc > 2 & duration < (27 * 60)) &
             !(tot_inc > 3 & duration < (32 * 60)))

  #This provides an overview of survey duration, only basic exclusions.
  #Note that duration can be under 22 here. This refers to screen-time.
  #In terms of KOBO/ODK start-end, these surveys generally exceed 22mins.
  audit_special <- subset(audit, audit$instance_id %in% c$instance_id)
  audit_proof_base <- audit_special %>%
    filter(instance_id %in%
             filter(dat, consent == 1 & consent2 == 1 &
                      !(atmosphere_uncomf == 1 & atmosphere_interfered == 1))$instance_id) %>%
    group_by(instance_id) %>%
    summarise(duration = sum(diff_min, na.rm = TRUE))

  #This overview excludes entries related to screen-time.
  audit_proof_advanced <- audit_special %>%
    filter(instance_id %in%
             filter(dat, consent == 1 & consent2 == 1 &
                      !(atmosphere_uncomf == 1 & atmosphere_interfered == 1))$instance_id) %>%
    filter(!(diff_min > max & grepl("Q.+", node))) %>%
    group_by(instance_id) %>%
    summarise(duration = sum(diff_min, na.rm = TRUE)) %>%
    filter(duration > 22)

  #This overview specifically presents the exclusions related to screen-time.
  audit_proof_evidence <- subset(audit_proof_base, !audit_proof_base$instance_id %in%
                                   audit_proof_advanced$instance_id)


  #Combine into a nice list
  audit_details <- c(list(audit_proof_base), list(audit_proof_advanced, list(audit_proof_evidence)))
  names(audit_details) <- c("Valid_General", "Valid_Screentime", "Invalid_Screentime")

  return(audit_details)
}

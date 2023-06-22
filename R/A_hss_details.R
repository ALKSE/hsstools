#' Produce a duration assessment for individual enumerators
#'
#'
#' This function generates two lists that provide clarity on: (1) The screen-time
#' duration + instance_ids of all audited surveys, and (2) the screen-time
#' duration + instance_ids of valid surveys.
#'
#' @param dat dataset that is prepared for the audit procedure
#' @param audit audit files (media export)
#' @param name the actual name of the enumerator in question
#' @param max a numerical value that is used to exclude lengthy question nodes
#'
#' @return two lists containing a detailed (duration) overview of clean surveys
#' @export
#'
#'
#' @rdname A_hss_details
#'
A_hss_details <- function(dat, audit, name, max){
  #make sure that data is cleaned with the audit in-mind
  a <- subset(dat, deviceid == as.character(name))
  b <- a %>% mutate(duration_temp = duration/60)
  c <- subset(b, duration_temp > 22 & consent == 1 & consent2 == 1 &
                !(atmosphere_uncomf == 1 & atmosphere_interfered == 1))

  #This provides an overview of survey duration, only basic exclusions.
  #Note that duration can be under 22 here. This refers to screentime.
  #In terms of KOBO/ODK start-end, these surveys exceed 22mins.
  audit_special <- subset(audit, audit$instance_id %in% c$instance_id)
  audit_proof_base <- audit_special %>%
    filter(instance_id %in%
             filter(dat, consent == 1 & consent2 == 1 &
                      !(atmosphere_uncomf == 1 & atmosphere_interfered == 1))$instance_id) %>%
    group_by(instance_id) %>%
    summarise(duration = sum(diff_min, na.rm = TRUE))

  audit_proof_base <- list(audit_proof_base)

  #This overview includes exclusions related to screen-time.
  audit_proof_advanced <- audit_special %>%
    filter(instance_id %in%
             filter(dat, consent == 1 & consent2 == 1 &
                      !(atmosphere_uncomf == 1 & atmosphere_interfered == 1))$instance_id) %>%
    filter(!(diff_min > max & grepl("Q.+", node))) %>%
    group_by(instance_id) %>%
    summarise(duration = sum(diff_min, na.rm = TRUE)) %>%
    filter(duration > 22)

  audit_proof_advanced <- list(audit_proof_advanced)

  #Combine into a nice list
  audit_details <- c(audit_proof_base, audit_proof_advanced)
  names(audit_details) <- c("Valid_General", "Valid_Screentime")

  return(audit_details)
}

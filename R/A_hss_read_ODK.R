#' Load audit files for HSS survey (ODK)
#'
#'
#' This function is used to load individual audit files for a HSS survey as a
#' single dataframe.
#'
#' @param dat Audit CSV produced by ODK export
#'
#' @return a dataframe that can be used to generate an audit report
#' @export
#'
#' @rdname A_hss_read_ODK
#'
A_hss_read_ODK <- function(audit_dat){
  audit_2 <- subset(audit_dat, select = c(event, node, start, end, instance.ID))
  names(audit_2)[5] <- "instance_id"
  return(audit_2)
}

#' Convert DeviceID to Name Reference
#'
#' This function is used in both the audit (enumerator assessmet) procedure,
#' and the daily report procedure. Numerical deviceids are converted to names.
#' This is a temporary function, as it still requires significant user
#' input. Improvements to this fucntion would involve reading pair-information
#' from an xls sheet.
#'
#' @param dat Dataframe (either audit or daily report)-oriented
#' @param id Numerical deviceid provided by ODK/KOBO
#' @param name Actual name of enumerator
#'
#' @return a modified dataframe that includes the actual names of enumerators.
#' @export
#'
#' @rdname A_hss_id

A_hss_id <- function(dat, id, name){
  dat$deviceid[dat$deviceid == id] <- name
  return(dat)
}

#' Add duration columns for audit + filters
#'
#' This fuction applis some basic cleaning (currently only adds a new column)
#' containing the duration spent on a screen.
#' @param dat The resulting dataframe from hss_load_audit
#' @return A new column with screen duration
#' @export
#'
A_hss_clean_audit <- function(dat) {
  out <- dat %>% dplyr::mutate(
    #This division is conducted to manage the scientific notation used by ODK when
    #outputting audit files
    diff_sec = (end - start) / 1000,
    diff_min = diff_sec / 60
  )
  return(out)
}


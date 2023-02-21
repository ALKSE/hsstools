#' Add duration columns for audit + filters
#'
#' This fuction applis some basic cleaning (currently only adds a new column)
#' containing the duration spend on a screen.
#' @param dat The resulting dataframe from hss_load_audit
#' @return
#' @export
#'
A_hss_clean_audit <- function(dat) {
  out <- dat %>% mutate(
    diff_sec = (end - start) / 1000,
    diff_min = diff_sec / 60
  )
  return(dat)
}

#' Load audit files for HSS survey
#'
#' These functions are used to load individual audit files for a HSS survey as a
#' single dataframe and apply some basic cleaning (currently only adds a new column
#' containing the duration spend on a screen.
#'
#' @param path Path to a folder containing the audit .csv files
#' @param dat The resulting dataframe from hss_load_audit
#' @return
#' @export
#'
A_hss_load_audit <- function(path) {
  filenames <- list.files(path = path, full.names = TRUE)

  dat <- lapply(filenames, function(x) {
    readr::read_csv(x) %>%
      dplyr::mutate(instance_id = x)
  }) %>%
    bind_rows()
  # remove file path from uuid values
  dat$instance_id <- gsub(".+uuid_", "uuid:", dat$instance_id) %>%
    gsub(".csv", "", .) %>%
    gsub("_", "-", .)
  return(dat)
}

#' @rdname A_hss_load_audit
hss_clean_audit <- function(dat) {
  out <- dat %>% mutate(
    diff_sec = (end - start) / 1000,
    diff_min = diff_sec / 60
  )
  return(dat)
}

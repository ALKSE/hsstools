#' Load audit files for HSS survey (KOBO)
#'
#' This function is used to load individual audit files for a HSS survey as a
#' single dataframe.This is likely a redundant function as it relies on
#' KOBO.
#'
#' @param path Path to a folder containing the audit .csv files
#' @return data-frame with all audit files
#' @export
#'
#' @rdname A_hss_load_audit_KOBO
#'
A_hss_load_audit_KOBO <- function(path) {
  filenames <- list.files(path = path, full.names = TRUE)

  dat <- lapply(filenames, function(x) {
    readr::read_csv(x) %>%
      dplyr::mutate(instance_id = x)
  }) %>%
    dplyr::bind_rows()
  # remove file path from uuid values
  dat$instance_id <- gsub(".+uuid_", "uuid:", dat$instance_id) %>%
    gsub(".csv", "", .) %>%
    gsub("_", "-", .)
  return(dat)
}

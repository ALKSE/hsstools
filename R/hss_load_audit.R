#' Load audit files for HSS survey
#'
#' These functions are used to load individual audit files for a HSS survey as a
#' single dataframe and apply some basic cleaning. Can optionally add a column with new
#' variable names based on the relevant XLS form.
#'
#' @param path Path to a folder containing the audit .csv files
#' @param dat The resulting dataframe from hss_load_audit
#' @param xlsform path to the XLS form for this particular HSS survey. If not provided,
#' the function skips the step of renaming variables.
#' @return
#' @export
#'
hss_load_audit <- function(path) {
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

#' @rdname hss_load_audit
hss_clean_audit = function(dat, xlsform = NULL) {
  dat = dat %>%
    .clean_audit()
  if(!is.null(xlsform)){
    dat = dat %>%
      .clean_audit_newnames(xlsform)
  }
  return(dat)
}
#' @keywords internal
.clean_audit <- function(dat) {
  out <- dat %>% mutate(
    diff_sec = (end - start) / 1000,
    diff_min = diff_sec / 60
  )
  return(out)
}
#' @keywords internal
.clean_audit_newnames = function(dat, xlsform) {
  mapping <- .load_xlsform(xlsform) %>%
    .create_mapping()
  out = dat %>% mutate(
  node = gsub(".+/(?=Q.+\\b)", "", node, perl = TRUE),
  node_new = .apply_mapping(node, .env$mapping)
  )
  return(out)
}

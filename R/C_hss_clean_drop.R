#' Drop HSS columns and rows
#'
#' This function is used to filter rows with invalid surveys from the HSS dataset.
#' Additionaly, columns containing notes & instructions to enumerators are removed
#' from the dataset.
#'
#' @param dat The dataframe you want to remove entries from
#'
#'
#' @return Returns a dataframe with invalid surveys removed as well as unnecessary columns.
#' @export
#'
C_hss_clean_drop <- function(dat) {
  dat <- dat %>%
    hss_filter_rows() %>%
    hss_select_cols()
  return(dat)
}

#' @rdname C_hss_clean_drop
hss_filter_rows <- function(dat) {
  # Filter duplicates
  # # (optional) detect and log duplicate entries
  # log_dup = duplicated(dat$instance_id) | duplicated(dat$instance_id, fromLast = TRUE)
  dat <- dat[!duplicated(dat$instance_id), ]

  return(dat)
}

#' @rdname C_hss_clean_drop
hss_select_cols <- function(dat) {
  # drop notes & instructions to enumerators
  dat <- dat[, -grep("\\bintro|\\bnote_|\\bmodule_", names(dat))]

  # drops 'uuid' column (user ID? same for each row)
  dat <- dat[, !names(dat) %in% "uuid"]
  # drop '*_all' cols
  #dat <- dat[, -grep("_all\\b", names(dat))]
  # drop '_whynot' cols if empty (not sure if we want this)
  #dat <- .drop_whynot_empty(dat)

  # drop metadata cols
  meta = c("__version__", "audit", "kry", "isvalidated")
  dat = dat[ , !(names(dat) %in% meta)]

  # drop 100% questions (alt answers are usually filtered out)
  details = c("enterhh", "enterhh_whynot", "enterhh_whynot_oth_what",
              "interview", "interview_whynot",
              "interview_whynot_oth_what", "available",
              "available_whynot", "available_whynot_oth_what",
              "consent_whynot", "consent_whynot_oth_what")
  dat = dat[ , !(names(dat) %in% details)]

  return(dat)
}

#' @keywords internal
.drop_whynot_empty <- function(dat) {
  # remove '_whynot' cols if empty
  # runs inside hss_select_cols()
  whynot <- grep("_whynot\\b", names(dat))
  empty <- sapply(names(dat)[whynot], function(x) {
    {
      length(is.na(dat[[x]])) == length(dat[[x]])
    }[TRUE]
  })
  dat <- dat[, !(names(dat) %in% empty)]
  return(dat)
}

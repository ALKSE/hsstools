#' Drop HSS columns and rows
#'
#' This function is used to filter rows with invalid surveys from the HSS dataset.
#' Additionaly, columns containing notes & instructions to enumerators are removed
#' from the dataset. The function prompts user for input to remove test rows. Set
#' `prompt` to FALSE to skip this step.
#'
#' @param dat The dataframe you want to remove entries from
#' @param prompt If TRUE, prompts user to provide criteria for removing test entries.
#' Input can be row numbers, a cut-off date, or device IDs. Accepts multiple criteria.
#' Set to FALSE to skip this prompt.
#'
#' @return Returns a dataframe with invalid surveys removed as well as unnecessary columns.
#' @export
#'
hss_clean_drop <- function(dat) {
  dat <- dat %>%
    hss_filter_rows() %>%
    hss_select_cols()
  return(dat)
}

#' @rdname hss_clean_drop
hss_filter_rows <- function(dat) {
  # Filter duplicates
  # # (optional) detect and log duplicate entries
  # log_dup = duplicated(dat$instance_id) | duplicated(dat$instance_id, fromLast = TRUE)
  dat <- dat[!duplicated(dat$instance_id), ]

  # filter no consent
  dat <- dat[dat$consent == 1 & dat$consent2 == 1, ]
  # sanity check: warn if 2x consent but no interview held
  if (TRUE %in% grepl(2, dat[, c("enterhh", "interview", "available")])) {
    warning("Informed consent but no full survey for some respondents")
  }

  # drop based on survey duration criteria
  # this is probably better done in seperate script

  # uncomfortable atmosphere & interference from HH member
  dat <- dat[dat$atmosphere_uncomf == 0 & dat$atmosphere_interfered == 0,]
  # short survey duration + uncomfortable/interference/interrupted

  return(dat)
}

#' @rdname hss_clean_drop
hss_select_cols <- function(dat) {
  # drop notes & instructions to enumerators
  dat <- dat[, -grep("\\bintro|\\bnote_|\\bmodule_", names(dat))]

  # drops 'uuid' column (user ID? same for each row)
  dat <- dat[, !names(dat) %in% "uuid"]
  # drop '*_all' cols
  dat <- dat[, -grep("_all\\b", names(dat))]
  # drop '_whynot' cols if empty (not sure if we want this)
  dat <- .drop_whynot_empty(dat)

  # drop metadata cols
  meta = c("__version__", "audit", "kry", "isvalidated")
  dat = dat[ , !(names(dat) %in% meta)]

  # drop area cols? This seems to differ between collection rounds
  area = c("area_mc", "payam_mc", "payam_mc_other", "boma_text")
  dat = dat[, !(names(dat) %in% area)]
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

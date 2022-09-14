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
hss_clean_drop <- function(dat, prompt = TRUE) {
  dat <- dat %>%
    hss_filter_rows(prompt = prompt) %>%
    hss_select_cols()
  return(dat)
}

#' @rdname hss_clean_drop
hss_filter_rows <- function(dat) {
  # Filter duplicates
  # # (optional) detect and log duplicate entries
  # log_dup = duplicated(dat$unique_id) | duplicated(dat$unique_id, fromLast = TRUE)
  dat <- dat[!duplicated(dat$unique_id), ]

  # filter no consent
  dat <- dat[consent == 1 & consent2 == 1, ]
  # sanity check: warn if 2x consent but no interview held
  if (TRUE %in% grepl(2, dat[, c("enterhh", "interview", "available")])) {
    warning("Informed consent but no full survey for some respondents")
  }
  # Filter test entries (requires user input)
  dat <- .remove_test_entries(dat)

  # drop based on survey duration criteria
  # this is probably better done in seperate script

  # uncomfortable atmosphere & interference from HH member
  dat <- dat[dat$atmosphere_uncomf == 1 & dat$atmosphere_interfered == 1]
  # short survey duration + uncomfortable/interference/interrupted

  return(dat)
}

#' @rdname hss_clean_drop
hss_select_cols <- function(dat) {
  # drop notes & instructions to enumerators
  dat <- dat[, -grep("\\bIntro_|\\bNote_|\\bModule_", names(dat))]

  # drops 'uuid' column (user ID? same for each row)
  dat = dat[, - "uuid"]
  # drop '*_all' cols
  dat <- dat[, -grep("_all\\b", names(dat))]
  # drop '_whynot' cols if empty (not sure if we want this)
  dat <- .drop_whynot_empty(dat)

  # drop metadata cols
  meta = c("__version__", "audit", "KEY", "isValidated")
  dat = dat[ ,-meta]


  # drop area cols? This seems to differ between collection rounds
  area = c("area_mc", "payam_mc", "payam_mc_other", "boma_text")
  dat = dat[, -area]
  return(dat)
}

#' @keywords internal
.remove_test_entries <- function(dat, prompt = prompt) {
  # function to remove test entries. prompts user for row input or cutoff date
  # does not work for dates yet
  # runs inside hss_filter_rows()
  if (prompt == TRUE) {
    loop <- 1

    while (loop == 1) {
      test_entries <- menu(c("By row number", "By date", "By Device ID"),
        title = "How do you want to filter test entries"
      )

      if (test_entries == 1) {
        exclude <- readline("What are the rows you want to exclude? ") %>%
          strsplit(" |,|, ") %>%
          unlist() %>%
          as.numeric()
        dat <- dat[-exclude, ]
      }

      if (test_entries == 2) {
        exclude <- readline("What is the cutoff date for exclusion? (dd-mm-yyyy) ") %>%
          as.Date("%d-%m-%y")
        # requires appropriate date-formatted columns
        dat <- dat
      }

      if (test_entries == 3) {
        exclude <- readline("What are the device IDs to exclude? ")
        strsplit(" |,|, ") %>%
          unlist() %>%
          dat() <- dat[dat$deviceid != exclude, ]
      }
      # Choosing 'yes' here will continue the loop and allows for additional filtering
      loop <- menu(c("Yes", "No"),
        title = "Filter further by other criteria?"
      )
    }
  }
  return(dat)
}

#' @keywords internal
.drop_whynot_empty <- function(dat) {
  # remove '_whynot' cols if empty
  # runs inside hss_select_cols()
  whynot <- grep("_whynot\\b", names(dat))
  empty <- sapply(names(dat)[whynot], function(x) {
    {
      length(is.na(data[[x]])) == length(data[[x]])
    }[TRUE]
  })
  dat <- dat[, -names(empty)]
  return(dat)
}

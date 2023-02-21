#' Rename HSS Survey variables
#'
#' This function is used to rename the variables in an HSS dataset from the 'Q01'
#' format to more human-readable variable names. This is done based on the XLS form
#' for that particular survey round. The XLS form needs to have 'r_name' columns
#' in both the question & response option sheets for this to work.
#'
#' @param dat the dataframe containing the HSS survey
#' @param xlsform File path to the XLS form for this particular HSS survey.
#'
#' @return Returns the dataframe with updated column names
#' @export
C_hss_rename <- function(dat, dict) {
  mapping <- .create_mapping(dict)
  names(dat) <- .apply_mapping(names(dat), mapping)
  return(dat)
}

#' @keywords internal
.load_xlsform <- function(file) {
  # Load the XLS form for renaming purposes
  form <- readxl::read_xls(file) %>%
    janitor::clean_names()
  form$type <- gsub("select_one |select_multiple ", "", form$type)

  form_val <- readxl::read_xls(file, sheet = 2) %>%
    janitor::clean_names()

  out <- list(form = form, form_val = form_val)
  return(out)
}

#' @keywords internal
.create_mapping <- function(dict) {
  # Create a mapping of old & new variable names based on provided XLS form. Output
  # is dataframe with columns 'oldvar' and 'newvar'
  merged <- dplyr::right_join(dict$var %>% dplyr::select(type, name, r_name),
    dict$val %>% dplyr::select(list_name, name, r_name),
    by = c("type" = "list_name")
  ) %>% dplyr::filter(!is.na(r_name.y))

  oldname2 <- paste(merged$name.x, merged$name.y, sep = "_") %>% unique()

  newname2 <- paste(merged$r_name.x, merged$r_name.y, sep = "") %>%
    gsub("_all_", "_", .) %>%
    unique()

  oldvar <- c(dict$var$name, oldname2) %>% tolower()
  newvar <- c(dict$var$r_name, newname2) %>% tolower()

  out <- data.frame(oldvar, newvar) %>%
    dplyr::mutate(newvar = dplyr::if_else(is.na(newvar), oldvar, newvar)) %>%
    dplyr::filter(!is.na(oldvar))
  return(out)
}
#' @keywords internal
.apply_mapping <- function(names, mapping) {
  # Replaces old names with new names. Accepts a character string as input. If an
  # element of 'names' is found in the mapping object, it is replaced with its corresponding
  # new name. If not found, the original name is kept.

  # just in case
  names <- janitor::make_clean_names(names)

  names <- sapply(names, function(x) {
    if (length(mapping$newvar[which(mapping$oldvar == x)]) == 0) {
      x
    } else {
      mapping$newvar[which(mapping$oldvar == x)]
    }
  })
  return(names)
}

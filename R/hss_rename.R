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
hss_rename <- function(dat, xlsform) {
  mapping <- .load_xlsform(xlsform) %>%
    .create_mapping()
  dat <- .apply_mapping(dat, mapping)
  return(dat)
}
#' @keywords internal
.load_xlsform <- function(file) {
  form <- readxl::read_xls(file) %>%
    janitor::clean_names()
  form$type <- gsub("select_one |select_multiple ", "", form$type)

  form_val <- readxl::read_xls(file, sheet = 2) %>%
    janitor::clean_names()

  out <- list(form = form, form_val = form_val)
  return(out)
}
#' @keywords internal
.create_mapping <- function(list) {
  merged <- right_join(list$form %>% select(type, name, r_name),
    list$form_val %>% select(list_name, name, r_name),
    by = c("type" = "list_name")
  ) %>% filter(!is.na(r_name.y))

  oldname2 <- paste(merged$name.x, merged$name.y, sep = "_") %>% unique()

  newname2 <- paste(merged$r_name.x, merged$r_name.y, sep = "") %>% unique()

  oldvar <- c(list$form$name, oldname2) %>% tolower()
  newvar <- c(list$form$r_name, newname2) %>% tolower()

  out <- data.frame(oldvar, newvar) %>%
    mutate(newvar = if_else(is.na(newvar), oldvar, newvar)) %>%
    filter(!is.na(oldvar))
  return(out)
}
#' @keywords internal
.apply_mapping <- function(dat, mapping) {
  # just in case
  dat = janitor::clean_names(dat)

  names(dat) <- sapply(names(dat), function(x) {
    if (length(mapping$newvar[which(mapping$oldvar == x)]) == 0) {
      x
    } else {
      mapping$newvar[which(mapping$oldvar == x)]
    }
  })
  return(dat)
}

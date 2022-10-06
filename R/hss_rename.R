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
  names(dat) <- .apply_mapping(names(dat), mapping)
  data_nw_names <<- dat
  return(data_nw_names)
}


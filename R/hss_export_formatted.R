#' Exports a formatted list of tables to .docx
#'
#' @param list A list containing flextable objects
#' @param path The file path to save to
#'
#' @return
#' @export
#'
hss_export_formatted <- function(list, path = NULL) {
  # select all successfully written tables
  list <- list[which(sapply(list, function(list_element) !is.null(list_element)))]

  flextable::save_as_docx(values = list, path = path)
}

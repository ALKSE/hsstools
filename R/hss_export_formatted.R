#' Exports a formatted list of tables to .docx
#'
#' Exports the list of formatted tables created by `hss_write_formatted()` to a
#' .docx file in the specified location. One thing to keep in mind: the duration
#' of this process increase exponentially with the number of tables to write. Exporting
#' around 200 tables could take up to 10 minutes.
#'
#' @param list A list containing flextable objects
#' @param file The file path to save to
#'
#' @return Outputs the selected flextable objects to a .docx file.
#' @export
hss_export_formatted <- function(list, file = NULL) {
  # select all successfully written tables
  t1 <- Sys.time()
  list <- list[which(sapply(list, function(list_element) !is.null(list_element)))]

    flextable::save_as_docx(values = list, path = file)

  t2 <- Sys.time()
  cat("Files succesfully written to ", file, " in ", round(t2 - t1, 3), " seconds.")
}

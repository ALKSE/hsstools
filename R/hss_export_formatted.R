#' Exports a formatted list of tables to .docx
#'
#' @param list A list containing flextable objects
#' @param file The file path to save to
#'
#' @return Outputs the selected flextable objects to a .docx file. Depending on the number of tables to export, this may take a long time.
#' NOTE: running this function while the chosen output file is opened in another program will cause the R session to abort.
#' @export
hss_export_formatted <- function(list, file = NULL, type = "word") {
  # select all successfully written tables
  t1 <- Sys.time()
  list <- list[which(sapply(list, function(list_element) !is.null(list_element)))]

  if (type == "word") {
    flextable::save_as_docx(values = list, path = file)
  }

  if (type == "html") {
    flextable::save_as_html(values = list, path = file)
  }
  t2 <- Sys.time()
  cat("Files succesfully written to ", file, " in ", round(t2 - t1, 3), " seconds.")
}

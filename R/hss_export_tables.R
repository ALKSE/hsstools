#' Exports a list of data tables to CSV
#'
#' Exports a list of HSS tables to CSV. This function uses sink() to write multiple
#' tables to the same file. If an error occurs during execution of the function
#' you may need to close the sink with sink() before proceeding.
#'
#' @param df_list A named list containing the data tables. The output of hss_write_tables() usually.
#' @param path Where to store the .csv output.
#'
#' @return A CSV file stored at the provided location.
#'
#' @seealso hss_write_tables()
#' @export
#'
hss_export_tables <- function(df_list, path) {
  # Select all non-NULL (successfully written) tables from list
  df_list <- df_list[-which(sapply(df_list, is.null))]

  sink(path)

  for (i in names(df_list)) {
    cat(i, strrep(";", ncol(df_list[[i]])), "\n", sep = "")

    write.table(df_list[[i]], sep = ";", dec = ",", row.names = FALSE)

    cat(strrep(";", ncol(df_list[[i]])), "\n", sep = "")
  }

  sink()
}

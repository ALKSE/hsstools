#' Exports a list of data tables to .csv
#'
#' @param df_list A named list containing the data tables. The output of hss_write_tables() usually.
#' @param path Where to store the .csv output.
#'
#' @return
#' @export
#'
hss_export_tables <- function(df_list, path) {
  df_list <- df_list[-which(sapply(df_list, is.null))]
  sink(path)

  for (i in names(df_list)) {
    cat(i, strrep(";", ncol(df_list[[i]])), "\n", sep = "")

    write.table(df_list[[i]], sep = ";", dec = ",", row.names = FALSE)

    cat(strrep(";", ncol(df_list[[i]])), "\n", sep = "")
  }

  sink()
}

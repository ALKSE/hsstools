#' Append all data files stored in the same folder + file name
#'
#' @param source_folder The folder in which the to-be appended data files are stored
#' @param destination_folder The folder in which the appended data file should be stored
#' @param destination_file The name of the appended data file
#' @param file_pattern Set to ".csv" when appending CSV files
#' @param file_separator Set to "," when appending CSV files
#'
#' @return A CSV file containing the contents of all source files, incl. a new column containing the file names of the source files. NOTE: If the source files contain column headers, the appended file will contain the header row of each source file
#' @export
#'
append_tables <- function (source_folder, destination_folder, destination_file, file_pattern, file_separator) {
  setwd(source_folder)
  list_audit_files <- list.files(path = source_folder, pattern = file_pattern)
  temp_new <- Map(cbind, lapply(list_audit_files, data.table::fread, sep = file_separator), filename = list_audit_files)
  setwd(destination_folder)
  file.remove(destination_file)
  lapply(temp_new, function(x) write.table(data.frame(x), destination_file, append= TRUE, sep=',', quote = FALSE, row.names = FALSE ))
  rm(temp_new, list_audit_files)
}

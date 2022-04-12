#' Write tables to a list for selected variables
#'
#' @param df The dataframe containing relevant variables
#' @param questions A named character vector containing variable names and the table type required.
#' @param group The desired grouping/disaggregation variable.
#'
#' @return A named list of dataframes.
#' @export
#'
hss_write_tables <- function(df, questions, group) {
  output_list <- lapply(questions, function(x) {
    tryCatch(

      if (names(questions[match(x, questions)]) == "select_one") {
        x <- hss_table_single(df, x, group)
      } else if (names(questions[match(x, questions)]) == "select_multiple") {
        x <- hss_table_multi(df, x, group)
      },
      error = function(e) NULL
    )
  })

  names(output_list) <- hss_lookup_list(questions, reverse = TRUE)

  return(output_list)
}

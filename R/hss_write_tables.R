#' Write tables to a list for selected variables
#'
#' @param df The dataframe containing relevant variables
#' @param questions A named character vector containing variable names and the table type required.
#' @param group The desired grouping/disaggregation variable.
#'
#' @return A named list of dataframes.
#' @export
#'
hss_write_tables <- function(df, questions, group, percent = TRUE) {
  output_list <- lapply(questions, function(x) {
    tryCatch(

      if (names(questions[match(x, questions)]) == "select_one") {
        x <- rbind(
          hss_table_single(df, x, group, percent = percent),
          c("Pval", hss_chisq(df, hss_lookup_list(x, TRUE), group, full = FALSE))
        )
      } else if (names(questions[match(x, questions)]) == "select_multiple") {
        x <- hss_table_multi(df, x, group, percent = percent)
      },
      error = function(e) NULL
    )
  })

  names(output_list) <- hss_lookup_list(questions, reverse = TRUE)

  return(output_list)
}

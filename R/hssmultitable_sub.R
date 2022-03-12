#' A specific implementation of hssmultitable for sub-questions
#'
#' @param df The dataframe containing the multiresponse questions
#' @param question The variable containing the sub-setting question
#' @param valid A character string containing responses that should be included in the contingency table
#' @param resp A character string of all response variables to include
#' @param group A grouping (or disaggregation) variable.
#' @param percent Set to TRUE to show percentages. Set to FALSE to show counts
#'
#' @return
#' @export
#'
#' @examples
#'data <-testdata()
#' hssmultitable_sub(df = data,
#'                   question = "multiresponse",
#'                   valid = "Yes",
#'                   resp = c("multi_option1", "multi_option2", "multi_option3"),
#'                   group = "location",
#'                   percent = TRUE)
hssmultitable_sub <- function(df, question, valid, resp, group, percent = TRUE) {
  total <- if(percent == TRUE) {"mean"} else {"sum"}
  addmargins(
    cross.multi.table(df[
      df[[question]] %in% valid,
      resp
    ],
    crossvar = df[df[[question]] %in% valid, ][[group]],
    freq = percent, tfreq = "col"
    ),
    margin = 2,
    FUN = total
  )
}

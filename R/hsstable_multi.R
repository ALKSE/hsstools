#' Generate contingency table for multiresponse questions
#'
#' @param df The dataframe containing the multiresponse questions
#' @param resp A character string of all response variables to include
#' @param group A grouping (or disaggregation) variable.
#' @param percent Set to TRUE to show percentages. Set to FALSE to show counts
#'
#' @return A contingency table containing the multiresponse answers and a grouping variable
#' @export
#'
#' @examples
#' data <- testdata()
#' hssmultitable(data, c("multi_option1", "multi_option2", "multi_option3"), "gender")
hsstable_multi <- function(df, resp, group, percent = TRUE) {
  if (is.factor(df[[group]]) == FALSE) {
    warning("Missing levels may not appear in table")
  }

  total <- if (percent == TRUE) {
    "mean"
  } else {
    "sum"
  }

  x <- addmargins(
    questionr::cross.multi.table(df[!is.na(resp[1]), resp],
                                 crossvar = df[!is.na(resp[1])][[group]],
                                 freq = percent,
                                 tfreq = "col",
                                 n = FALSE,
                                 na.rm = TRUE
    ),
    margin = 2,
    FUN = total
  )
  p <- hsschisq(df, resp, group, full = FALSE)
  x <- cbind(
    "Answer" = rownames(x),
    as.data.frame.matrix(x, row.names = NULL),
    p
  )
  x
}

#' @param question The variable containing the sub-setting question
#' @param valid A character string containing responses that should be included in the contingency table
#' @rdname hsstable_multi

hsstable_multi_sub <- function(df, question, valid, resp, group, percent = TRUE) {
  if (is.factor(df[[group]]) == FALSE) {
    warning("Grouping variable is not a factor. Missing levels may not appear in table")
  }

  total <- if (percent == TRUE) {
    "mean"
  } else {
    "sum"
  }

  addmargins(
    questionr::cross.multi.table(df[
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

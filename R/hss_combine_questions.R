#' Combine multiple questions with the same response options
#'
#' @param df The dataframe containing the questions
#' @param var The shared element in all of the question/variable names
#'
#' @return This returns a dataframe with the response options and the number of responses for that option.
#' @export
#'
#' @rdname hss_combine_questions
hss_combine_single <- function(df, var) {
  dat <- df %>%
    dplyr::select(contains(var)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "answers") %>%
    na.exclude()

  dat[1] <- stringr::str_replace_all(
    unlist(dat[1]),
    paste("\\w+_(?=", var, ")", sep = ""),
    ""
  )

  dat <- dat %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(count = n())
  return(dat)
}

#' @rdname hss_combine_questions
hss_combine_multi <- function(df, var) {
  dat <- df %>%
    dplyr::select(contains(var) & !contains("what")) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "answers"
    ) %>%
    na.exclude()

  dat[1] <- stringr::str_replace_all(
    unlist(dat[1]),
    paste("\\w+_(?=", var, ")", sep = ""),
    ""
  )

  dat <- dat %>%
    dplyr::group_by(answers) %>%
    dplyr::summarise(count = sum(value))
  return(dat)
}

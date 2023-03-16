H_TEST_FUN <- function(df, questions, group, dict, percent = TRUE, digits = 1, lang = "en") {
  out <- lapply(questions, function(questions_element) {
    tryCatch(
      if (names(questions[match(questions_element, questions)]) == "select_one") {
        T_hss_table_single(df, questions_element, group, percent, digits) %>%
          T_hss_label(questions_element, group, dict, lang) %>%
          T_hss_format_single() %>%
          flextable::add_footer_lines(
            values = T_hss_chisq_formatted(df, questions_element, group)
          )
      },
      error = function(e) NULL
    )
  })
  # set names to appropriate question labels
  names(out) <- sapply(
    questions,
    function(questions_element) {
      .get_dict_varname(
        questions_element,
        "r_name",
        paste0("r_table_label_", lang),
        dict
      )
    }
  )

  return(out)
}

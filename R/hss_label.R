#' Apply text labels to tables.
#'
#' Apply text labels to the specified table. Labels are taken from the dictionaries
#' created from the XLS forms. This converts the table to a flextable object. Works for EN or AR text labels.
#' If used with AR text labels some optional formatting is applied to correctly display text.
#'
#' @param table the table for which labels should be applied
#' @param var the variable name. Used to determine the question label and to look up the appropriate
#' response labels.
#' @param grouping the grouping variable. Will be used to apply appropriate column headers
#' @param language To determine the language of labels to be applied.
#'
#' @return A flextable object with the original table values and appropriate question & response labels.
#' @export
#'
hss_label <- function(table, var, grouping, lang = "en") {
  # check if table was already converted to flextable object
  if(!inherits(table, "flextable")) {
    table <- flextable::flextable(table)
  }
  # retrieve formatting options
  format <- .get_format_options()
  # retrieve question and answer labels
  labels <- .get_table_labels(var, lang)
  # retrieve header/grouping labels & exclude "refuse to answer" and other missing values
  group_labs <- .get_table_labels(grouping, lang)
  exclude <- .get_dict_valname(grouping, "list_name", "name") %>% stringr::str_which("99")
  if(length(exclude)>0) {
  group_labs$answers <- group_labs$answers[-exclude]
  }
# set proper EN/AR labels
  newlabels <- list(
    Response = "",
    group_labs$answers,
    Total = paste0("format$total_", lang) %>% str2lang() %>% eval(),
    p = "p"
  ) %>%
    purrr::flatten()
# replace the column label with appropriate EN/AR label while keeping the (N = ...) part
  header_labels <- lapply(1:length(newlabels), function(x) {
    stringr::str_replace(table$col_keys[x], ".+\n", paste0(newlabels[x], "\n"))
  })
names(header_labels) <- table$col_keys

  # apply labels
  table <- table %>%
    flextable::set_header_labels(values = header_labels) %>%
    flextable::mk_par(j = "Response", value = flextable::as_paragraph(
      flextable::as_chunk(labels$answers)
    )) %>%
    flextable::set_header_labels(Response = "") # %>%
  # flextable::add_header_lines(values = labels$question)

  # optional formatting for AR
  if(lang == "ar") {
  table <- table %>%
      flextable::align_text_col(align = "right")
  }

  return(table)
}
#' helper function for hss_label function. Don't run separately.
#' @keywords internal
.get_table_labels <- function(var, lang) {
  # check for language input and define label column names
  if(!(lang == "en" | lang == "ar")) stop("invalid language input ", lang, ". Use \'en\' or \'ar\'.")
  label_lang <- paste0("r_table_label_", lang)

  # look up variable name in "r_name" column and return value from "r_table_label_[language]"
  question <- var %>% .get_dict_varname("r_name", label_lang)
  # look up "type" value for variable name. looks up "type" value in dict_val "list_name
  # and returns matching value labels from "r_table_label_[language]
  answers <- var %>% .get_dict_varname("r_name", "type") %>%
    .get_dict_valname("list_name", label_lang)
  labels <- list(question = question, answers = answers)
  return(labels)
}

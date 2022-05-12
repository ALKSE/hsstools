#' Apply text labels to tables.
#'
#' @param table the table
#' @param var the variable name
#'
#' @return
#' @export
#'
hss_label <- function(table, var) {
  # retrieve question and answer labels
  labels <- .get_table_labels(var)
  # check if table was already converted to flextable object
  if(!inherits(table, "flextable")) {
    table <- flextable::flextable(table)
  }
  # apply labels
  table <- table %>%
    add_header_lines(values = labels$question) %>%
    mk_par(j = var, value = as_paragraph(
      as_chunk(labels$answers)
    ))
  return(table)
}
#' helper function for hss_label function. Don't run separately.
#' @keywords internal
.get_table_labels <- function(var, lang = "en") {
  # check for language input and define label column names
  if(!(lang == "en" | lang == "ar")) stop("invalid language input ", lang, ". Use \'en\' or \'ar\'.")
  var_lang <- paste0("r_table_label_", lang)
  val_lang <- paste0("r_table_label_", lang, "_column")

  # look up variable name in "r_name" column and return value from "r_table_label_en"
  question <- var %>% .get_dict_varname("r_name", var_lang)
  # look up "type" value for variable name. looks up "type" value in dict_val "list_name
  # and returns matching value labels from "r_table_label_en_columns
  answers <- var %>% .get_dict_varname("r_name", "type") %>%
    .get_dict_valname("list_name", val_lang)
  labels <- list(question = question, answers = answers)
  return(labels)
}

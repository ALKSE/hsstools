#' Label all your named variables
#'
#'This function generates labels for named variables. A prerequisite is ensuring that
#'variables have been already named using hss_rename.
#' @param data actual survey data
#' @param survey the master xls form -character-
#' @param dictionary the dual list dictionary that should be generated early in the cleaning process
#'
#' @rdname hss_VarLabel
#' @export
#'
C_hss_VarLabel <- function(data, survey, dictionary) {
  form <- readxl::read_excel(survey)
  dic_val <- dictionary$val
  labels_1 <- subset(form, select = c(`label::English`, R_name))
  labels_2 <- subset(dic_val, select = c(label_english, r_name))
  names(labels_1)[1] <- "label_english"
  names(labels_1)[2] <- "r_name"
  all_label <- rbind(labels_1, labels_2)

  vector_1 <- all_label$label_english
  vector_2 <- all_label$r_name
  combined <- setNames(vector_1, vector_2)
  Hmisc::label(data) <- as.list(combined[match(names(data), names(combined))])
  return(data)
}



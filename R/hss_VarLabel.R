#' Label all your named variables
#'
#'This function generates labels for named variables. A prerequisite is ensuring that
#'variables have been already named using hss_rename.
#' @param data actual survey data
#' @param survey the master xls form -character-
#' @param location the location of the master xls form -path-
#'
#' @rdname hss_VarLabel
#' @export
#'
hss_VarLabel <- function(data, survey, location) {
  form <- readxl::read_excel(survey)
  dic_val_new <- hss_create_dict(survey, "val", location = location)
  labels_1 <- subset(form, select = c(`label::English`, R_name))
  labels_2 <- subset(dic_val_new, select = c(label_english, r_name))
  names(labels_1)[1] <- "label_english"
  names(labels_1)[2] <- "r_name"
  all_label <- rbind(labels_1, labels_2)

  vector_1 <- all_label$label_english
  vector_2 <- all_label$r_name
  combined <- setNames(vector_1, vector_2)
  Hmisc::label(data) <- as.list(combined[match(names(data), names(combined))])
  return(data)
}



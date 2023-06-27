#' Generate a new HSS variable
#'
#' Update the dataset to include a new variable and generate a doc that will be
#' a necessary input in the following function which adds this new variable to
#' the dictionary. It is important to keep in mind that this will not change the
#' actual XLS form used to generate the dictionary. This is an internal
#' modification.
#'
#' @param dat HSS dataframe
#' @param name the name of the new variable (var_specific)
#' @param location the name of the variable that should come before the new var
#'
#' @return HSS dataframe which includes the new variable + a doc to be used for
#' the subsequent update of the dictionary
#' @export
#'
#' @rdname C_hss_add_var

C_hss_add_var <- function(dat, name, location){
  #Adding the new variable to the dataset
  new_var <- as.data.frame(1:nrow(dat))
  names(new_var)[1] <- name
  new_var[1] <- 0
  dat_2 <- cbind(dat, new_var)
  dat_3 <- dat_2 %>% dplyr::relocate(name, .after = as.character(location))

  #Generating a doc for input to secondary function to update dictionary
  value_matrix <- matrix(1, nrow = 1, ncol = 6)
  colnames(value_matrix) <- c("variable_name", "value_number", "r_name",
                              "english_label", "arabic_label", "limit")
  value_matrix <- as.data.frame(value_matrix)
  writexl::write_xlsx(value_matrix, path = "new_var.xls")

  return(dat_3)
}

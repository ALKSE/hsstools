#' Split select_multiple survey variables
#'
#' BASIC: Split select_multiple variables into their individual components,
#' with consideration for proper naming, conversation of 0s to NAs, and later
#' reordering of select_multiple components.
#' PREP: Run C_hss_create_dict, C_hss_rename
#' NEXT: Run C_hss_relocate_multi
#'
#'
#' @param data A dataframe that has been cleaned minimally - only C_hss_rename.
#' @param dict The dictionary reference that will be used to collect the
#' names associated with each variable.
#'
#' @return A dataframe that splits select_multiple variables into their components
#' @export
#'
#'
#' @rdname C_hss_split_multi
C_hss_split_multi <- function(data, dict){

  split_3 <- function(i){
    data <- first[[i]]
    split_2(data, dict)
  }

  q <- 1:nrow(data)
  first <- lapply(q, FUN = function(i) as.data.frame(x <- data[i,]))
  second <- lapply(q, split_3)
  third <- data.table::rbindlist(second, use.names = TRUE, fill = TRUE)
  return(third)
}


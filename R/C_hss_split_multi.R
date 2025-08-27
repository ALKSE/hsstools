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
  #This function seeks to convert rows into a list of dfs for more efficient processing (each row becomes a df)
  first <- lapply(q, FUN = function(i) as.data.frame(x <- data[i,]))

  # Use purrr::map instead of lapply to add progress bar to the function. Investigate utility over time as purr::map can be slower than lapply
  #second <- lapply(q, split_3)

  second <- purrr::map(q, split_3, .progress = TRUE)
  third <- data.table::rbindlist(second, use.names = TRUE, fill = TRUE)
  return(third)
}


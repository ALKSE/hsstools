#' Fix recode mismatches through text replacement(s)
#'
#' This function is a follow-up to the recode_check function, and seeks to correct NA text
#' mismatches. The goal of this function is to generate an updated recode_file that can be shoved
#' back into the cleaning process. In premise, this function replaces broken recode text with
#' updated text that is recognized as part of the OG DF.
#'
#' @param recode_checked recode_file generated after running the recode_check function
#' @param prep code_prep file generated when filtering out _oth text at start of process
#'
#' @return A new recode file that is 'fixed' and can be inputted to the rewrite function
#' @rdname C_hss_recode_fix
#' @export
#'
C_hss_recode_fix <- function(recode_checked, prep){
  #Generate a list of NA values that were not recoded
  recode_null <- recode_checked %>% filter(is.na(Sanity))

  #Add a validation object to the global environment for comparison
  recode_null_check <<- recode_checked %>% filter(is.na(Sanity))

  #Looping logic
  num <- as.list(1:nrow(recode_null))

  for (i in num){
    #Select first row
    null_a <- recode_null[i,]
    #Filter original text_recode document to match new text (modified)
    prep_a <- prep %>% filter(prep[[1]] == null_a[[1]][1], prep[[6]] == null_a[[6]][1])
    #Replace text
    recode_null[[2]][i] <- prep_a[[2]][1]
    output <- recode_null
  }
  #Additional Sanity check to ensure that text was replaced correctly
  recode_fix_check <<- cbind(output[2], recode_null_check[2])
  return(output)
}

#' Check if recode process is successful
#'
#' This function aims to review the updated DF (post-recode) to check if all values were replaced
#' correctly. The output will indicate successful changes, and NA where text was not matched.
#'
#' @param dat Updated df that is generated after initial recode
#' @param recode_file Recode file to be used as reference for updates
#' @return A update object (recode_format) with indicators on successful and failed matches
#' @rdname C_hss_recode_check
#' @export
#'
C_hss_recode_check <- function(dat, recode_file){

  #Create an updated file to contain the new values.
  #Sanity checks if the main DF was updated
  #Match checks if recode values match values in the main DF.
  recode_3_file <- recode_file %>% mutate(Sanity = "", Match = "")
  num <- as.list(1:nrow(recode_file))

  for( i in num){
    #This function operates row-by-row. This step initiates this process and removes empty rows.
    step_1 <- dat %>% select(recode_file[[3]][i], recode_file[[1]][i]) %>% na.omit()

    #This step filters the specific row and adds the corresponding recode value.
    #If a match is not made based on the filter, then NA is displayed.
    step_2 <- step_1 %>% filter(step_1[[2]] == recode_file[[2]][i]) %>% mutate(Recode = recode_file[[4]][i])

    #In case a match is made, the designation of 'TRUE' is added.
    step_3 <- step_2 %>% mutate(Match = step_2[[1]] == step_2[[3]])

    #Values are added to the main recode file to display progress.
    recode_3_file[[7]][i] <- step_3[[3]][1]
    recode_3_file[[8]][i] <- step_3[[4]][1]
    output <- recode_3_file}
  return(output)
}

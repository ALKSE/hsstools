#' Reports & Publications: Age Distriubution
#'
#' @param dat HSS dataframe
#'
#' @return age breakdown for data
#' @export
#'
#'
#' @rdname R_hss_age_count

R_hss_age_count <- function(dat){
  age_rows <- subset(dat, select = age)
  age_16 <- subset(age_rows, age >= 16 & age <= 30)
  age_31 <- subset(age_rows, age >= 31 & age <= 45)
  age_46 <- subset(age_rows, age >= 46 & age <= 65)
  age_66 <- subset(age_rows, age >= 66)
  sixteen <- nrow(age_16)/nrow(age_rows) * 100
  thirtyone <- nrow(age_31)/nrow(age_rows) * 100
  fortysix <- nrow(age_46)/nrow(age_rows) * 100
  sixtysix <- nrow(age_66)/nrow(age_rows) * 100
  age_per <- data.frame(
    range= c("16-30", "31-45", "46-65", "65+"),
    percentage=c(sixteen, thirtyone, fortysix, sixtysix)
  )
  print(age_per)
  print(flextable(age_per))
  return(age_per)
}

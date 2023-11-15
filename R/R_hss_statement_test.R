#' Reports & Publications: Statement Preference (A/B)
#'
#' This function quickly summarizes respondent preferences for a statement (either A or B).
#' Input for the statement is the R_name of the variable, e.g st_securityforces.
#' Function does not require entity to run.
#'
#' @param data HSS dataframe
#' @param var variable name of statement
#'
#' @return a printed summary
#' @export
#'
#' @rdname R_hss_statement_test

R_hss_statement_test <- function(data, var){
  summary(data[[var]])
  A <- summary(data[[var]])[1]
  B <- summary(data[[var]])[2]
  C <- summary(data[[var]])[3]
  D <- summary(data[[var]])[4]
  CD <- C + D

  E <- sum(c(A,B,C,D))
  print(statement1 <- (A/E)*100)
  print(statement2 <- (B/E)*100)
  print(statement3 <- (CD/E)*100)

  source <- subset(dic$var, select = (c(type, label_english,r_name)))
  source_1 <- subset(source, r_name == as.character(var))
  print("                    ", quote = FALSE)
  print(source_1$label_english, quote = FALSE)
}

#' Reports & Publications: Actor Performance
#'
#' This function calculates actor performance.
#' Performance is displayed in console and as a simplified plot.
#' This function does not require an entity.
#'
#' @param data HSS dataframe
#' @param actor variable name relating to actor performance (_Perf)
#'
#' @return A breakdown of actor performance
#' @export
#'
#' @rdname R_hss_act_perf
#'
R_hss_act_perf <- function(data, actor){

  VG <- summary(data[[actor]])[1]
  G <- summary(data[[actor]])[2]
  OK <- summary(data[[actor]])[3]
  NG <- summary(data[[actor]])[4]
  VB <- summary(data[[actor]])[5]
  IDK <- summary(data[[actor]])[6]
  RTA <- summary(data[[actor]])[7]

  TOT <- sum(c(VG, G, OK, NG, VB, IDK, RTA))

  st1 <- "Who is being evaluated"
  st2 <- "How many respondents indicated that this actor is present"
  note_1 <- paste(st1,paste0(": ", actor))
  note_2 <- paste(st2,paste0(": ", TOT))

  print("-----------------------------------------------------------------", quote = FALSE)
  print("#######################|   Actor_perf   |########################", quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)
  print(note_1, quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)
  print(note_2, quote = FALSE)
  print("                         ", quote = FALSE)
  print("                         ", quote = FALSE)


  (TVG <- (VG/TOT)*100)
  (TG <- (G/TOT)*100)
  (TOK <- (OK/TOT)*100)
  (TNG <- (NG/TOT)*100)
  (TVB <- (VB/TOT)*100)
  (TIDK <- (IDK/TOT)*100)
  (TRTA <- (RTA/TOT)*100)


  #performance shorthand
  Just_good <- sum(c(TVG, TG))
  Just_ok <- TOK
  Just_bad <- sum(c(TNG, TVB))
  Just_other <- sum(c(TIDK, TRTA))

  table_basic <- as.data.frame(c(Just_good, Just_ok, Just_bad, Just_other))
  names(table_basic)[1] <- "percentage"
  table_basic$performance <- c("Good", "Bad", "Just Ok", "IDK/RTA")
  table_basic <- table_basic %>% relocate(performance)
  print(flextable(table_basic))


  #detailed presentation of performance
  table <- as.data.frame(c(TVG, TG, TOK, TNG, TVB, TIDK, TRTA))
  names(table)[1] <- "percentage"
  print(table)

}

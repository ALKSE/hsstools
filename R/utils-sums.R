#All functions listed below are run as part of R_hss_sum_values

# inc_satis_why sum function ------------------------------------------------
# Calculate sums on why an incident was resolved in a satisfactory way
# Calculation includes breakdown by type of incident

.hss_sum_satis_why <- function(dat, dictionary){
  #General calculation of all reasons for satis (includes all variables, focus on sums)
  yes_satis_1 <- dat %>% select(matches("_satis_why_caught")) %>% select(!(matches("_oth")))
  yes_satis_2 <- dat %>% select(matches("_satis_why_comp")) %>% select(!(matches("_oth")))
  yes_satis_3 <- dat %>% select(matches("_satis_why_honor")) %>% select(!(matches("_oth")))
  yes_satis_4 <- dat %>% select(matches("_satis_why_revenge")) %>% select(!(matches("_oth")))
  yes_satis_5 <- dat %>% select(matches("_satis_why_return")) %>% select(!(matches("_oth")))
  yes_satis_6 <- dat %>% select(matches("_satis_why_reconcil")) %>% select(!(matches("_oth")))
  yes_satis_7 <- dat %>% select(matches("_satis_why_safer")) %>% select(!(matches("_oth")))
  yes_satis_8 <- dat %>% select(matches("_satis_why_tried")) %>% select(!(matches("_oth")))
  yes_satis_9 <- dat %>% select(matches("_satis_why_idk")) %>% select(!(matches("_oth")))
  yes_satis_10 <- dat %>% select(matches("_satis_why_rta")) %>% select(!(matches("_oth")))

  #Distinction by year
  incidents_2022 <- c("catt", "rob", "prison", "recruit",
                      "kidnap", "assault", "kill", "bomb", "fmarr", "sex")
  incidents_2023 <- c("catt", "rob", "prison", "recruit",
                      "kidnap", "assault", "kill", "bomb", "fmarr", "sex", "narco")
  incidents_names <- list(incidents_2022, incidents_2023)

  #select proper incident collection
  year_list <- function(incidents_names) {
    if (dat$date_simple[1] < "2023-01-01") {naam <- incidents_names[[1]]}
    else if (dat$date_simple[1] > "2023-01-01") {naam <- incidents_names[[2]]}
    return(naam)
  }

  year_inc <- year_list(incidents_names)

  #Calculating the totals on an incident level
  incident_sums <- function(df){
    vals <- NULL
    for (i in 1:ncol(df)){
      temp <- sum(df[[i]])
      vals <- rbind(vals, temp)
    }
    vals_2 <- t(vals)
    colnames(vals_2) <- year_inc
    vals_3 <- as.data.frame(vals_2)
    return(vals_3)
  }


  df_list <- list(yes_satis_1, yes_satis_2, yes_satis_3, yes_satis_4,
                  yes_satis_5, yes_satis_6, yes_satis_7, yes_satis_8,
                  yes_satis_9, yes_satis_10)

  sum_list <- lapply(df_list, incident_sums)
  sum_df <- data.table::rbindlist(sum_list, use.names = TRUE, fill = TRUE)
  #Now joining everything together

  total_satis <- sum(yes_satis_1, yes_satis_2, yes_satis_3, yes_satis_4,
                     yes_satis_5, yes_satis_6, yes_satis_7, yes_satis_8,
                     yes_satis_9, yes_satis_10)

  yes_satis_table_in <- as.data.frame(c(sum(yes_satis_1),sum(yes_satis_2),sum(yes_satis_3),
                                        sum(yes_satis_4),sum(yes_satis_5),sum(yes_satis_6),
                                        sum(yes_satis_7),sum(yes_satis_8),sum(yes_satis_9),
                                        sum(yes_satis_10)))
  names(yes_satis_table_in)[1] <- "sum"
  yes_satis_table <- cbind(sum_df, yes_satis_table_in)


  yes_satis_table$percentage <- (yes_satis_table$sum/total_satis)*100
  yes_satis_table$satis_why_all <- c("The perpetrator was caught and punished",
                                     "Compensation for losses was offered",
                                     "Honor was restored to the family",
                                     "Revenge was taken against the perpetrator",
                                     "Stolen goods, cattle or abducted people were returned",
                                     "Reconciliation with the perpetrator took place",
                                     "I feel safer now generally",
                                     "At least they tried to help me",
                                     "I don't know",
                                     "Refused to answer")

  yes_satis_table <- yes_satis_table %>% dplyr::relocate(satis_why_all)
  ft <- flextable::flextable(yes_satis_table)
  ft <- flextable::autofit(ft)
  print(ft)

  #Small display for later validation (if necessary)
  print("-----------------------------------------------------------------", quote = FALSE)
  print("#################|   (Inc.)_satisfaction   |#####################", quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)

  x <- sum(yes_satis_table$sum)

  st1 <- "As a count (Satisfied)"
  note_1 <- paste(st1,paste0("= ", x))
  print(note_1, quote = FALSE)

  return(yes_satis_table)
}

# inc_satis_whynot sum function ------------------------------------------------
# Calculate sums on why an incident was NOT resolved in a satisfactory way
# Calculation includes breakdown by type of incident

.hss_sum_satis_whynot <- function(dat, dictionary){
  #General calculation of all reasons for no-contact (includes all variables, focus on sums)
  no_satis_1 <- dat %>% select(matches("satis_whynot_caught")) %>% select(!(matches("_oth")))
  no_satis_2 <- dat %>% select(matches("_satis_whynot_punished")) %>% select(!(matches("_oth")))
  no_satis_3 <- dat %>% select(matches("_satis_whynot_comp")) %>% select(!(matches("_oth")))
  no_satis_4 <- dat %>% select(matches("_satis_whynot_nothing")) %>% select(!(matches("_oth")))
  no_satis_5 <- dat %>% select(matches("_satis_whynot_return")) %>% select(!(matches("_oth")))
  no_satis_6 <- dat %>% select(matches("_satis_whynot_corr")) %>% select(!(matches("_oth")))
  no_satis_7 <- dat %>% select(matches("_satis_whynot_threats")) %>% select(!(matches("_oth")))
  no_satis_8 <- dat %>% select(matches("_satis_whynot_safer")) %>% select(!(matches("_oth")))
  no_satis_9 <- dat %>% select(matches("_satis_whynot_idk")) %>% select(!(matches("_oth")))
  no_satis_10 <- dat %>% select(matches("_satis_whynot_rta")) %>% select(!(matches("_oth")))

  #Disinction by year
  incidents_2022 <- c("catt", "rob", "prison", "recruit",
                      "kidnap", "assault", "kill", "bomb", "fmarr", "sex")
  incidents_2023 <- c("catt", "rob", "prison", "recruit",
                      "kidnap", "assault", "kill", "bomb", "fmarr", "sex", "narco")
  incidents_names <- list(incidents_2022, incidents_2023)

  #select proper incident collection
  year_list <- function(incidents_names) {
    if (dat$date_simple[1] < "2023-01-01") {naam <- incidents_names[[1]]}
    else if (dat$date_simple[1] > "2023-01-01") {naam <- incidents_names[[2]]}
    return(naam)
  }

  year_inc <- year_list(incidents_names)

  #Calculating the totals on an incident level
  incident_sums <- function(df){
    vals <- NULL
    for (i in 1:ncol(df)){
      temp <- sum(df[[i]])
      vals <- rbind(vals, temp)
    }
    vals_2 <- t(vals)
    colnames(vals_2) <- year_inc
    vals_3 <- as.data.frame(vals_2)
    return(vals_3)
  }

  df_list <- list(no_satis_1, no_satis_2, no_satis_3, no_satis_4,
                  no_satis_5, no_satis_6, no_satis_7, no_satis_8,
                  no_satis_9, no_satis_10)

  sum_list <- lapply(df_list, incident_sums)
  sum_df <- data.table::rbindlist(sum_list, use.names = TRUE, fill = TRUE)
  #Now joining everything together

  total_no_satis <- sum(no_satis_1, no_satis_2, no_satis_3, no_satis_4,
                        no_satis_5, no_satis_6, no_satis_7, no_satis_8,
                        no_satis_9, no_satis_10)

  no_satis_table_in <- as.data.frame(c(sum(no_satis_1),sum(no_satis_2),sum(no_satis_3),
                                       sum(no_satis_4),sum(no_satis_5),sum(no_satis_6),
                                       sum(no_satis_7),sum(no_satis_8),sum(no_satis_9),
                                       sum(no_satis_10)))
  names(no_satis_table_in)[1] <- "sum"
  no_satis_table <- cbind(sum_df, no_satis_table_in)

  no_satis_table$percentage <- (no_satis_table$sum/total_no_satis)*100
  no_satis_table$satis_whynot_all <- c("The perpetrator was not caught", "The perpetrator was not punished",
                                       "No compensation for losses was offered",
                                       "The person or institution I went to for help did nothing",
                                       "Stolen goods, cattle or abducted people were not returned",
                                       "There was corruption involved",
                                       "Me or my family experienced threats as a result of seeking assistance",
                                       "I do not feel safer generally",
                                       "I don't know",
                                       "Refused to answer")

  no_satis_table <- no_satis_table %>% dplyr::relocate(satis_whynot_all)
  ft <- flextable::flextable(no_satis_table)
  ft <- flextable::autofit(ft)
  print(ft)

  #Small display for later validation (if necessary)
  print("-----------------------------------------------------------------", quote = FALSE)
  print("###############|   (Inc.)_[NON]satisfaction   |##################", quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)

  x <- sum(no_satis_table$sum)

  st1 <- "As a count (NOT Satisfied)"
  note_1 <- paste(st1,paste0("= ", x))
  print(note_1, quote = FALSE)

  return(no_satis_table)
}

# inc_satis sum function ------------------------------------------------
# Calculate percentages/counts related to satisfication (or lack of) when it comes to
# an incident that lead to contact
# Function distinguishes between incident/household

.hss_sum_inc_satis <- function(dat, dictionary){
  #Were you satisfied with your contact?
  var1 <- as.data.frame(dictionary[[1]][c(1:6)])
  var2 <- var1 %>% filter(type == "yesnortaidk") %>% filter(grepl("_satis", r_name)) %>%
    filter(!grepl("_oth", r_name))

  vars <- var2$r_name
  security_incidents_satis_basic <- subset(dat, select = vars)

  security_incidents_satis <- security_incidents_satis_basic[rowSums(is.na(security_incidents_satis_basic)) != ncol(security_incidents_satis_basic), ]

  security_incidents_satis$total_yes <- apply(security_incidents_satis, 1, function(x) length(which(x=="Yes")))
  security_incidents_satis$total_no <- apply(security_incidents_satis, 1, function(x) length(which(x=="No")))
  a <- sum(security_incidents_satis$total_yes)
  b <- sum(security_incidents_satis$total_no)
  ab <- sum(c(a,b))

  #Households that are satisfied with their contact or Not - should correspond to "HH making contact"
  print("-----------------------------------------------------------------", quote = FALSE)
  print("##################|   (Inc.)_satisfaction   |####################", quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)
  security_incidents_satis$total_yes <- apply(security_incidents_satis, 1, function(r) any(r %in% "Yes"))
  print("Households satisfied with their contact:", quote = FALSE)

  x <- as.numeric(summary(security_incidents_satis$total_yes)[3])
  y <- as.numeric(summary(security_incidents_satis$total_yes)[2])
  z <- sum(c(x,y))

  #As a percentage/Count
  st1 <- "As a count (Satisfied)"
  st2 <- "As a count (NOT Satisfied)"
  st3 <- "  *As a percentage (Satisfied)"
  st4 <- "  *As a percentage (NOT Satisfied)"
  st4_5 <- "                         "

  note_1 <- paste(st1,paste0("= ", x))
  note_2 <- paste(st2,paste0("= ", y))
  note_3 <- paste(st3,paste0("= ", (x/z)*100))
  note_4 <- paste(st4,paste0("= ", (y/z)*100))

  print(note_1, quote = FALSE)
  print(note_2, quote = FALSE)
  print(st4_5, quote = FALSE)
  print(note_3, quote = FALSE)
  print(note_4, quote = FALSE)

  #Incidents where the household was happy making contact (Satisfied)
  print("-----------------------------------------------------------------", quote = FALSE)
  st5 <- "(+) Incidents that DID lead to a satisfactory outcome"
  note_5 <- paste(st5,paste0("= ", a))
  print(note_5, quote = FALSE)

  ##As a percentage
  st6 <- "  *Percentage"
  note_6 <- paste(st6,paste0("= "), (a/ab)*100)
  print(note_6, quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)

  #Incidents where the household was NOT happy making contact (Not Satisfied)
  st7 <- "(-) Incidents that DID NOT lead to a satisfactory outcome"
  note_7 <- paste(st7,paste0("= ", b))
  print(note_7, quote = FALSE)

  ##As a percentage
  st8 <- "  *Percentage"
  note_9 <- paste(st8,paste0("= "), (b/ab)*100)
  print(note_9, quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)

}

# inc_contact sum function ------------------------------------------------
# Calculate percentages/counts related to contact (or lack of) about an incident
# Function distinguishes between incident/household

.hss_sum_inc_contact <- function(dat, dictionary){
  print("-----------------------------------------------------------------", quote = FALSE)
  print("####################|   (Inc.)_contact   |#######################", quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)

  #subset variables related to incident contact
  var1 <- as.data.frame(dictionary[[1]][c(1:6)])
  var2 <- var1 %>% filter(type == "yesnortaidk") %>% filter(grepl("_contact", r_name)) %>%
    filter(!grepl("_oth", r_name))

  vars <- var2$r_name

  SC_contact <- subset(dat, select = c(vars)) #contact (Yes - once) / (No - all)

  SC_contact_1 <- subset(dat, select = c(vars)) #contact (No - once)

  #Number of households which had an incident (remove NA/0)
  SC_contact <- SC_contact[rowSums(is.na(SC_contact)) != ncol(SC_contact), ]
  SC_contact_1 <- SC_contact_1[rowSums(is.na(SC_contact_1)) != ncol(SC_contact_1), ]

  #Number of households who contacted someone after an incident
  SC_contact$Yes_contact <- apply(SC_contact, 1, function(r) any(r %in% "Yes"))
  SC_contact_1$No_contact <- apply(SC_contact_1, 1, function(r) any(r %in% "No"))
  print("Households making contact:", quote = FALSE)

  a <- as.numeric(summary(SC_contact$Yes_contact)[3])
  b <- as.numeric(summary(SC_contact$Yes_contact)[2])
  c <- sum(c(a,b))

  d <- as.numeric(summary(SC_contact_1$No_contact)[3])
  e <- as.numeric(summary(SC_contact_1$No_contact)[2])
  f <- sum(c(d,e))

  #As a count
  st0 <- "                         "
  st1 <- "As a count (CONTACT about at least 1 incident)"
  st2 <- "As a count (NO CONTACT about at least 1 incident)"
  st3 <- "As a count (NO CONTACT about any incidents)"
  st4 <- "As a count (CONTACT about all incidents)"
  note_1 <- paste(st1,paste0("= ", a))
  note_2 <- paste(st2,paste0("= ", d))
  note_3 <- paste(st3,paste0("= ", b))
  note_4 <- paste(st4,paste0("= ", e))

  print(note_1, quote = FALSE)
  print(note_2, quote = FALSE)
  print(note_3, quote = FALSE)
  print(note_4, quote = FALSE)
  print(st0, quote = FALSE)

  #As a percentage
  st5 <- "*Number of households that made contact/asked for help"
  st6 <- "  *As a percentage (CONTACT about at least 1 incident)"
  st7 <- "  *As a percentage (NO CONTACT about at least 1 incident)"
  st8 <- "  *As a percentage (NO CONTACT about any incidents)"
  st9 <- "  *As a percentage (CONTACT about all incidents)"

  note_5 <- paste(st5,paste0("= ", c))
  note_6 <- paste(st6,paste0("= ", (a/c)*100))
  note_7 <- paste(st7,paste0("= ", (d/f)*100))
  note_8 <- paste(st8,paste0("= ", (b/c)*100))
  note_9 <- paste(st9,paste0("= ", (e/f)*100))


  print(note_5, quote = FALSE)
  print(st0, quote = FALSE)
  print(note_6, quote = FALSE)
  print(note_7, quote = FALSE)
  print(note_8, quote = FALSE)
  print(note_9, quote = FALSE)


  print("-----------------------------------------------------------------", quote = FALSE)
  #Sums of Incident reporting rates
  SC_contact$count_yes <- apply(SC_contact, 1, function(x) length(which(x=="Yes")))
  SC_contact$count_no <- apply(SC_contact, 1, function(x) length(which(x=="No")))
  SC_contact$count_idk <- apply(SC_contact, 1, function(x) length(which(x=="_I don't know")))
  SC_contact$count_rta <- apply(SC_contact, 1, function(x) length(which(x=="_Refused to answer")))

  YES_sum <- sum(SC_contact$count_yes)
  NO_sum <- sum(SC_contact$count_no)
  IDK_sum <- sum(SC_contact$count_idk)
  RTA_sum <- sum(SC_contact$count_rta)

  Rate_contact_total <- sum(c(YES_sum, NO_sum, IDK_sum, RTA_sum))

  #Count of how many incidents lead to contact
  st6 <- "YES: Incidents which have  been reported"
  note_6 <- paste(st6,paste0("= ", YES_sum))
  print(note_6, quote = FALSE)

  ##As a percentage
  st6.1 <- "  *Percentage"
  note_6.1 <- paste(st6.1,paste0("= "), (YES_sum/Rate_contact_total)*100)
  print(note_6.1, quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)

  #Count of how many incidents did not lead to contact
  st5 <- "NO: Incidents which have NOT been reported"
  note_5 <- paste(st5,paste0("= ", NO_sum))
  print(note_5, quote = FALSE)

  ##As a percentage
  st5.1 <- "  *Percentage"
  note_5.1 <- paste(st5.1,paste0("= "), (NO_sum/Rate_contact_total)*100)
  print(note_5.1, quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)

  #Count of how many incidents lead to IDK contact
  st4 <- "IDK: Incidents which have MAYBE been reported"
  note_4 <- paste(st4,paste0("= ", IDK_sum))
  print(note_4, quote = FALSE)

  ##As a percentage
  st4.1 <- "  *Percentage"
  note_4.1 <- paste(st4.1,paste0("= "), (IDK_sum/Rate_contact_total)*100)
  print(note_4.1, quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)

  #Count of how many incidents lead to RTA contact
  st3 <- "RTA: I refuse to answer"
  note_3 <- paste(st3,paste0("= ", RTA_sum))
  print(note_3, quote = FALSE)

  ##As a percentage
  st3.1 <- "  *Percentage"
  note_3.1 <- paste(st3.1,paste0("= "), (RTA_sum/Rate_contact_total)*100)
  print(note_3.1, quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)

  report <- subset(SC_contact, select = c(count_yes, count_no, count_idk, count_rta))
  return(report)
}

# inc_contact_who sum function ------------------------------------------------
# Calculate percentages/counts related to who was contacted about an incident
# Function uses incident-level rather than household-level

.hss_sum_cont_who <- function(dat, dictionary){
  who_contact_1 <- dat %>% select(matches("cont_who_police"))
  who_contact_1 <- who_contact_1 %>% select(!(matches("_oth")))
  who_contact_2 <- dat %>% select(matches("cont_who_sspdf"))
  who_contact_2 <- who_contact_2 %>% select(!(matches("_oth")))
  who_contact_3 <- dat %>% select(matches("cont_who_youth"))
  who_contact_3 <- who_contact_3 %>% select(!(matches("_oth")))
  who_contact_4 <- dat %>% select(matches("cont_who_unmiss"))
  who_contact_4 <- who_contact_4 %>% select(!(matches("_oth")))
  who_contact_5 <- dat %>% select(matches("cont_who_un"))
  who_contact_5 <- who_contact_5 %>% select(!(matches("_oth")))
  who_contact_6 <- dat %>% select(matches("cont_who_paramil"))
  who_contact_6 <- who_contact_6 %>% select(!(matches("_oth")))
  who_contact_7 <- dat %>% select(matches("cont_who_otharm"))
  who_contact_7 <- who_contact_7 %>% select(!(matches("_oth")))
  who_contact_8 <- dat %>% select(matches("cont_who_locleader"))
  who_contact_8 <- who_contact_8 %>% select(!(matches("_oth")))
  who_contact_9 <- dat %>% select(matches("cont_who_relleader"))
  who_contact_9 <- who_contact_9 %>% select(!(matches("_oth")))
  who_contact_10 <- dat %>% select(matches("cont_who_lawyer"))
  who_contact_10 <- who_contact_10 %>% select(!(matches("_oth")))
  who_contact_11 <- dat %>% select(matches("cont_who_commis"))
  who_contact_11 <- who_contact_11 %>% select(!(matches("_oth")))
  who_contact_12 <- dat %>% select(matches("cont_who_ngo"))
  who_contact_12 <- who_contact_12 %>% select(!(matches("_oth")))
  who_contact_13 <- dat %>% select(matches("cont_who_family"))
  who_contact_13 <- who_contact_13 %>% select(!(matches("_oth")))
  who_contact_14 <- dat %>% select(matches("cont_who_idk"))
  who_contact_14 <- who_contact_14 %>% select(!(matches("_oth")))
  who_contact_15 <- dat %>% select(matches("cont_who_rta"))
  who_contact_15 <- who_contact_15 %>% select(!(matches("_oth")))

  total_who_contact <- sum(who_contact_1, who_contact_2, who_contact_3, who_contact_4,
                           who_contact_5, who_contact_6, who_contact_7, who_contact_8,
                           who_contact_9, who_contact_10, who_contact_11, who_contact_13,
                           who_contact_14, who_contact_15)

  who_contact_table <- as.data.frame(c(sum(who_contact_1),sum(who_contact_2),sum(who_contact_3),
                                       sum(who_contact_4),sum(who_contact_5),sum(who_contact_6),
                                       sum(who_contact_7),sum(who_contact_8),sum(who_contact_9),
                                       sum(who_contact_10),sum(who_contact_11), sum(who_contact_12),
                                       sum(who_contact_13), sum(who_contact_14),
                                       sum(who_contact_15)))
  names(who_contact_table)[1] <- "sum"
  who_contact_table$percentage <- (who_contact_table$sum/total_who_contact)*100
  who_contact_table$actors <- (c("Police", "SSPDF", "Local armed youth",
                                 "UNMISS", "Other UN Agencies", "Other organized forces",
                                 "National security forces", "Community leader",
                                 "Religious leader", "Lawyer", "Commissioner",
                                 "NGO", "Family/Friends",
                                 "I don't know","Refuse to answer"))

  who_contact_table <- who_contact_table %>% dplyr::relocate(actors)
  print(flextable(who_contact_table))
  return(who_contact_table)
}

# inc_contact_whynot sum function ------------------------------------------------
# Calculate percentages/counts related to why no contact was bade after an incident
# Function uses incident-level rather than household-level

.hss_sum_cont_whynot <- function(dat, dictionary){
  #General calculation of all reasons for no-contact (includes all variables, focus on sums)
  no_contact_1 <- dat %>% select(matches("cont_whynot_believe"))
  no_contact_1 <- no_contact_1 %>% select(!(matches("_oth")))
  no_contact_2 <- dat %>% select(matches("cont_whynot_cost"))
  no_contact_2 <- no_contact_2 %>% select(!(matches("_oth")))
  no_contact_3 <- dat %>% select(matches("cont_whynot_shame"))
  no_contact_3 <- no_contact_3 %>% select(!(matches("_oth")))
  no_contact_4 <- dat %>% select(matches("cont_whynot_harm"))
  no_contact_4 <- no_contact_4 %>% select(!(matches("_oth")))
  no_contact_5 <- dat %>% select(matches("cont_whynot_bias1"))
  no_contact_5 <- no_contact_5 %>% select(!(matches("_oth")))
  no_contact_6 <- dat %>% select(matches("cont_whynot_bias2"))
  no_contact_6 <- no_contact_6 %>% select(!(matches("_oth")))
  no_contact_7 <- dat %>% select(matches("cont_whynot_nomeans"))
  no_contact_7 <- no_contact_7 %>% select(!(matches("_oth")))
  no_contact_8 <- dat %>% select(matches("cont_whynot_nothink"))
  no_contact_8 <- no_contact_8 %>% select(!(matches("_oth")))
  no_contact_9 <- dat %>% select(matches("cont_whynot_idk"))
  no_contact_9 <- no_contact_9 %>% select(!(matches("_oth")))
  no_contact_10 <- dat %>% select(matches("cont_whynot_rta"))
  no_contact_10 <- no_contact_10 %>% select(!(matches("_oth")))

  #Calculating the totals on an incident level
  incident_sums <- function(df){
    vals <- NULL
    for (i in 1:ncol(df)){
      temp <- sum(df[[i]])
      vals <- rbind(vals, temp)
    }
    vals_2 <- t(vals)
    colnames(vals_2) <- c("catt", "rob", "prison", "recruit",
                          "kidnap", "assault", "kill", "bomb", "fmarr", "sex", "narco")
    vals_3 <- as.data.frame(vals_2)
    return(vals_3)
  }
  df_list <- list(no_contact_1, no_contact_2, no_contact_3, no_contact_4,
                  no_contact_5, no_contact_6, no_contact_7, no_contact_8,
                  no_contact_9, no_contact_10)

  sum_list <- lapply(df_list, incident_sums)
  sum_df <- data.table::rbindlist(sum_list, use.names = TRUE, fill = TRUE)
  #Now joining everything together

  total_no_contact <- sum(no_contact_1, no_contact_2, no_contact_3, no_contact_4,
                          no_contact_5, no_contact_6, no_contact_7, no_contact_8,
                          no_contact_9, no_contact_10)

  no_contact_table_in <- as.data.frame(c(sum(no_contact_1),sum(no_contact_2),sum(no_contact_3),
                                         sum(no_contact_4),sum(no_contact_5),sum(no_contact_6),
                                         sum(no_contact_7),sum(no_contact_8),sum(no_contact_9),
                                         sum(no_contact_10)))
  names(no_contact_table_in)[1] <- "sum"
  no_contact_table <- cbind(sum_df, no_contact_table_in)


  no_contact_table$percentage <- (no_contact_table$sum/total_no_contact)*100
  no_contact_table$cont_whynot_all <- c("I did not believe anyone could help me resolve the issue",
                                        "It would cost too much time or money to resolve",
                                        "It would bring shame on my family",
                                        "I feared more harm against myself or my family",
                                        "There is bias against me or other household members",
                                        "There is bias in favor of the ones who committed the violence",
                                        "I could not get in contact with anyone for help (e.g. was in a remote area, had no phone or network)",
                                        "I did not think of it",
                                        "I don't know",
                                        "Refuse to answer")
  no_contact_table <- no_contact_table %>% dplyr::relocate(cont_whynot_all)
  ft <- flextable(no_contact_table)
  ft <- autofit(ft)
  print(ft)
  return(no_contact_table)
}

# inc_perp sum function ------------------------------------------------
# Calculate percentages/counts related to who was the perpetrator of an incident
# Function uses incident-level rather than household-level

.hss_sum_perp <- function(dat, dictionary){
  security_incidents_perp_base <- dat %>% select(matches("_perp"))
  security_incidents_perp <- security_incidents_perp_base %>% select(!(matches("secinc")))

  a <- sum(security_incidents_perp == "Local armed youth", na.rm=T)
  b <- sum(security_incidents_perp == "Someone from a neighboring community", na.rm=T)
  c <- sum(security_incidents_perp == "Criminals", na.rm=T)
  d <- sum(security_incidents_perp == "Police", na.rm=T)
  e <- sum(security_incidents_perp == "National army (SSPDF)", na.rm=T)
  f <- sum(security_incidents_perp == "Other organized forces (wildlife brigade, prison service, fire brigade, etc)", na.rm=T)
  g <- sum(security_incidents_perp == "National Security Forces (NS)", na.rm=T)
  h <- sum(security_incidents_perp == "Someone from my own family", na.rm=T)
  i <- sum(security_incidents_perp == "Someone from my own community", na.rm=T)
  j <- sum(security_incidents_perp == "_I don't know", na.rm=T)
  k <- sum(security_incidents_perp == "_Refused to answer", na.rm=T)
  l <- sum(security_incidents_perp == "_Other", na.rm=T)
  total_perps <- sum(c(a,b,c,d,e,f,g,h,i,j,k,l))

  perp_table <- as.data.frame(c(a,b,c,d,e,f,g,h,i,j,k,l))
  names(perp_table)[1] <- "sum"
  perp_table$percentage <- (perp_table$sum/total_perps)*100
  perp_table$perpetrators <- c("Local armed youth","Someone from a neighboring community",
                               "Criminals","Police","National army (SSPDF)",
                               "Other organized forces (wildlife brigade, prison service, fire brigade, etc)",
                               "National Security Forces (NS)", "Someone from my own family",
                               "Someone from my own community", "_I don't know", "_Refused to answer", "_Other")
  perp_table <- perp_table %>% dplyr::relocate(perpetrators)
  print(flextable(perp_table))
  return(perp_table)
}

# inc sum function ------------------------------------------------
# Calculate percentages/counts related to experienced incidents
# Function distinguishes between incident/household

.hss_sum_incidents <- function(dat, dictionary){

  #Determine which incidents have been recorded
  #At the moment, this is not a useful addition, but will help make the code
  #applicable to older datasets
  incidents_SS_1 <- data.frame(
    rob=(1),
    bomb=(2),
    assault=(3),
    sex=(4),
    prison=(5),
    kidnap=(6),
    kill=(7),
    catt=(8),
    recruit=(9),
    fmarr=(10),
    narco=(11),
    suicide=(12)
  )

  result <- janitor::compare_df_cols(dat, incidents_SS_1, return = "mismatch")
  incidents_2 <- result$column_name

  #hardcoded incidents

  security_incidents <- subset(dat, select = incidents_2)
  security_incidents_2 <- subset(dat, select = incidents_2)

  #Basic calculations to establish how many households reported at least 1 inc
  security_incidents$num <- apply(security_incidents, 1, function(r) any(r %in% "Yes"))
  security_incidents_2$num_2 <- apply(security_incidents_2, 1,
                                      function(r) all(r %in% c("No",
                                                               "_Refused to answer",
                                                               "_I don't know")))

  print("-----------------------------------------------------------------", quote = FALSE)
  print("######################|   (Inc.)_count   |#######################", quote = FALSE)
  print("-----------------------------------------------------------------", quote = FALSE)
  print("                         ", quote = FALSE)

  a <- as.numeric(table(security_incidents$num))[1] #no incidents (count)
  b <- as.numeric(table(security_incidents$num))[2] #incident reported (count)
  c <- sum(c(a,b)) #total households
  d <- (b/c)*100 #percentage of households that reported one incident

  e <- as.numeric(table(security_incidents_2$num_2))[1] #some incidents (count)
  f <- as.numeric(table(security_incidents_2$num_2))[2] #no incidents reported (count)
  g <- sum(c(e,f)) #total households
  h <- (f/g)*100 #percentage of households that reported one incident


  st1 <- "Number of households reporting incidents"
  st2 <- "                         "
  st3 <- "Number of households that EXPERIENCED at least one incident"
  st4 <- "**Percentage of households that EXPERIENCED at least one incident"
  st5 <- "                         "
  st6 <- "Number of households that DID NOT EXPERIENCE any incidents"
  st7 <- "**Percentage of households that DID NOT EXPERIENCE any incidents"

  note_1 <- paste(st1,paste0("= ", c))
  note_2 <- paste(st2)
  note_3 <- paste(st3,paste0("= ", b))
  note_4 <- paste(st4,paste0("= ", d))
  note_5 <- paste(st5)
  note_6 <- paste(st6,paste0("= ", f))
  note_7 <- paste(st7,paste0("= ", h))

  print(note_1, quote = FALSE)
  print(note_2, quote = FALSE)
  print(note_3, quote = FALSE)
  print(note_4, quote = FALSE)
  print(note_5, quote = FALSE)
  print(note_6, quote = FALSE)
  print(note_7, quote = FALSE)

  #Calculation of total incidents reported
  aa <- as.numeric(length(which(security_incidents$catt=="Yes")))
  bb <- as.numeric(length(which(security_incidents$rob=="Yes")))
  cc <- as.numeric(length(which(security_incidents$prison=="Yes")))
  dd <- as.numeric(length(which(security_incidents$recruit=="Yes")))
  ee <- as.numeric(length(which(security_incidents$kidnap=="Yes")))
  ff <- as.numeric(length(which(security_incidents$assault=="Yes")))
  gg <- as.numeric(length(which(security_incidents$kill=="Yes")))
  hh <- as.numeric(length(which(security_incidents$bomb=="Yes")))
  ii <- as.numeric(length(which(security_incidents$fmarr=="Yes")))
  jj <- as.numeric(length(which(security_incidents$sex=="Yes")))
  kk <- as.numeric(length(which(security_incidents$narco=="Yes")))
  ll <- as.numeric(length(which(security_incidents$suicide=="Yes")))

  names <- c("catt", "rob","prison","recruit","kidnap","assault","kill", "bomb",
             "fmarr", "sex", "narco", "suicide")

  inc_sum <- sum(c(aa,bb,cc,dd,ee,ff,gg,hh,ii,jj,kk,ll))
  frame <- as.data.frame(c(aa,bb,cc,dd,ee,ff,gg,hh,ii,jj,kk,ll))
  names(frame)[1] <-  "Count"
  frame$percent <- (frame$Count/inc_sum)*100
  rownames(frame) <- names
  frame$variable <- names
  frame <- frame %>%
    dplyr::relocate(variable, .before = Count)
  print(flextable(frame))

  print("-----------------------------------------------------------------", quote = FALSE)
  i <- sum(frame$Count)
  st8 <- "Total number of incidents reported"
  note_8 <- paste(st8,paste0("= ", i))
  print(note_8, quote = FALSE)

  return(frame)
}

# vctm sum function ------------------------------------------------
# Calculate percentages/counts related to victimized groups

.hss_sum_vctms <- function(dat, dictionary){
  boys <- dat %>% select(matches("ms_boys"))
  girls <- dat %>% select(matches("ms_girls"))
  men <- dat %>% select(matches("ms_men"))
  women <- dat %>% select(matches("ms_women"))
  idk <- dat %>% select(matches("ms_idk"))
  rta <- dat %>% select(matches("ms_rta"))

  total_vctms <- sum(boys, girls, men, women, idk, rta)

  vctms_table <- as.data.frame(c(sum(boys),sum(girls), sum(men),sum(women),sum(idk),sum(rta)))
  names(vctms_table)[1] <- "sum"
  vctms_table$percentage <- (vctms_table$sum/total_vctms)*100
  vctms_table$victims <- c("boys","girls","men","women","idk","rta")
  vctms_table <- vctms_table %>% dplyr::relocate(victims)
  print(flextable(vctms_table))
  return(vctms_table)
}



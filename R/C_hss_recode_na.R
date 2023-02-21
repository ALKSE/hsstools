#' Recode values to NA
#'
#' In the exported HSS datafile values are coded as 0  even when that particular
#' question was skipped for a respondent. In those cases NA values are expected.
#' This function recodes 0 values to NA where appropriate. Note that currently
#' the variables which are recoded are hard-coded into the function
#'
#' @param dat The dataframe with HSS survey data for which you want to recode
#'
#' @return Returns a dataframe with recoded values
#' @export
#'
C_hss_recode_na  = function(dat) {
  # vars to recode ----------------------------------------------------------
  occupation <- list(
    vars = c("occupation_wagetype"),
    cond = "occupation != 5"
  )
  migr <- list(
    vars = c(
      "migr_nr", "migr_why_insec", "migr_why_hazard", "migr_why_seasonomad", "migr_why_econopp",
      "migr_why_marriage", "migr_why_basicserv", "migr_why_ethnic", "migr_why_return",
      "migr_why_oth", "migr_why_idk", "migr_why_rta", "before_residence"
    ),
    cond = "residence != 2"
  )
  sec_imp <- list(
    vars = c(
      "imprv_exposed", "imprv_weapons", "imprv_patrol", "imprv_gangs",
      "imprv_bribe", "imprv_leavehome", "imprv_travel", "imprv_livelihood",
      "imprv_basicserv", "imprv_food", "imprv_family", "imprv_support", "imprv_none", "imprv_oth",
      "imprv_idk", "imprv_rta"
    ),
    cond = "security_change != 1"
  )
  sec_wors <- list(
    vars = c(
      "wors_exposedoutside", "wors_strangers", "wors_weapons",
      "wors_patrol", "wors_gangs", "wors_bribe", "wors_stressed",
      "wors_leavehome", "wors_checkpoints", "wors_livelihood",
      "wors_basicserv", "wors_food", "wors_house", "wors_assets",
      "wors_family", "wors_exposedinside", "wors_none", "wors_oth",
      "wors_idk", "wors_rta"
    ),
    cond = "security_change != 2"
  )
  env <- list(vars = c(
    "env_cons_resources", "env_cons_conflict", "env_cons_security", "env_cons_food",
    "env_cons_migration", "env_cons_disease", "env_cons_none", "env_cons_oth", "env_cons_idk",
    "env_cons_rta", "env_addr_ngo", "env_addr_smallfunds", "env_addr_authorities",
    "env_addr_migration", "env_addr_exchange", "env_addr_none", "env_addr_oth", "env_addr_idk",
    "env_addr_rta"
  ),
  cond = "env_change_rain != 1 & env_change_season != 1 & env_change_drought != 1 & env_change_flood != 1 & env_change_temperature != 1 & env_change_oth != 1"
  )
  out = dat %>%
    .recode_na(occupation) %>%
    .recode_na(migr) %>%
    .recode_na(sec_imp) %>%
    .recode_na(sec_wors) %>%
    .recode_na(env)
  return(out)
}

#' @keywords internal
.recode_na <- function(dat, varlist) {
  # used in hss_recode_na. Don't run separately

  # filter for existing vars
  varlist$vars <- varlist$vars[which(varlist$vars %in% names(dat))]
  # recode listed cols to NA for given condition
  out <- dat %>% dplyr::mutate(
    dplyr::across(
      .cols = varlist$vars,
      .fns = ~ dplyr::if_else(eval(parse(text = varlist$cond)), NA_integer_, .x)
    )
  )
  return(out)
}

#' Translate HSS content from Arabic to English
#'
#' @param df the dataframe containing Arabic text columns. These columns are expected to end in "_ar"
#'
#' @return A dataframe containing Arabic text columns and their English translations.
#' @export
#'
hss_translate <- function(df) {
  library(dplyr)
  library(stringr)
  library(translateR)
  # translateR gives warnings about source language, since we both define and expect
  # source text as Arabic these warnings can be safely ignored.
  defaultW <- getOption("warn")
  options(warn = -1)
  # select columns with ar answers & add index column
  df_ar <- select(df, ends_with("_ar") & !where(is.logical)) %>%
    mutate(index = seq.int(nrow(df)))

  # newdata is copy of dataframe. All translated columns are appended to newdata
  df_translated <- df_ar

  for (i in names(select(df_ar, !index))) {
    translated <- df_ar %>% filter(!is.na(!!rlang::sym(i)))
    translated <- translate(
      dataset = translated,
      content.field = i,
      google.api.key = apikey,
      source.lang = "ar",
      target.lang = "en"
    )
    # all.x must be set to true to make sure the entire index is present in the new dataframe.
    df_translated <- merge(df_translated, translated, all.x = TRUE)
    # rename translated to original colname with "_en" suffix
    names(df_translated)[names(df_translated) == "translatedContent"] <- sub("_ar", "_en", i)
  }
  df_translated <- df_translated[, stringr::str_sort(names(df_translated), numeric = TRUE)]
  options(warn = defaultW)
  return(df_translated)
}

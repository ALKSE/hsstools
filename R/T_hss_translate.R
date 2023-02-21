#' Translate HSS content from Arabic to English
#'
#' Translate the content of Arabic text columns from the HSS dataset to English. This function
#' uses the translateR package to connect to Google Cloud Services. An API key from a GCS account is required.
#' Translated columns are returned as a separate dataframe. This makes it possible to export the translated
#' columns to e.g. check the accuracy of translations. The extracted and translated columns can be re-inserted
#' into the original dataset using the `hss_merge_translated` function.
#' @param df the dataframe containing Arabic text columns. These columns are expected to end in "_ar"
#'
#' @return A dataframe containing Arabic text columns and their English translations.
#' #@seealso `hss_merge_translated`
#' @export
#'
T_hss_translate <- function(df, apikey) {
  # translateR gives warnings about source language, since we both define and expect
  # source text as Arabic these warnings can be safely ignored.
  defaultW <- getOption("warn")
  options(warn = -1)
  # select all columns containing Arabic text (skipping empty cols). Adds an index
  # column for later matching to original dataframe.
  df_ar <- dplyr::select(df, dplyr::ends_with("_ar") & !where(is.logical)) %>%
    dplyr::mutate(index = seq.int(nrow(df)))

  # df_translated is copy of dataframe. All translated columns are appended to this new df
  df_translated <- df_ar
  # select only non-empty cells to translate
  for (i in names(dplyr::select(df_ar, !index))) {
    translated <- df_ar %>% dplyr::filter(!is.na(!!rlang::sym(i)))
    translated <- translateR::translate(
      dataset = translated,
      content.field = i,
      google.api.key = apikey,
      source.lang = "ar",
      target.lang = "en"
    )
    # merge translation results with df_translated
    df_translated <- merge(df_translated, translated, all.x = TRUE)
    # set correct names for translated cols (based on "_ar" name)
    names(df_translated)[names(df_translated) == "translatedContent"] <- sub("_ar", "_en", i)
  }
  # NB: don't use base sort()
  df_translated <- df_translated[, stringr::str_sort(names(df_translated), numeric = TRUE)]
  # reset warning options to original
  options(warn = defaultW)

  return(df_translated)
}

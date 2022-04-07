#' Merges HSS dataframe with dataframe containing translated columns
#'
#' @param df The original HSS dataframe
#' @param df_translated The dataframe containing "_ar" and translated "_en" columns
#'
#' @return A merged dataframe containing HSS data and all translated columns.
#' @export
#'
hss_mergetranslated <- function(df, df_translated) {
  library(dplyr)
  # adds empty _en column if missing from translated
  for (i in names(select(df, ends_with("_ar")))) {
    if (!gsub("_ar", "_en", i) %in% names(df_translated)) {
      df_translated$newcol <- df_translated[[i]]
    }
    names(df_translated)[names(df_translated) == "newcol"] <- sub("_ar", "_en", i)
  }

  # Binds translated columns to the original datafile
  df_merged <- cbind(df, dplyr::select(df_translated, ends_with("_en")))

  # each translated column is relocated to the position after its original arabic
  # column. e.g. "Q1_en" after "Q1_ar" etc.
  for (i in names(select(df_translated, ends_with("_en")))) {
    df_merged <- dplyr::relocate(df_merged, all_of(i),
      .after = gsub("_en", "_ar", i)
    )
  }
  return(df_merged)
}

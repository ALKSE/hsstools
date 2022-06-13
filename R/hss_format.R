#' Format tables using HSS style preferences
#'
#' Converts table to a flextable and applies formatting. Formatting preferences
#' are stored in [TBD]
#'
#' @return
#' @export
#' @rdname hss_format
hss_format_single <- function(table) {
  # load formatting options
  format <- .get_format_options()
  # Check if table was already converted to flextable object.
  if (!inherits(table, "flextable")) {
    table <- flextable::flextable(table)
  }
  # applies formatting
  table <- .hss_format(table)
  return(table)
}
#' @rdname hss_format
hss_format_multi <- function(table) {
  format <- .get_format_options()
  # Check if table was already converted to flextable object.
  if (!inherits(table, "flextable")) {
    table <- flextable::flextable(table)
  }
  # applies formatting
  table <- .hss_format(table) %>%
    flextable::bg(i = ~ p <= 0.05, bg = format$body_bg_emph, part = "body") %>%
    # flextable::mk_par()
  flextable::void(j = "p", part = "all")

  return(table)
}

#' @rdname hss_format
hss_format_group <- function() {

}
#' helper function for hss_format functions. Don't run separately.
#' @keywords internal
.hss_format <- function(table) {
  # load formatting options
  format <- .get_format_options()
  # set font
  flextable::set_flextable_defaults(font.family = format$font)
  # apply formatting
  table <- table %>%
    flextable::hline(
      border = officer::fp_border(color = format$border_color, style = format$border_style, width = format$border_width),
      part = "all"
    ) %>%
    flextable::hline_top(
      border = officer::fp_border(color = format$border_color, style = format$border_style, width = format$border_width),
      part = "all"
    ) %>%
    # format header
    flextable::bg(bg = format$header_bg, part = "header") %>%
    flextable::color(color = format$header_text, part = "header") %>%
    flextable::bold(part = "header") %>%
    # format body
    flextable::bg(bg = format$body_bg, part = "body") %>%
    flextable::set_table_properties(layout = "autofit") %>%
    # format footer
    # flextable::add_footer_row(values = "", colwidth = flextable::ncol_keys(table)) %>%
    return(table)
}

# PAX colors tables
#
# RGB
# Orange		    R 230	G 79	B 26
# Orange Light	R 252	G 228	B 214
# grey		      R 242	G 242	B 242
# grey dark	    R 217	G 217	B 217
#
# Hex
# orange		    #E64F1A
# orange light	#FCE4D6
# grey	       	#F2F2F2
# grey dark    	#D9D9D9

.get_format_options <- function() {
  out <- list(
    # header (question, title etc.)
    header_bg     = "#E64F1A",
    header_text   = "white",
    # body (grouping, response options, values)
    body_bg       = "#F2F2F2",
    body_bg_emph  = "#D9D9D9",
    body_bg_total = "#FCE4D6",
    body_text     = "black",
    # footer (chi-sq message, etc)
    footer_bg     = "white",
    footer_text   = "black",
    #header labels
    total_en      = "Total",
    # Heel erg lelijke hack om unicode correct weer te geven. Dit moet ooit beter
    total_ar      = c("0627", "0644", "0645", "062C", "0645", "0648", "0639") %>% paste0("\\u", .) %>% stringi::stri_unescape_unicode() %>% stringr::str_flatten(),
    # misc.
    border_color  = "black",
    border_style  = "solid",
    border_width  = 1,
    font          = "PT Sans"
  )
  return(out)
}

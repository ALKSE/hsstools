#' Calculate variable sums based on group
#'
#' This multipurpose function provides a selection of utility functions which can
#' be used to calculate sums/disagregations necessary for HSS publications. In case of
#' any issues, please input '?' into the function to list options.
#'
#' @param dat clean data-set
#' @param group variable shothand used to select utility function
#'
#' @return Text/Table providing required calculations for HSS outputs
#' @export
#'
#' @rdname R_hss_sum_values

R_hss_sum_values <- function(dat, dictionary, group){
  if(group == "?")
  {
    print("Try the following:", quote = FALSE)
    print("(1) _vctms", quote = FALSE)
    print("(2) _perp", quote = FALSE)
    print("(3) _cont_whynot", quote = FALSE)
    print("(4) _cont_who", quote = FALSE)
    print("(5) _inc", quote = FALSE)
    print("(6) _inc_satis", quote = FALSE)
    print("(7) _inc_cont", quote = FALSE)
    print("(8) _satis_whynot", quote = FALSE)
    print("(9) _satis_why", quote = FALSE)
  }
  else if (group == "_perp"){.hss_sum_perp(dat)}
  else if (group == "_cont_whynot"){.hss_sum_cont_whynot(dat)}
  else if (group == "_cont_who"){.hss_sum_cont_who(dat)}
  else if (group == "_vctms"){.hss_sum_vctms(dat)}
  else if (group == "_inc"){.hss_sum_incidents(dat)}
  else if (group == "_inc_satis"){
    dictionary <- dic
    .hss_sum_inc_satis(dat, dictionary)
    }
  else if ((group == "_inc_cont") & (!missing(dictionary))){.hss_sum_inc_contact(dat, dictionary)}

  else if (group == "_satis_whynot"){.hss_sum_satis_whynot(dat)}
  else if (group == "_satis_why"){.hss_sum_satis_why(dat, year = NULL)}

}

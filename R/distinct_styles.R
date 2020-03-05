#' @title Obtain a Set of Distinct Line Types
#' @description return \code{n} line types
#' @param n the number of line types to generate
#' @return a vector of line types
#' @export aitoa.distinct.lty
aitoa.distinct.lty <- function(n) {
  stopifnot(!is.null(n),
            is.integer(n),
            length(n) == 1L,
            !is.na(n),
            is.finite(n),
            n > 0L);
  return(as.integer(1L + (seq_len(n) %% 6L)));
}

.interal.pch.transform <- unique(unname(unlist(c(c(1L, 2L, 5L, 6L),
                            ((1L:18L)[-c(1L, 2L, 5L, 6L)])))));

#' @title Obtain a Set of Distinct Plot Characters
#' @description return \code{n} plot characters
#' @param n the number of plot characters to generate
#' @return a vector of plot characters
#' @export aitoa.distinct.pch
aitoa.distinct.pch <- function(n) {
  stopifnot(!is.null(n),
            is.integer(n),
            length(n) == 1L,
            !is.na(n),
            is.finite(n),
            n > 0L);
  return(.interal.pch.transform[as.integer(1L + (seq_len(n) %% 19L))]);
}

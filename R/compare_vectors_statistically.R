#' @title Perform a Statistical Test Comparing Two Vectors
#' @description Compare two vectors of samples using a statistical test (the
#'   two.sided \link[stats]{wilcox.test}, i.e., the Mann-Whitney U test). This
#'   function suppresses warnings that emerge from ties and repeated values. It
#'   is not reliable for productive use, but for the context of our book, it
#'   will suffice for now.
#' @param x the first vector
#' @param y the second vector
#' @param alpha the significance threshols
#' @return the result, a list of values\describe{ \item{p}{the p-value computed
#'   by the test} \item{r}{a character, either\describe{ \item{<}{if \code{x} is
#'   significantly smaller than \code{y}} \item{>}{if \code{x} is significantly
#'   larger than \code{y}} \item{?}{if the difference between \code{x} and
#'   \code{y}, if any, is not significant} }} }
#' @importFrom stats wilcox.test
#' @export aitoa.compare.vectors.statistically
aitoa.compare.vectors.statistically <- function(x,
                                                y,
                                                alpha) {
  stopifnot(!is.null(x),
            is.numeric(x),
            length(x) > 0L,
            all(is.finite(x)),
            !is.null(y),
            is.numeric(y),
            length(y) > 0L,
            all(is.finite(y)),
            is.numeric(alpha),
            length(alpha) == 1L,
            is.finite(alpha),
            alpha >= 0,
            alpha < 0.5);

  result <- suppressWarnings(
              wilcox.test(x=x,
                          y=y,
                          correct=TRUE,
                          alternative="two.sided",
                          paired=FALSE,
                          exact=TRUE,
                          conf.int=FALSE,
                          conf.level=FALSE));

  p <- result$p.value;
  stopifnot(is.numeric(p),
            length(p) == 1L,
            is.finite(p),
            p >= 0,
            p <= 1);

  if(p <= alpha) {
    all.ranks <- rank(c(x, y));
    s.1 <- sum(all.ranks[seq_along(x)]);
    s.2 <- sum(all.ranks[length(x) + seq_along(y)]);
    stopifnot(is.numeric(s.1),
              length(s.1) == 1L,
              is.finite(s.1),
              s.1 >= 0,
              is.numeric(s.2),
              length(s.2) == 1L,
              is.finite(s.2),
              s.2 >= 0,
              s.1 != s.2);
    if(s.1 < s.2) { r <- '<'; }
    else { r <- '>'; }
  } else {
    r <- '?';
  }

  result <- list(p = p,
                 r = r);
  result <- force(result);
  return(result);
}

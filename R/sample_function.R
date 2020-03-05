#' @title Sample a Function at Some Points in the Given Interval
#' @description  Sample a function so that it can be plotted smoothly.
#' @param f the function to sample
#' @param x.min the minimum (inclusive) x value
#' @param x.max the maximum (inclusive) x value
#' @param keep.only a selector: only points meeting this specification will be
#'   returned
#' @return a list with the x and y coordinates of the samples
#' @export aitoa.sample.function
aitoa.sample.function <- function(f,
                                  x.min,
                                  x.max,
                                  keep.only=is.finite) {
  stopifnot(!is.null(f),
            is.function(f),
            !is.null(x.min),
            is.numeric(x.min),
            length(x.min) == 1L,
            is.finite(x.min),
            !is.null(x.max),
            is.numeric(x.max),
            length(x.max) == 1L,
            is.finite(x.max),
            x.max > x.min,
            !is.null(keep.only),
            is.function(keep.only));

  x.base.lin <- ((0L:100L) / 0.01);
  x.base.log <- log(1L:100L)/log(100L);
  x.base.sqr <- (1L:100L)^2 / 100000L;
  x.base.exp <- exp(0L:100L) / exp(100L);
  x.base <- sort(unique(unname(unlist(c(
    x.base.lin,
    x.base.log,
    1 - x.base.log,
    x.base.sqr,
    1 - x.base.sqr,
    x.base.exp,
    1 - x.base.exp
  )))));
  x.base <- x.base[is.finite(x.base) &
                   (x.base >= 0) & (x.base <= 1)];

  x <- x.min + (x.base * (x.max - x.min));
  x <- x[is.finite(x) &
         (x >= x.min) & (x <= x.max)];
  y <- f(x);

  keep <- keep.only(y);
  stopifnot(any(keep));

  y <- y[keep];
  x <- x[keep];

  stopifnot(is.numeric(x),
            is.numeric(y),
            length(x) == length(y));
  return(list(x=x, y=y));
}

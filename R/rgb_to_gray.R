#' @title Convert a RGB Color to Gray Scale using Luminosity Weights
#' @description We apply the simple algorithm from
#'   \url{http://www.johndcook.com/blog/2009/08/24/algorithms-convert-color-grayscale/}
#'    to convert rgb values to gray-scale based on luminosity. Notice that this
#'   function does not care about the range of \code{r}, \code{g}, and \code{b}:
#'   If all of them are in \code{0..255}, then the output will also be in
#'   \code{[0,255]} (though not necessary integer). If they are in \code{[0,1]},
#'   the output is also in \code{[0,1]}. As coefficients, we use the values for
#'   BT.709 (\url{http://en.wikipedia.org/wiki/Rec._709}).
#' @param color the r-g-b vector
#' @param limit the color limit
#' @param make.int should we convert the result to an integer?
#' @return the scalar gray scale value.
#' @export aitoa.rgb2gray
aitoa.rgb2gray <- function(color,
                           limit=255L,
                           make.int=(limit>1L)) {
  r <- color[[1L]];
  g <- color[[2L]];
  b <- color[[3L]];
  if((r == g) && (g == b)) {
    color <- r;
  } else {
    color <- (0.2126*r) + (0.7152*g) + (0.0722*b);
  }

  if(make.int) {
    color <- as.integer(round(color));
  }
  if(color < 0) { color <- 0L; }
  if(color > limit) { color <- limit; }
  if(make.int) {
    return(as.integer(color));
  }
  return(color);
}

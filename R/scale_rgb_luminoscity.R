#' @title Scale the Luminosity of a Color
#' @description The luminosity of a color is scaled in a range of \code{[-1,1]},
#'   where -1 means black, 1 means white, and 0 means the color at its current
#'   luminosity. A value in \code{(0, 1]} will make the color brighter while
#'   trying to preserve its chromotographical properties. A value in \code{[-1,
#'   0)} will make the color darker while trying to preserve its
#'   chromotographical properties. See
#'   \url{https://stackoverflow.com/questions/6615002}
#' @param color the r-g-b vector
#' @param scale the scale value, in \code{[-1,1]}
#' @param limit the maximum permitted value for each rgb-coordinate: defaults to
#'   255
#' @param  make.int should the result be converted to integer?
#' @return a color with similar chromotographical properties but either lighter
#'   (scale>0) or darker (scale<0) complexion.
#' @export aitoa.scale.rgb.luminosity
aitoa.scale.rgb.luminosity <- function(color,
                                       scale,
                                       limit=255L,
                                       make.int=(limit>1L)) {
  if(scale >= 1) {
    color <- (c(limit, limit, limit));
    if(make.int) {
      return(as.integer(color));
    }
    return(color);
  }
  if(scale <= (-1)) {
    return(as.integer(c(0L, 0L, 0L)));
  }
  if(scale != 0) {
    cc <- (color / limit) ^ 2.2;

    if(scale < 0) {
      m <- (1 + scale);
      cc <- m*cc;
    } else {
      cc <- cc + (scale * (1 - cc));
    }

    color <- (cc ^ (1/2.2)) * limit;
  }
  if(make.int) {
    color <- as.integer(round(color));
  }
  color[color < 0L] <- 0L;
  color[color > limit] <- limit;
  if(make.int) {
    return(as.integer(color));
  }
  return(color);
}

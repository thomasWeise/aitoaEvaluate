#' @title Mix Two RGB Colors
#' @description Mix two RGB colors
#' @param color.1 the first r-g-b vector
#' @param color.2 the second r-g-b vector
#' @param weight the weight of the first color vector
#' @param limit the color limit
#' @param make.int should we convert the result to an integer?
#' @return  the mixed color
#' @export aitoa.rgb2gray
aitoa.rgb.mix <- function(color.1,
                          color.2,
                          weight=0.5,
                          limit=255L,
                          make.int=(limit > 1L)) {
  if(all(color.1 == color.2) || (weight <= 0)) {
    color <- color.1;
  } else {
    if(weight >= 1) {
      color <- color.2;
    } else {
      color <- sqrt( (weight * (color.1^2)) +
                    ((1 - weight) * (color.2^2)));
    }
  }

  if(make.int) {
    color <- (as.integer(round(color)));
  }
  color[color < 0] <- 0L;
  color[color > limit] <- limit;
  if(make.int) {
    return(as.integer(round(color)));
  }
  return(color);
}

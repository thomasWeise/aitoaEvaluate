#' @title Make a Color Partially Transparent
#' @description Take a color and add some transparency to it.
#' @param color the color
#' @param transparency the degree of transparency: 1 for maximally transparent
#'   (i.e., invisible), 0 for no transparency at all (i.e., the original color)
#' @export aitoa.make.color.transparent
#' @importFrom grDevices col2rgb rgb
#' @include common_styles.R
aitoa.make.color.transparent <- function(color,
                                         transparency=.default.transparency) {
  stopifnot(!is.null(color),
            is.character(color),
            !any(is.na(color)),
            length(color)== 1L,
            nchar(color) > 0L,
            !is.null(transparency),
            is.numeric(transparency),
            !any(is.na(transparency)),
            length(transparency) == 1L,
            is.finite(transparency),
            transparency >= 0,
            transparency <= 1);
  if(transparency <= 0) {
    return(color);
  }
  rgbv <- col2rgb(color);
  stopifnot(is.numeric(rgbv),
            length(rgbv) >= 3L,
            all(is.finite(rgbv)),
            all(rgbv >= 0L),
            all(rgbv <= 255L));
  return(rgb(rgbv[[1L]],
             rgbv[[2L]],
             rgbv[[3L]],
             (1-transparency)*255L,
             maxColorValue = 255L));
}

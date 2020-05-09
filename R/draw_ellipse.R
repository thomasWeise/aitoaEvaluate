#' @title Draw an Ellipse
#' @description  Draw an ellipse around the specified center with the specified radii.
#' @param x.center the horizontal center
#' @param y.center the vertical center
#' @param x.radius the horizontal radius
#' @param y.radius the vertical radius
#' @param resolution the number of points to use to emulate the ellipse
#' @param ... the parameters to be passed to \link[graphics]{polygon}
#' @importFrom graphics polygon
#' @export aitoa.draw.ellipse
aitoa.draw.ellipse <- function(x.center=0,
                               y.center=x.center,
                               x.radius=1,
                               y.radius=x.radius,
                               resolution=201L, ...) {
  stopifnot(is.numeric(x.center),
            length(x.center) == 1L,
            is.finite(x.center),
            is.numeric(y.center),
            length(y.center) == 1L,
            is.finite(y.center),
            is.numeric(x.radius),
            length(x.radius) == 1L,
            is.finite(x.radius),
            is.numeric(y.radius),
            length(y.radius) == 1L,
            is.finite(y.radius),
            is.integer(resolution),
            length(resolution) == 1L,
            is.finite(resolution),
            resolution > 2L);
  theta <- seq.int(from=0L, to=2L * pi, length.out=(resolution+1L))[1L:resolution];
  polygon(x = x.center + (x.radius * cos(theta)),
          y = y.center + (y.radius * sin(theta)),
          ...);
}

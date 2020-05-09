#' @title The 2-dimensional Rastrigin Function
#' @param x the 2-dimensional input vector
#' @return the value of the Rastrigin function
#' @export aitoa.rastrigin
aitoa.rastrigin <- function(x) 10*(2-sum(cos(2*pi*x))) + sum(x^2)

#' @title The 2-dimensional Rosenbrock Function
#' @param x the 2-dimensional input vector
#' @return the value of the Rosenbrock function
#' @export aitoa.rosenbrock
aitoa.rosenbrock <- function(x) (100*((x[[1L]]^2-x[[2L]])^2)) + ((x[[1L]] - 1L)^2)

#' @title Create the Data Needed for Plotting a Function in 3D
#' @description Create the \code{x}, \code{y}, and \code{z} data needed to plot
#'   a function in 3D
#' @param x.min the minimum value of the x-coordinate
#' @param x.max the maximum value of the x-corrdinate
#' @param y.min the minimum value of the y-coordinate
#' @param y.max the maximum value of the y-corrdinate
#' @param n.points.x the number of points to generate along the x-axis
#' @param n.points.y the number of points to generate along the y-axis
#' @param f a function taking a 2-dimensional vector as input and returning the
#'   z-coordinate as output
#' @return a \code{list(x, y, z, grid)}, where \code{x} and \code{y} are
#'   numerical vectors of \code{x} and \code{y} coordinates and \code{z} is a
#'   matrix with the \code{z} coordinate for each \code{x-y} pair and
#'   \code{grid} is a matrix with two columns and one row, one row per
#'   \code{x-y} pair
#' @export aitoa.function.to.matrix
aitoa.function.to.matrix <- function(x.min=-5,
                                     x.max=if(x.min < 0) -x.min else if(x.min==0) 1 else max((x.min+1), 1.1*x.min),
                                     y.min=x.min,
                                     y.max=x.max,
                                     n.points.x=101L,
                                     n.points.y=n.points.x,
                                     f=aitoa.rosenbrock) {
  stopifnot(is.numeric(x.min),
            length(x.min) == 1L,
            is.numeric(x.max),
            length(x.max) == 1L,
            is.finite(x.min),
            is.finite(y.min),
            x.min < x.max,
            is.numeric(y.min),
            length(y.min) == 1L,
            is.numeric(y.max),
            length(y.max) == 1L,
            is.finite(y.min),
            is.finite(y.min),
            y.min < y.max,
            is.integer(n.points.x),
            length(n.points.x) == 1L,
            is.finite(n.points.x),
            n.points.x > 2L,
            is.integer(n.points.y),
            length(n.points.y) == 1L,
            is.finite(n.points.y),
            n.points.y > 2L,
            !is.null(f),
            is.function(f));

  x <- seq.int(from=x.min, to=x.max, length.out=n.points.x);
  if((x.min == y.min) && (x.max == y.max) && (n.points.y == n.points.x)) {
    y <- x;
  } else {
    y <- seq.int(from=y.min, to=y.max, length.out=n.points.y);
  }
  stopifnot(length(x) == n.points.x,
            length(y) == n.points.y,
            all(is.finite(x)),
            all(is.finite(y)));

  grid <- as.matrix(expand.grid(x, y));
  stopifnot(is.matrix(grid),
            ncol(grid) == 2L,
            nrow(grid) == n.points.x*n.points.y);
  z <- apply(grid, 1L, f);
  stopifnot(is.numeric(z),
            all(is.finite(z)),
            length(z) == n.points.x*n.points.y);

  z <- matrix(z,
              nrow=n.points.x,
              ncol=n.points.y);
  stopifnot(is.matrix(z),
            all(is.finite(z)),
            nrow(z) == n.points.x,
            ncol(z) == n.points.y);

  return(list(x=x, y=y, z=z, grid=grid));
}

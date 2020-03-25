#' @title Generate a Virtual Run based on a Statistic
#' @description For a given set of runs, selected x- and y-columns, a new run is
#'   computed whose x-coordinates are the unique x-coordinates from the original
#'   run and the corresponding y-coordinates are the specified statistic,
#'   computed over all the current values of the runs. Useless/duplicated points
#'   are deleted.
#' @param data the set of runs
#' @param x.column the name of the x column
#' @param y.column the name of the y column
#' @param stat.func the statistic function to be computed
#' @param make.stairs should stairs be computed?
#' @param x.min an optional minimal x-coordinate, for which a point will be
#'   generated
#' @param x.max an optional maximum x corrdinate, for which a point will be
#'   generated
#' @param na.replacement a value to be used instead if \code{NA}.
#' @return a matrix whose first column holds the x-coordinates and whose second
#'   column holds the y-coordinates, i.e., the computed statistics
#' @export aitoa.create.stat.run
aitoa.create.stat.run <- function(data,
                                  x.column=c("t", "fes", "f"),
                                  y.column=c("f", "t", "fes"),
                                  stat.func=mean,
                                  make.stairs=TRUE,
                                  x.min=NA_real_,
                                  x.max=NA_real_,
                                  na.replacement=Inf) {
# validate input
  stopifnot(!is.null(data),
            is.list(data),
            length(data) > 0L,
            !is.null(stat.func),
            is.function(stat.func),
            length(stat.func) == 1L,
            !is.null(make.stairs),
            is.logical(make.stairs),
            length(make.stairs) == 1L,
            !is.na(make.stairs),
            isTRUE(make.stairs) || isFALSE(make.stairs),
            !is.null(x.max),
            is.numeric(x.max),
            length(x.max) == 1L,
            !is.null(x.min),
            is.numeric(x.min),
            length(x.min) == 1L,
            is.na(x.min) || is.na(x.max) || (x.max > x.min),
            !is.null(na.replacement),
            is.numeric(na.replacement),
            length(na.replacement) == 1L);
  x.column <- match.arg(x.column);
  y.column <- match.arg(y.column);
  stopifnot(!is.null(x.column),
            is.character(x.column),
            length(x.column) == 1L,
            !is.na(x.column),
            x.column %in% c("t", "fes", "f"),
            !is.null(y.column),
            is.character(y.column),
            length(y.column) == 1L,
            !is.na(y.column),
            y.column %in% c("t", "fes", "f"),
            x.column != y.column);

# select the data
  data <- lapply(data, function(d) {
    stopifnot(is.data.frame(d),
              x.column %in% colnames(d),
              y.column %in% colnames(d),
              nrow(d) > 0L);
    x <- unname(unlist(d[x.column]));
    y <- unname(unlist(d[y.column]));
    stopifnot(is.numeric(x),
              is.numeric(y),
              length(x) == length(y),
              all(is.finite(x)),
              all(is.finite(y)));
    return(list(x=x, y=y));
  });

# gather all unique x-coordinates
  unique.x <- sort(unique(unlist(lapply(data, function(d) d$x))));
  if(!(is.na(x.min))) {
    unique.x <- unique.x[unique.x >= x.min];
    if((length(unique.x) <= 0L) ||
       (unique.x[[1L]] > x.min)) {
      unique.x <- c(x.min, unique.x);
    }
  }
  if(!(is.na(x.max))) {
    unique.x <- unique.x[unique.x <= x.max];
    if((length(unique.x) <= 0L) ||
       (unique.x[[length(unique.x)]] < x.max)) {
      unique.x <- c(unique.x, x.max);
    }
  }
  stopifnot(length(unique.x) > 0L,
            sum(is.finite(unique.x)) > 0L);

  if(x.column == "f") {
    unique.x <- rev(unique.x);
  }

# compute y value for each run at each x-coordinate
  if(x.column %in% c("t", "fes")) {
    expanded <- lapply(data, function(d) {
      xx <- d$x;
      yy <- d$y;
      indexes <- findInterval(unique.x, xx);
      y <- rep_len(if(y.column == "f") +Inf else 0L, length(unique.x));
      y[indexes > 0L] <- yy[indexes];
      return(y);
    });
  } else {
    expanded <- lapply(data, function(d) {
      xx <- d$x;
      yy <- d$y;
      indexes <- 1L + length(xx) - findInterval(unique.x, rev(xx));
      y <- rep_len(+Inf, length(unique.x));
      s <- indexes <= length(xx);
      y[s] <- yy[indexes][s];
      return(y);
    });
  }
  rm("data");

# compute the statistics
  y <- vapply(seq_along(unique.x), function(i)
               stat.func(vapply(expanded, `[[`, NA_real_, i)),
               NA_real_);
  x <- unique.x;
  rm("expanded");
  rm("unique.x");

  stopifnot(length(x) == length(y),
            length(x) >= 1L);

  if(!is.na(na.replacement)) {
    y[is.na(y)] <- na.replacement;
  }
  stopifnot(!any(is.na(y)),
            length(x) == length(y),
            length(x) >= 1L);

# remove redundant points
  keep <- !duplicated(y);
  keep[[length(keep)]] <- TRUE;
  keep[[1L]] <- TRUE;
  y <- y[keep];
  x <- x[keep];
  stopifnot(length(x) == length(y),
            length(x) >= 1L);

  # make stairs?
  if(make.stairs) {
    x <- unname(unlist(c(x[[1L]],
           lapply(seq.int(2L, length(x)),
           function(i) c(x[[i]], x[[i]]) ))));
    y <- unname(unlist(c(lapply(seq.int(1L, length(y)-1L),
                                function(i) c(y[[i]], y[[i]])),
                         y[[length(y)]])));

    l <- length(y);
    stopifnot(length(x) == l,
              l >= 1L);
    if(l >= 3L) {
      if(y[[l]] == y[[l - 2L]]) {
        stopifnot(y[[l - 1L]] == y[[l]]);
        l <- l - 1L;
        y <- y[-l];
        x <- x[-l];
      }
    }

    l <- length(y);
    stopifnot(length(x) == l,
              l >= 1L);
    if(l >= 3L) {
      if(y[[1L]] == y[[3L]]) {
        stopifnot(y[[2L]] == y[[1L]]);
        y <- y[-2L];
        x <- x[-2L];
      }
    }
  }
  stopifnot(length(x) == length(y),
            length(x) >= 1L);

  if(x.column == "f") {
    x <- rev(x);
    y <- rev(y);
  }

  res <- as.matrix(cbind(x, y));
  colnames(res) <- c(x.column, y.column);
  return(res);
}

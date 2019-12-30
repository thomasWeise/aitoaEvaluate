#' @importFrom bit64 as.integer64
.int64.max.dbl.int <- as.integer64((2 ^ 53) - 1);
.int64.0 <- as.integer64(0L);

#' @title Load a Single Log File
#' @description Load a log file and return the results as a data frame. The data
#'   frame will have three columns: \code{t} contains the time in milliseconds
#'   consumed since the start of the optimization process and is monotonously
#'   increasing. \code{fes} contains the number of objective function
#'   evaluations since the start of the optimization process and is strictly
#'   monotonously increasing. \code{f} contains the objective value and is
#'   strictly monotonously decreasing, with the except of the last point which
#'   might have the same objective value as the second-to-last point. Time and
#'   FEs have a resolution of 1 and a maximum value of \code{2^53 - 1}, as this
#'   is the highest integer value that can represented precisely with a double.
#'   If a higher value is encountered in any of the two columns, \code{stop}
#'   will be invoked, i.e., higher values are not permitted. In order to test
#'   for this, we use the 64 bit integers from the \code{bit64} package, since
#'   \code{R} does not support 64 bit integers natively.
#' @param file the log file to load
#' @param makeTimeUnique should we make the time indices unique (except maybe
#'   for the first and last point)? This makes sense when we want to plot
#'   diagrams over a time axis, as we then have removed redundant points right
#'   away. If \code{makeTimeUnique==FALSE}, then there may be multiple
#'   improvements at the same time index due to the resolution of the computer
#'   clock (while each improvement will definitely have a unique FE).
#' @return a data frame with the columns \code{t} (time in ms), \code{fes}
#'   (function evaluations), and \code{f} (objective value), all of which are
#'   numeric or integer valued, with integer type being preferred if it can be
#'   used without loss of precision
#' @importFrom bit64 as.integer64
#' @export aitoa.load.log.file
aitoa.load.log.file <- function(file,
                                makeTimeUnique=FALSE) {
  old.options <- options(warn=2);
  stopifnot(is.character(file),
            is.logical(makeTimeUnique));

  file <- normalizePath(file, mustWork=TRUE);
  file <- force(file);
  stopifnot(file.exists(file),
            file.size(file) > 100L);

  # read file as text file, one line = one element
  data <- readLines(con=file, warn=FALSE);
  data <- force(data);

  # detect consumed FEs
  consumedFEs <- grep("# CONSUMED_FES:", data, fixed = TRUE);
  consumedFEs <- force(consumedFEs);
  stopifnot(length(consumedFEs) == 1L);
  consumedFEs <- as.integer64(trimws(strsplit(data[consumedFEs[[1L]]],":", fixed=TRUE)[[1L]][2L]));
  consumedFEs <- force(consumedFEs);
  stopifnot(is.finite(consumedFEs),
            consumedFEs <= .int64.max.dbl.int,
            consumedFEs > .int64.0);
  consumedFEs <- as.numeric(consumedFEs);
  if(consumedFEs <= .Machine$integer.max) {
    .fes <- as.integer;
    consumedFEs <- as.integer(consumedFEs);
  } else {
    .fes <- as.numeric;
  }

  # detect consumed time
  consumedTime <- grep("# CONSUMED_TIME:", data, fixed = TRUE);
  consumedTime <- force(consumedTime);
  stopifnot(length(consumedTime) == 1L);
  consumedTime <- as.integer64(trimws(strsplit(data[consumedTime[[1L]]],":", fixed=TRUE)[[1L]][2L]));
  consumedTime <- force(consumedTime);
  stopifnot(is.finite(consumedTime),
            consumedTime <= .int64.max.dbl.int,
            consumedTime >= .int64.0);
  consumedTime <- as.numeric(consumedTime);
  if(consumedTime <= .Machine$integer.max) {
    .time <- as.integer;
    consumedTime <- as.integer(consumedTime);
  } else {
    .time <- as.numeric;
  }

  # extract the correct lines
  start <- grep("# BEGIN_LOG", data, fixed=TRUE)[[1L]];
  start <- force(start);
  end <- grep("# END_OF_LOG", data, fixed=TRUE)[[1L]];
  end <- force(end);
  stopifnot(start > 0L,
            end > (start + 2L),
            is.finite(start),
            is.finite(end));

  # load data as CSV
  data <- strsplit(data[(start+2L):(end-1L)], ";", fixed=TRUE);
  data <- force(data);
  f   <- as.numeric(vapply(data, `[[`, "", 1L));
  stopifnot(all(is.finite(f)));
  f   <- force(f);

  fes <- .fes(vapply(data, `[[`, "", 2L));
  fes <- force(fes);
  stopifnot(all(is.finite(fes)),
            all(fes > 0),
            all(ceiling(fes) == floor(fes)),
            all(fes <= consumedFEs));

  t   <- .time(vapply(data, `[[`, "", 3L));
  t   <- force(t);
  stopifnot(all(is.finite(t)),
            all(t >= 0),
            all(ceiling(t) == floor(t)),
            all(t <= consumedTime));
  rm("data");

  stopifnot(length(unique(fes)) == length(fes));

  startPointAdded <- FALSE;
  if(isTRUE(makeTimeUnique)) {
    # take care of re-occuring time values
    t.unique.indexes <- findInterval(unique(t), t);
    if(t.unique.indexes[1L] != 1L) {
      t.unique.indexes <- c(1L, t.unique.indexes);
      startPointAdded <- TRUE;
    }
    t <- t[t.unique.indexes];
    t <- force(t);
    f <- f[t.unique.indexes];
    f <- force(f);
    fes <- fes[t.unique.indexes];
    fes <- force(fes);
  }

  l <- length(t);
  stopifnot(l >= 1L);

  # add an end point if necessary
  endPointAdded <- FALSE;
  if(fes[l] < consumedFEs) {
    stopifnot(consumedTime >= t[l]);
    l <- l + 1L;
    f[l]   <- f[l - 1L];
    fes[l] <- consumedFEs;
    t[l]   <- consumedTime;
    endPointAdded <- TRUE;
  } else {
    stopifnot(t[l] <= consumedTime);
    consumedTime <- t[l];
  }

  stopifnot(fes[l] == consumedFEs,
              t[l] == consumedTime);

  minLength <- 1L;
  if(startPointAdded) { minLength <- minLength + 1L; }
  if(endPointAdded) { minLength <- minLength + 1L; }
  stopifnot(length(t) >= minLength,
            length(t) == length(f),
            length(f) == length(fes),
            length(t) == l,
            all(is.finite(t)),
            all(is.finite(f)),
            all(is.finite(fes)),
            all(is.numeric(t)),
            all(is.numeric(f)),
            all(is.numeric(fes)),
            all(fes > 0),
            all(fes <= consumedFEs),
            all(t >= 0),
            all(t <= consumedTime));

# check how the objective values progress: always decreasing
  if(l > 1L) {
    before <- 1L:(l-1L);
    after  <- 2L:l;
    stopifnot(all(f[before] >= f[after]),
              all(fes[before] <= fes[after]),
              all(t[before] <= t[after]));
  }

  if(endPointAdded) {
    if(l > 2L) {
      stopifnot(fes[1L:(l-2L)] < fes[2L:(l-1L)]);
    }
    stopifnot(f[l] == f[l-1L]);
  }
  if(startPointAdded) {
    stopifnot(t[1L] == t[2L],
              f[1L] >= f[2L]);
  }

  # OK, we should have valid data
  t <- force(t);
  fes <- force(fes);
  f <- force(f);

  # try to convert the data to the cheapest format
  if(all((f >= (-.Machine$integer.max)) & (f <= .Machine$integer.max))) {
    .ft <- as.integer(f);
    if(all(.ft == f)) {
      f <- .ft;
    }
  }

  # finished converting
  data <- data.frame(t=t, fes=fes, f=f);
  data <- force(data);
  rm("f");
  rm("fes");
  rm("t");

  data <- force(data);
  data <- do.call(force, list(x=data));
  stopifnot(is.data.frame(data),
            nrow(data) > 0L,
            ncol(data) == 3L,
            names(data) == c("t", "fes", "f"));
  options(old.options);

  return(data);
}

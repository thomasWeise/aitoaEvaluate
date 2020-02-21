#' @title Load a Directory with all Results
#' @description Load all the log files in an results directory recursively
#' @param results.dir the results directory
#' @param keep.columns the columns to keep, any vector containing elements
#'   \code{"t"} (for time), \code{"f"} (for the objective value), and
#'   \code{"fes"} (for the consumed FEs)
#' @param make.time.unique should we make the time indices unique (except maybe
#'   for the first and last point)? This makes sense when we want to plot
#'   diagrams over a time axis, as we then have removed redundant points right
#'   away. If \code{make.time.unique==FALSE}, then there may be multiple
#'   improvements at the same time index due to the resolution of the computer
#'   clock (while each improvement will definitely have a unique FE).
#' @return a list of list of list of data frames, each loaded via
#'   \link{aitoa.load.algo.dir}, where the names are the instance IDs
#' @export aitoa.load.results.dir
#' @include load_algorithm_dir.R
#' @seealso \link{aitoa.load.algo.dir}
aitoa.load.results.dir <- function(results.dir,
                                   keep.columns = c("fes", "t", "f"),
                                   make.time.unique=FALSE) {
  old.options <- options(warn=2);
  stopifnot(is.character(results.dir),
            is.character(keep.columns),
            length(keep.columns) > 0L,
            is.logical(make.time.unique));

  keep.columns <- unique(keep.columns);
  stopifnot(length(keep.columns) > 0L,
            all(keep.columns %in% c("fes", "t", "f")));

  results.dir <- normalizePath(results.dir, mustWork=TRUE);
  results.dir <- force(results.dir);
  stopifnot(dir.exists(results.dir));

  algoDirs <- list.dirs(path=results.dir,
                        full.names = TRUE,
                        recursive = FALSE);
  algoDirs <- sort(algoDirs);

  stopifnot(length(algoDirs) > 0L,
            length(unique(algoDirs)) == length(algoDirs));

  data <- lapply(algoDirs, aitoa.load.algo.dir,
                 keep.columns=keep.columns,
                 make.time.unique=make.time.unique);
  stopifnot(length(data) == length(algoDirs));

## verify results
  for(a in data) {
    stopifnot(is.list(a),
              length(a) > 0L);
    for(id in a) {
      stopifnot(is.list(id),
                length(id) > 0L);
      for(r in id) {
        stopifnot(is.data.frame(r),
                  colnames(r) == keep.columns,
                  nrow(r) > 0L);
      }
    }
  }

  algorithms <- vapply(data, function(n) attr(n[[1L]][[1L]], "algorithm"), NA_character_);
  stopifnot(length(algorithms) == length(data),
            length(unique(algorithms)) == length(data),
            all(nchar(algorithms) > 0L));
  names(data) <- algorithms;

  options(old.options);

  return(data);
}

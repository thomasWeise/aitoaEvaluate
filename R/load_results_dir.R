#' @title Load a Directory with all Results
#' @description Load all the log files in an results directory recursively
#' @param resultsDir the results directory
#' @param keepColumns the columns to keep, any vector containing elements
#'   \code{"t"} (for time), \code{"f"} (for the objective value), and
#'   \code{"fes"} (for the consumed FEs)
#' @param makeTimeUnique should we make the time indices unique (except maybe
#'   for the first and last point)? This makes sense when we want to plot
#'   diagrams over a time axis, as we then have removed redundant points right
#'   away. If \code{makeTimeUnique==FALSE}, then there may be multiple
#'   improvements at the same time index due to the resolution of the computer
#'   clock (while each improvement will definitely have a unique FE).
#' @return a list of list of list of data frames, each loaded via
#'   \link{aitoa.load.algo.dir}, where the names are the instance IDs
#' @export aitoa.load.results.dir
#' @include load_algorithm_dir.R
#' @seealso \link{aitoa.load.algo.dir}
aitoa.load.results.dir <- function(resultsDir,
                                  keepColumns = c("fes", "t", "f"),
                                  makeTimeUnique=FALSE) {
  old.options <- options(warn=2);
  stopifnot(is.character(resultsDir),
            is.character(keepColumns),
            length(keepColumns) > 0L,
            is.logical(makeTimeUnique));

  keepColumns <- unique(keepColumns);
  stopifnot(length(keepColumns) > 0L,
            all(keepColumns %in% c("fes", "t", "f")));

  resultsDir <- normalizePath(resultsDir, mustWork=TRUE);
  resultsDir <- force(resultsDir);
  stopifnot(dir.exists(resultsDir));

  algoDirs <- list.dirs(path=resultsDir,
                        full.names = TRUE,
                        recursive = FALSE);
  algoDirs <- sort(algoDirs);

  stopifnot(length(algoDirs) > 0L,
            length(unique(algoDirs)) == length(algoDirs));

  data <- lapply(algoDirs, aitoa.load.algo.dir,
                 keepColumns=keepColumns,
                 makeTimeUnique=makeTimeUnique);
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
                  colnames(r) == keepColumns,
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

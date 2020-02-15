#' @title Load a Directory with all Results for one Algorithm
#' @description Load all the log files in an algorithm directory recursively
#' @param algoDir the algorithm directory
#' @param keepColumns the columns to keep, any vector containing elements
#'   \code{"t"} (for time), \code{"f"} (for the objective value), and
#'   \code{"fes"} (for the consumed FEs)
#' @param makeTimeUnique should we make the time indices unique (except maybe
#'   for the first and last point)? This makes sense when we want to plot
#'   diagrams over a time axis, as we then have removed redundant points right
#'   away. If \code{makeTimeUnique==FALSE}, then there may be multiple
#'   improvements at the same time index due to the resolution of the computer
#'   clock (while each improvement will definitely have a unique FE).
#' @return a list of list of data frames, each loaded via
#'   \link{aitoa.load.inst.dir}, where the names are the instances
#' @export aitoa.load.algo.dir
#' @seealso \link{aitoa.load.inst.dir}
#' @include load_instance_dir.R
aitoa.load.algo.dir <- function(algoDir,
                                keepColumns = .default.colums,
                                makeTimeUnique=FALSE) {
  old.options <- options(warn=2);
  stopifnot(is.character(algoDir),
            is.character(keepColumns),
            length(keepColumns) > 0L,
            is.logical(makeTimeUnique));

  keepColumns <- unique(keepColumns);
  stopifnot(length(keepColumns) > 0L,
            all(keepColumns %in% .default.colums));

  algoDir <- normalizePath(algoDir, mustWork=TRUE);
  algoDir <- force(algoDir);
  stopifnot(dir.exists(algoDir));

  instDirs <- list.dirs(path=algoDir,
                        full.names = TRUE,
                        recursive = FALSE);

  stopifnot(length(instDirs) > 0L);

  instDirs <- sort(instDirs);
  stopifnot(length(instDirs) == length(unique(instDirs)));

  data <- lapply(instDirs,
    aitoa.load.inst.dir,
    keepColumns=keepColumns,
    makeTimeUnique=makeTimeUnique);
  stopifnot(length(data) == length(instDirs));

## verify results
  for(id in data) {
    stopifnot(is.list(id),
              length(id) > 0L);
    for(r in id) {
      stopifnot(is.data.frame(r),
                colnames(r) == keepColumns,
                nrow(r) > 0L);
    }
  }

  instances <- vapply(data, function(n) attr(n[[1L]], "instance"), NA_character_);
  stopifnot(length(instances) == length(data),
            length(unique(instances)) == length(data),
            all(nchar(instances) > 0L));
  names(data) <- instances;

  options(old.options);

  return(data);
}

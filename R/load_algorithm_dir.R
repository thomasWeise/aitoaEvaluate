#' @title Load a Directory with all Results for one Algorithm
#' @description Load all the log files in an algorithm directory recursively
#' @param algo.dir the algorithm directory
#' @param keep.columns the columns to keep, any vector containing elements
#'   \code{"t"} (for time), \code{"f"} (for the objective value), and
#'   \code{"fes"} (for the consumed FEs)
#' @param make.time.unique should we make the time indices unique (except maybe
#'   for the first and last point)? This makes sense when we want to plot
#'   diagrams over a time axis, as we then have removed redundant points right
#'   away. If \code{make.time.unique==FALSE}, then there may be multiple
#'   improvements at the same time index due to the resolution of the computer
#'   clock (while each improvement will definitely have a unique FE).
#' @return a list of list of data frames, each loaded via
#'   \link{aitoa.load.inst.dir}, where the names are the instances
#' @export aitoa.load.algo.dir
#' @seealso \link{aitoa.load.inst.dir}
#' @include load_instance_dir.R
aitoa.load.algo.dir <- function(algo.dir,
                                keep.columns = c("fes", "t", "f"),
                                make.time.unique=FALSE) {
  old.options <- options(warn=2);
  stopifnot(is.character(algo.dir),
            is.character(keep.columns),
            length(keep.columns) > 0L,
            is.logical(make.time.unique));

  keep.columns <- unique(keep.columns);
  stopifnot(length(keep.columns) > 0L,
            all(keep.columns %in% c("fes", "t", "f")));

  algo.dir <- normalizePath(algo.dir, mustWork=TRUE);
  algo.dir <- force(algo.dir);
  stopifnot(dir.exists(algo.dir));

  instDirs <- list.dirs(path=algo.dir,
                        full.names = TRUE,
                        recursive = FALSE);

  stopifnot(length(instDirs) > 0L);

  instDirs <- sort(instDirs);
  stopifnot(length(instDirs) == length(unique(instDirs)));

  data <- lapply(instDirs,
    aitoa.load.inst.dir,
    keep.columns=keep.columns,
    make.time.unique=make.time.unique);
  stopifnot(length(data) == length(instDirs));

## verify results
  for(id in data) {
    stopifnot(is.list(id),
              length(id) > 0L);
    for(r in id) {
      stopifnot(is.data.frame(r),
                colnames(r) == keep.columns,
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

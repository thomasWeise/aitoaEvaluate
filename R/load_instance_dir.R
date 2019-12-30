#' @title Load an Instance Directory
#' @description Load all the log files in a directory
#' @param instDir the instance directory
#' @param keepColumns the columns to keep, any vector containing elements
#'   \code{"t"} (for time), \code{"f"} (for the objective value), and
#'   \code{"fes"} (for the consumed FEs)
#' @param makeTimeUnique should we make the time indices unique (except maybe
#'   for the first and last point)? This makes sense when we want to plot
#'   diagrams over a time axis, as we then have removed redundant points right
#'   away. If \code{makeTimeUnique==FALSE}, then there may be multiple
#'   improvements at the same time index due to the resolution of the computer
#'   clock (while each improvement will definitely have a unique FE).
#' @return a list of data frames, where the names are the random seeds
#' @export aitoa.load.inst.dir
#' @include load_log_file.R
aitoa.load.inst.dir <- function(instDir,
                                keepColumns = .default.colums,
                                makeTimeUnique=FALSE) {
  old.options <- options(warn=2);
  stopifnot(is.character(instDir),
            is.character(keepColumns),
            length(keepColumns) > 0L,
            is.logical(makeTimeUnique));

  keepColumns <- unique(keepColumns);
  stopifnot(length(keepColumns) > 0L,
            all(keepColumns %in% .default.colums));

  instDir <- normalizePath(instDir, mustWork=TRUE);
  instDir <- force(instDir);
  stopifnot(dir.exists(instDir));

  files <- list.files(path=instDir, pattern=".txt", all.files = FALSE,
                      full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                      include.dirs = FALSE, no..=TRUE);
  stopifnot(length(files) > 0L);
  names <- vapply(files, function(s) {
    start <- gregexpr("_", s, fixed=TRUE);
    stopifnot(length(start) > 0L);
    start <- as.integer(start[[length(start)]]);
    stopifnot(length(start) > 0L);
    start <- as.integer(start[[length(start)]]);
    stopifnot(start > 0L);
    end <- gregexpr(".", s, fixed=TRUE);
    stopifnot(length(end) > 0L);
    end <- as.integer(end[[length(end)]]);
    stopifnot(length(end) > 0L);
    end <- as.integer(end[[length(end)]]);
    stopifnot(end > 0L,
              end > (start + 1L),
              end == nchar(s) - 3L);
    hc <-substr(s, start + 1L, end - 1L);
    stopifnot(startsWith(hc, "0x"));
    return(hc);
  }, NA_character_);
  stopifnot(length(names) > 0L,
            length(unique(names)) == length(names));

  data <- lapply(files, function(s) {
    aitoa.load.log.file(file=file.path(instDir, s),
                        keepColumns=keepColumns,
                        makeTimeUnique=makeTimeUnique);
  });
  stopifnot(length(data) == length(names));

  o <- order(names);
  data <- data[o];
  names <- names[o];

  names(data) <- names;


  ## verify results
  for(r in data) {
    stopifnot(is.data.frame(r),
              colnames(r) == keepColumns,
              nrow(r) > 0L);
  }

  options(old.options);

  return(data);
}

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
#' @return a list of data frames, each loaded with \link{aitoa.load.log.file},
#' where the "names" are the random seeds
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
  instName <- basename(instDir);
  stopifnot(nchar(instName) > 0L);

  files <- list.files(path=instDir,
                      pattern=".txt",
                      all.files = FALSE,
                      full.names = TRUE,
                      recursive = FALSE,
                      ignore.case = FALSE,
                      include.dirs = FALSE,
                      no..=TRUE);
  files <- sort(files);
  stopifnot(length(files) == length(unique(files)));

  data <- lapply(files,
                 aitoa.load.log.file,
                 keepColumns=keepColumns,
                 makeTimeUnique=makeTimeUnique);
  stopifnot(length(data) == length(files));

  ## verify results
  for(i in seq_along(data)) {
    r <- data[[i]];
    stopifnot(is.data.frame(r),
              colnames(r) == keepColumns,
              nrow(r) > 0L,
              identical(attr(r, "instance"), instName),
              is.character(attr(r, "seed")),
              is.character(attr(r, "algorithm")),
              identical(attr(r, "file"), files[[i]]));
  }

  stopifnot(all(attr(data[[1L]], "instance") ==
                vapply(data, function(n) attr(n, "instance"), NA_character_)),
            all(attr(data[[1L]], "algorithm") ==
                  vapply(data, function(n) attr(n, "algorithm"), NA_character_)));

  seeds <- vapply(data, function(n) attr(n, "seed"), NA_character_);
  stopifnot(length(unique(seeds)) == length(data),
            length(data) == length(seeds));

  names(data) <- seeds;

  options(old.options);

  return(data);
}

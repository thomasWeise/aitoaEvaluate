#' @title Load an Instance Directory
#' @description Load all the log files in a directory
#' @param inst.dir the instance directory
#' @param keep.columns the columns to keep, any vector containing elements
#'   \code{"t"} (for time), \code{"f"} (for the objective value), and
#'   \code{"fes"} (for the consumed FEs)
#' @param make.time.unique should we make the time indices unique (except maybe
#'   for the first and last point)? This makes sense when we want to plot
#'   diagrams over a time axis, as we then have removed redundant points right
#'   away. If \code{make.time.unique==FALSE}, then there may be multiple
#'   improvements at the same time index due to the resolution of the computer
#'   clock (while each improvement will definitely have a unique FE).
#' @param f.must.be.improving \code{true} if the logged objective values must be
#'   strictly improving? This is the default way logs are generated by aitoa.
#'   However, you can also create a log where every single sampled solution is
#'   logged, so then you must set \code{f.must.be.improving=FALSE} to load the
#'   data.
#' @param max.runs.to.load an optional limit for the maximum number of runs to
#'     load
#' @return a list of data frames, each loaded with \link{aitoa.load.log.file},
#' where the "names" are the random seeds
#' @export aitoa.load.inst.dir
#' @include load_log_file.R
#' @include utils.R
aitoa.load.inst.dir <- function(inst.dir,
                                keep.columns = c("fes", "t", "f"),
                                make.time.unique=FALSE,
                                f.must.be.improving=TRUE,
                                max.runs.to.load=NA_integer_) {

  old.options <- options(warn=2);

  stopifnot(!is.null(inst.dir),
            is.character(inst.dir),
            length(inst.dir) == 1L,
            !is.na(inst.dir),
            is.character(keep.columns),
            !is.null(keep.columns),
            is.character(keep.columns),
            length(keep.columns) > 0L,
            !any(is.na(keep.columns)),
            !is.null(make.time.unique),
            is.logical(make.time.unique),
            length(make.time.unique) == 1L,
            isTRUE(make.time.unique) || isFALSE(make.time.unique),
            !is.null(f.must.be.improving),
            is.logical(f.must.be.improving),
            length(f.must.be.improving) == 1L,
            isTRUE(f.must.be.improving) || isFALSE(f.must.be.improving));

  keep.columns <- unique(keep.columns);
  stopifnot(length(keep.columns) > 0L,
            all(keep.columns %in% c("fes", "t", "f")));

  if(is.null(max.runs.to.load)) {
    max.runs.to.load <- NA_integer_;
  }
  stopifnot(is.integer(max.runs.to.load),
            is.na(max.runs.to.load) || (
              is.finite(max.runs.to.load) &&
              (max.runs.to.load > 0L)));

  inst.dir <- .dir.exists(inst.dir);
  inst.dir <- force(inst.dir);
  instName <- basename(inst.dir);
  stopifnot(nchar(instName) > 0L);

  files <- list.files(path=inst.dir,
                      pattern=".txt",
                      all.files = FALSE,
                      full.names = TRUE,
                      recursive = FALSE,
                      ignore.case = FALSE,
                      include.dirs = FALSE,
                      no..=TRUE);
  files <- sort(files);
  stopifnot(length(files) == length(unique(files)));
  if(!is.na(max.runs.to.load)) {
    if(max.runs.to.load < length(files)) {
      files <- unname(unlist(files[seq_len(max.runs.to.load)]));
      files <- force(files);
    }
    stopifnot(length(files) <= max.runs.to.load);
  }
  stopifnot(is.character(files),
            length(files) > 0L);

  data <- lapply(files,
                 aitoa.load.log.file,
                 keep.columns=keep.columns,
                 make.time.unique=make.time.unique,
                 f.must.be.improving=f.must.be.improving);
  stopifnot(length(data) == length(files));

  ## verify results
  for(i in seq_along(data)) {
    r <- data[[i]];
    stopifnot(is.data.frame(r),
              colnames(r) == keep.columns,
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

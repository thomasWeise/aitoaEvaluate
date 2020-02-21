
#' @title Load the End Result of a Run whose Performance Coincidents with a
#'   given Statistic
#' @description Load the end results of a run whose performance corresponds
#'   closely to a given statistic. For instance, by default, this function would
#'   load the end result of the run where the quality of best discovered
#'   solution is closest to the median best quality over all runs of the given
#'   algorithm/instance combination. Basically, this method links the setups
#'   discoverd with \link{aitoa.load.end.result.stats} to the routine
#'   \code{aitoa.load.result.from.log.file}.
#' @param end.result.stats the data frame with the end result statistics,
#'   obtained via \link{aitoa.load.end.result.stats}
#' @param results.dir the directory with the end results
#' @param algorithm the algorithm id
#' @param instance the instance if
#' @param statistic the statistic for which the matching run is sought
#' @return a list with the following fields:\describe{ \item{file}{the file from
#'   which the result was loaded} \item{consumed.fes}{the total number of
#'   consumed function evaluations} \item{last.improvement.fe}{the function
#'   evaluation where the last improvement took palce} \item{consumed.time}{the
#'   total consumed time in milliseconds} \item{last.improvement.time}{the time
#'   when the last improved solution was found} \item{best.f}{the objective
#'   value of the best solution found} \item{best.y}{the contents of the BEST_Y
#'   section, i.e., the string representation of the best solution found}
#'   \item{best.x}{if the BEST_X section is present, then its contents
#'   (otherwise this key is not present). If this key is present, then it holds
#'   the string representation of the internal encoding of the best solution
#'   found}\item{setup}{the setup corresponding to the run}\item{statistic}{the
#'   value of the parameter \code{statistic}}\item{statistic.value}{the value of
#'   that statistic, which could be different of the performance indicator of
#'   the run}}
#' @include load_end_result_stats.R
#' @include load_result_from_log_file.R
#' @export aitoa.load.stat.result
aitoa.load.stat.result <- function(end.result.stats,
                                   results.dir,
                                   algorithm,
                                   instance,
                                   statistic=c("best.f.median",
                                               "best.f.min",
                                               "best.f.q050",
                                               "best.f.q159",
                                               "best.f.q250",
                                               "best.f.q750",
                                               "best.f.q841",
                                               "best.f.q950",
                                               "best.f.max",
                                               "best.f.mean",
                                               "success.time.min",
                                               "success.time.max",
                                               "success.fes.min",
                                               "success.fes.max")) {
  .check.end.result.stats(end.result.stats);
  stopifnot(is.character(results.dir),
            !is.na(results.dir),
            length(results.dir) == 1L,
            nchar(results.dir) > 0L,
            is.character(instance),
            !is.na(instance),
            length(instance) == 1L,
            nchar(instance) > 0L,
            is.character(algorithm),
            !is.na(algorithm),
            length(algorithm) == 1L,
            nchar(algorithm) > 0L);
  statistic <- match.arg(statistic);
  stopifnot(is.character(statistic),
            nchar(statistic) > 0L);
  results.dir <- normalizePath(results.dir, mustWork = TRUE);
  stopifnot(is.character(results.dir),
            length(results.dir) == 1L,
            !is.na(results.dir),
            nchar(results.dir) > 0L,
            dir.exists(results.dir));

  found <- (end.result.stats$algorithm == algorithm) &
           (end.result.stats$instance == instance);
  stopifnot(sum(found) == 1L);
  found <- which(found);
  stopifnot(is.integer(found),
            length(found) == 1L,
            found > 0L,
            found <= nrow(end.result.stats));

  stat.value <- unname(unlist(end.result.stats[found, statistic]));
  stopifnot(is.numeric(stat.value),
            length(stat.value) == 1L,
            !is.na(stat.value),
            is.finite(stat.value));
  if((stat.value > -.Machine$integer.max) &&
     (stat.value < .Machine$integer.max)) {
    d <- as.integer(round(stat.value));
    if(is.finite(d) && (d == stat.value)) {
      stat.value <- d;
      stopifnot(is.integer(stat.value),
                is.finite(stat.value));
    }
  }

  setup <- unname(unlist(end.result.stats[found, paste0(statistic, ".setup")]));
  stopifnot(is.character(setup),
            length(setup) == 1L,
            !is.na(setup),
            nchar(setup) >= 5L);

  file <- strsplit(setup, "/", fixed=TRUE);
  stopifnot(length(file) == 1L);
  file <- file[[1L]];
  stopifnot(is.character(file),
            length(file) == 3L,
            !any(is.na(file)),
            all(nchar(file) > 0L));

  file <- file.path(results.dir,
                    file[[1L]],
                    file[[2L]],
                    paste0(file[[1L]], "_",
                           file[[2L]], "_",
                           file[[3L]], ".txt"));
  stopifnot(is.character(file),
            !is.na(file),
            length(file) == 1L,
            nchar(file) > 0L);
  file <- normalizePath(file, mustWork = TRUE);
  stopifnot(is.character(file),
            !is.na(file),
            length(file) == 1L,
            nchar(file) > 0L,
            file.exists(file),
            file.size(file) > 100L);

  result <- aitoa.load.result.from.log.file(file);

  result <- force(result);
  stopifnot(is.list(result),
            length(result) >= 7L,
            length(result) <= 8L);

  result$statistic <- statistic;
  result$statistic.value <- stat.value;
  result$setup <- setup;

  result <- force(result);
  stopifnot(is.list(result),
            length(result) >= 10L,
            length(result) <= 11L);

  return(result);
}

#' @title Parse the Result of a JSSP Experiment
#' @description Parse the final solution of a run in the JSSP experiment into a
#'   form that can be processed by \link{aitoa.plot.gantt}.
#' @param y the final result of the experiment, as returned by
#'   \code{aitoa.load.result.from.log.file(..)$best.y}
#' @return the list of lists of lists that can be processed by
#'   \link{aitoa.plot.gantt}.
#' @seealso \link{aitoa.plot.gantt}, \link{aitoa.load.result.from.log.file}
#' @include plot_gantt.R
#' @export aitoa.jssp.parse.result
aitoa.jssp.parse.result <- function(y) {
  stopifnot(is.character(y),
            !is.na(y),
            length(y) > 0);
  y <- trimws(y);
  stopifnot(sum(nchar(y)) > 100L);

  start <- grep("plot.gantt(", y, fixed=TRUE);
  stopifnot(is.integer(start),
            length(start) == 1L,
            is.finite(start),
            start > 0L,
            start + 2L < length(y));
  y <- y[(start+1L):(length(y)-1L)];
  stopifnot(is.character(y),
            !is.na(y),
            length(y) > 0,
            sum(nchar(y)) > 100L);

  y <- paste0(c("list(", y, ")"), sep="", collapse="");
  stopifnot(is.character(y),
            length(y) == 1L,
            nchar(y) > 0L);
  data <- parse(text=y, keep.source = FALSE);
  data <- force(data);
  stopifnot(is.expression(data));
  data <- eval(data);
  data <- force(data);

  .check.gantt.data(data);

  return(data);
}

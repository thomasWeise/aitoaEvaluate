.extract <- function(text, start, end, optional) {
  start <- grep(start, text, fixed=TRUE);
  stopifnot(is.integer(start));
  if(optional && (length(start) <= 0L)) {
    return(NULL);
  }
  stopifnot(length(start) == 1L,
            is.finite(start),
            start > 0L);

  end <- grep(end, text, fixed=TRUE);
  stopifnot(is.integer(end),
            length(end) == 1L,
            end > (start + 1L),
            end <= length(text));
  text <- text[(start + 1L):(end - 1L)];
  stopifnot(is.character(text),
            length(text) > 0L,
            sum(nchar(text)) > 0L);
  return(text);
}

.parse.item <- function(state,
                        key) {
  i <- grep(key, state, fixed = TRUE);
  stopifnot(is.integer(i),
            length(i) == 1L,
            i > 0L,
            i <= length(state));
  s <- trimws(state[[i]]);
  stopifnot(startsWith(s, key));
  s <- trimws(substring(s, nchar(key)+ 1L, nchar(s)));
  stopifnot(is.character(s),
            nchar(s) > 0L);

  result <- as.numeric(s);
  stopifnot(!is.na(result),
             is.finite(result));
  return(result);
}



#' @title Load the End Result from a Log File
#' @description  Load the end result of a run from a log file.
#' @param file the file to load
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
#'   found}}
#' @export aitoa.load.result.from.log.file
aitoa.load.result.from.log.file <- function(file) {
  stopifnot(is.character(file),
            length(file) == 1L,
            !is.na(file),
            nchar(file) >= 5L);
  file <- normalizePath(file, mustWork = TRUE);
  stopifnot(is.character(file),
            !is.na(file),
            length(file) == 1L,
            nchar(file) > 0L,
            file.exists(file),
            file.size(file) > 100L);

  text <- readLines(con=file, warn = FALSE);
  stopifnot(is.character(text),
            length(text) > 0L);

  state <- trimws(.extract(text,
                    "# BEGIN_STATE",
                    "# END_STATE", FALSE));
  stopifnot(is.character(state),
            length(state) >= 5L);

  consumed.fes <- .parse.item(state, "# CONSUMED_FES:");
  stopifnot(consumed.fes > 0L);
  if(consumed.fes < .Machine$integer.max) {
    consumed.fes <- as.integer(round(consumed.fes));
    stopifnot(is.integer(consumed.fes),
              is.finite(consumed.fes),
              consumed.fes > 0L);
  }

  last.improvement.fe <- .parse.item(state, "# LAST_IMPROVEMENT_FE:");
  stopifnot(last.improvement.fe > 0L,
            last.improvement.fe <= consumed.fes);
  if(last.improvement.fe < .Machine$integer.max) {
    last.improvement.fe <- as.integer(round(last.improvement.fe));
    stopifnot(is.integer(last.improvement.fe),
              is.finite(last.improvement.fe),
              last.improvement.fe > 0L,
              last.improvement.fe <= consumed.fes);
  }

  consumed.time <- .parse.item(state, "# CONSUMED_TIME:");
  stopifnot(consumed.time >= 0L);
  if(consumed.time < .Machine$integer.max) {
    consumed.time <- as.integer(round(consumed.time));
    stopifnot(is.integer(consumed.time),
              is.finite(consumed.time),
              consumed.time >= 0L);
  }

  last.improvement.time <- .parse.item(state, "# LAST_IMPROVEMENT_TIME:");
  stopifnot(last.improvement.time >= 0L,
            last.improvement.time <= consumed.time);
  if(last.improvement.time < .Machine$integer.max) {
    last.improvement.time <- as.integer(round(last.improvement.time));
    stopifnot(is.integer(last.improvement.time),
              is.finite(last.improvement.time),
              last.improvement.time >= 0L,
              last.improvement.time <= consumed.time);
  }
  best.f <- .parse.item(state, "# BEST_F:");
  if((best.f > (-.Machine$integer.max)) &&
     (best.f < .Machine$integer.max)) {
    d <- as.integer(round(best.f));
    if(is.finite(d) && (d == best.f)) {
      best.f <- d;
      stopifnot(is.integer(best.f),
                is.finite(best.f));
    }
  }

  best.y <- .extract(text,
                     "# BEST_Y",
                     "# END_BEST_Y",
                     FALSE);
  stopifnot(!is.null(best.y),
            length(best.y) > 0L,
            sum(nchar(best.y)) > 0L);

  result <- list(file = file,
                 consumed.fes = consumed.fes,
                 last.improvement.fe = last.improvement.fe,
                 consumed.time = consumed.time,
                 last.improvement.time = last.improvement.time,
                 best.f = best.f,
                 best.y = best.y);

  best.x <- .extract(text,
                     "# BEST_X",
                     "# END_BEST_X",
                     TRUE);
  if(!is.null(best.x)) {
    stopifnot(length(best.x) > 0L,
              sum(nchar(best.x)) > 0L);
    result$best.x <- best.x;
  }

  result <- force(result);
  stopifnot(is.list(result),
            length(result) >= 8L,
            length(result) <= 9L);
  return(result);
}

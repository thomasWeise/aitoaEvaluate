
.transform.log <- function(v, prep) {
  stopifnot(is.numeric(v),
            length(v) == 1L,
            is.finite(v));
  y <- as.integer(prep(log10(v)));
  p <- 10L ^ y;
  x <- as.character(signif(v/p, 3L));
  if(nchar(x) < 4L) {
    if(nchar(x) == 1L) {
      x <- paste0(x, ".00");
    } else {
      stopifnot(nchar(x) == 3L);
      x <- paste0(x, "0");
    }
  }
  r <- paste0("$", x , "\\!\\cdot\\!10^{", y, "}$");
  stopifnot(is.character(r),
            length(r) == 1L,
            nchar(r) > 0L);
  return(r);
}

#' @title Format a Large Number to a Text
#' @description Transform a large but finite number to a pleasant string.
#' @param v the number
#' @return the string representation
#' @export aitoa.format.large.number
aitoa.format.large.number <- function(v) {
  .transform.log(v, identity)
}

#' @title Format a Small Number to a Text
#' @description Transform a small but finite number to a pleasant string.
#' @param v the number
#' @return the string representation
#' @export aitoa.format.small.number
aitoa.format.small.number <- function(v) {
  stopifnot(is.numeric(v),
            length(v) == 1L,
            is.finite(v));
  r <- format(v, nsmall=0, big.mark="'");
  stopifnot(is.character(r),
            length(r) == 1L,
            nchar(r) > 0L);
  return(r);
}


#' @title Format a Fractional Number to a Text
#' @description Transform a finite number from (-1, 1) to a pleasant string.
#' @param v the number
#' @return the string representation
#' @export aitoa.format.fractional.number
aitoa.format.fractional.number <- function(v) {
  stopifnot(is.numeric(v),
            length(v) == 1L,
            is.finite(v));
  r <- as.character(v);
  stopifnot(is.character(r),
            length(r) == 1L,
            nchar(r) > 0L);
  if((nchar(r) <= 4L) &&
     (!grepl("e", tolower(r), fixed=TRUE))) {
    return(r);
  }
  return(.transform.log(v, floor));
}

#' @title Format a Number for Presentation in Markdown
#' @description Format a number such that it can suitably be presented in Markdown text.
#' @param n the number
#' @param finite.preprocessor a function applied to all finite numbers
#' @param integer.preprocessor a function applied to all integer numbers
#' @param real.preprocessor a function applied to all real numbers
#' @param format.large a formatter for large numbers (larger than 10'000'000'000 in absolute value)
#' @param format.small a formatter for numbers which are small
#' @param format.fraction a formatter for fractional numbers in (-1, 1)
#' @param const.na the string constant for NA
#' @param const.p.inf the string constant for positive infinity
#' @param const.n.inf the string constant for negative infinity
#' @param const.0 the string constant for 0
#' @param const.1 the string constant for 1
#' @param const.m1 the string constant for -1
#' @return a string representation of the number
#' @export aitoa.format.number.markdown
aitoa.format.number.markdown <- function(n,
                                         finite.preprocessor=identity,
                                         real.preprocessor=identity,
                                         integer.preprocessor=real.preprocessor,
                                         format.large=aitoa.format.large.number,
                                         format.small=aitoa.format.small.number,
                                         format.fraction=aitoa.format.fractional.number,
                                         const.na="N/A",
                                         const.p.inf="$\\infty$",
                                         const.n.inf="$-\\infty$",
                                         const.0="0",
                                         const.1="1",
                                         const.m1="-1") {
  stopifnot(is.numeric(n),
            length(n) == 1L);
  if(is.na(n)) {
    return(const.na);
  }
  if(n >= (+Inf)) {
    return(const.p.inf);
  }
  if(n <= (-Inf)) {
    return(const.n.inf);
  }
  stopifnot(is.finite(n));
  n <- finite.preprocessor(n);
  stopifnot(is.finite(n));

  if(!is.integer(n)) {
    if((n > (-.Machine$integer.max)) && (n < .Machine$integer.max)) {
      i <- as.integer(round(n));
      if(i == n) {
        n <- i;
      }
    }
  }

  if(is.integer(n)) {
    n <- integer.preprocessor(n);
  } else {
    n <- real.preprocessor(n);
  }
  stopifnot(is.finite(n));

  if(n==0L) {
    return(const.0);
  }
  if(n==1L) {
    return(const.1);
  }
  if(n==(-1L)) {
    return(const.m1);
  }

  if(is.integer(n)) {
    return(format.small(n));
  }

  if(abs(n) < 1) {
    return(format.fraction(n));
  }

  if((abs(n) > 1e10) ||
     grepl("e", tolower(as.character(n)), fixed=TRUE)) {
    return(format.large(n));
  }
  return(format.small(n));
}


#' @title Format a Time
#' @description format a time value
#' @param t the time number
#' @return the formatted time string
#' @export aitoa.format.time
aitoa.format.time <- function(t) {
  return(aitoa.format.number.markdown(t,
          finite.preprocessor=function(tt) round(tt, 0)));
}


#' @title Format an Integer-based Objective Value
#' @description format an integer objective value
#' @param f the objective value
#' @return the formatted string
#' @export aitoa.format.integer.objective.value
aitoa.format.integer.objective.value <- function(t) {
  return(aitoa.format.number.markdown(t,
           finite.preprocessor=function(tt) round(tt, 0)));
}


#' @title Format a Small Integer-based Objective Value
#' @description format a small integer objective value
#' @param f the objective value
#' @return the formatted string
#' @export aitoa.format.small.integer.objective.value
aitoa.format.small.integer.objective.value <- function(t) {
  return(as.character(as.integer(round(t))));
}

#' @title Format an improvement statistic
#' @description format an improvement counter value
#' @param t the time number
#' @return the formatted improvement string
#' @export aitoa.format.improvements
aitoa.format.improvements <- function(t) {
  return(aitoa.format.number.markdown(t,
          finite.preprocessor=function(tt) round(tt, 1)));
}

#' @title Format an integer counter
#' @description format an integer counter value
#' @param t the time number
#' @return the formatted string
#' @export aitoa.format.counter
aitoa.format.counter <- function(t) {
  return(aitoa.format.small.number(as.integer(round(t))));
}

#' @title Format a Setup
#' @param x the setup
#' @return the formatted string
#' @export aitoa.format.setup
aitoa.format.setup <- function(x) paste0("`", trim(as.character(x)), "`")

#' @title Convert a Column Type to a Formatter Function
#' @param col the column type
#' @param format.time the time formatting function
#' @param format.fes the FEs formatting function
#' @param format.improvements the improvements formatting function
#' @param format.objective the objective formatting function
#' @param format.counter the counter formatting function
#' @param format.setup the setup formatting function
#' @return the formatter function
#' @export aitoa.statistics.col.to.format
aitoa.statistics.col.to.format <- function(col,
      format.time=aitoa.format.time,
      format.fes=aitoa.format.fes,
      format.improvements=aitoa.format.improvements,
      format.objective=aitoa.format.small.integer.objective.value,
      format.counter=aitoa.format.counter,
      format.setup=aitoa.format.setup) {
  stopifnot(!is.null(col),
            is.character(col),
            length(col) == 1L,
            !is.na(col),
            nchar(col) > 0L);
  if(endsWith(col, ".setup") ||
     startsWith(col, "instance") ||
     startsWith(col, "algorithm") ) {
    return(aitoa.format.setup);
  }
  if(startsWith(col, "last.improvement.time") ||
     startsWith(col, "total.time") ||
     startsWith(col, "budget.time") ||
     startsWith(col, "success.time")||
     startsWith(col, "ert.time")) {
    stopifnot(!is.null(format.time),
              is.function(format.time));
     return(format.time);
  }
  if(startsWith(col, "last.improvement.fe") ||
     startsWith(col, "total.fes") ||
     startsWith(col, "budget.fes") ||
     startsWith(col, "success.fes") ||
     startsWith(col, "ert.fes")) {
    stopifnot(!is.null(format.fes),
              is.function(format.fes));
    return(format.fes);
  }
  if(startsWith(col, "best.f")) {
    stopifnot(!is.null(format.objective),
              is.function(format.objective));
    return(format.objective);
  }
  if(startsWith(col, "n.improvements")) {
    stopifnot(!is.null(format.counter),
              is.function(format.counter));
    return(format.improvements);
  }
  if(startsWith(col, "n.successes") ||
     startsWith(col, "n.runs")) {
    stopifnot(!is.null(format.counter),
              is.function(format.counter));
    return(format.counter);
  }
  stop(paste0("unknown column type: ", col));
}

#' @title Convert a Column Type to a Formatter Function
#' @param col the column type
#' @param format.time the time formatting function
#' @param format.fes the FEs formatting function
#' @param format.improvements the improvements formatting function
#' @param format.objective the objective formatting function
#' @param format.counter the counter formatting function
#' @param format.setup the setup formatting function
#' @return the formatter function
#' @export aitoa.statistics.cols.to.formats
aitoa.statistics.cols.to.formats <- function(cols,
                 format.time=aitoa.format.time,
                 format.fes=aitoa.format.fes,
                 format.improvements=aitoa.format.improvements,
                 format.objective=aitoa.format.small.integer.objective.value,
                 format.counter=aitoa.format.counter,
                 format.setup=aitoa.format.setup) {
  stopifnot(is.list(cols) || is.character(cols));
  lapply(cols, aitoa.statistics.col.to.format,
         format.time=format.time,
         format.fes=format.fes,
         format.improvements=format.improvements,
         format.objective=format.objective,
         format.counter=format.counter,
         format.setup=format.setup);
}

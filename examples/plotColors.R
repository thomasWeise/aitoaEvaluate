# the internal hash
.pairs.cache <- new.env();


# reduce all sub-sequence of type (i, i) to i
.reduce.pairs.AA <- function(pairs, n) {
  return(pairs[vapply(X=1L:n,
                      FUN=function(i) {
                        if(i <= 1L) { return(TRUE); }
                        else { return(pairs[i] != pairs[i-1L]); }
                      },
                      FUN.VALUE = FALSE)]);
}

# reduce all pairs of form ABACA to ABCA
.reduce.pairs.ABACA <- function(pairs, n) {
  if(n > 4) {
    for(i in 1L:(n-4L)) {
      A <- pairs[i];
      if((A == pairs[i + 2L]) &&
         (A == pairs[i + 4L])) {
        return(pairs[-(i + 2L)]);
      }
    }
  }
  return(pairs);
}

# reduce AB at the end to A if AB occurs elsewhere
.reduce.pairs.end <- function(pairs, n) {
  A <- pairs[n-1L];
  B <- pairs[n];
  for(i in 1L:(n-2L)) {
    A2 <- pairs[i];
    B2 <- pairs[i+1L];
    if(((A2 == A) && (B2 == B)) ||
       ((A2 == B) && (B2 == A))) {
      return(pairs[-n]);
    }
  }
  return(pairs);
}

# reduce AB at the start to B if AB occurs elsewhere
.reduce.pairs.start <- function(pairs, n) {
  A <- pairs[1L];
  B <- pairs[2L];
  for(i in 2L:(n-1L)) {
    A2 <- pairs[i];
    B2 <- pairs[i+1L];
    if(((A2 == A) && (B2 == B)) ||
       ((A2 == B) && (B2 == A))) {
      return(pairs[-1L]);
    }
  }
  return(pairs);
}

# reduce the pairs
.reduce.pairs <- function(pairs) {
  n <- length(pairs);
  repeat {
    n.old <- n;
    pairs <- .reduce.pairs.AA(pairs, n);
    n <- length(pairs);
    if(n <= 2L) { break; }
    pairs <- .reduce.pairs.start(pairs, n);
    n <- length(pairs);
    if(n <= 2L) { break; }
    pairs <- .reduce.pairs.end(pairs, n);
    n <- length(pairs);
    if(n <= 2L) { break; }
    pairs <- .reduce.pairs.ABACA(pairs, n);
    n <- length(pairs);
    if(n <= 2L) { break; }
    if(n.old <= n) { break; }
  }
  return(pairs);
}

# make a vector where all possible combinations of (i, j) with i, j in 1..n
# occur
.make.pairs <- function(n) {
  # make n an integer and check trivial case
  n <- as.integer(n);
  if(n <= 3L) {
    return(1L:n);
  }

  # see if the table is in the cache
  name <- as.character(n);
  table <- get0(x=name, envir=.pairs.cache, inherits=FALSE, ifnotfound=NULL);
  if(!is.null(table)) {
    return(table);
  }

  # create list of pairs
  pairList <- (unlist(lapply(X=1L:(n-1L),
                             FUN=function(i) {
                               lapply(X=(i+1L):n,
                                      FUN=function(j, i) c(i, j), i)
                             }), recursive = FALSE))

  # compute standard reduced list
  pairs.best <- .reduce.pairs(unlist(pairList, recursive=TRUE));
  pl <- length(pairs.best);

  # check if we can simply extend the next smaller array
  if(n > 4L) {
    pairs <- .make.pairs(n - 1L);
    add <- 1L:(n - 1L);
    add <- add[-(pairs[length(pairs)])];
    pairs <- c(pairs, rbind(rep(n, n-2L), add));
    pairs <- .reduce.pairs(pairs);
    nl <- length(pairs);
    if(nl < pl) {
      pairs.best <- pairs;
      pl <- nl;
    }
  }

  # sample possible permutations of pairList, reduce them,
  # and check if they lead to fewer bars
  for(i in 1L:(n*10L)) {
    pairs <- as.integer(unlist(
      pairList[sample(x=length(pairList),
                      size=length(pairList),
                      replace=FALSE)], recursive=TRUE));
    pairs <- .reduce.pairs(pairs);
    nl <- length(pairs);
    if(nl < pl) {
      pairs.best <- pairs;
      pl <- nl;
    }
  }

  # translate the list in such a way that the first occuring colors
  # are also the first colors in the palette
  translate <- vector(mode="integer", length=pl);
  result    <- vector(mode="integer", length=pl);
  index     <- 0L;
  for(i in 1L:pl) {
    q <- pairs.best[i];
    t <- translate[q];
    if(t == 0L) {
      index <- index + 1L;
      t <- index;
      translate[q] <- t;
    }
    result[i] <- t;
  }

  result <- force(result);
  assign(x=name, envir=.pairs.cache, value=result);

  return(result);
}

#' @title Plot a Vector of Colors so that we can see if they are unique
#' @description Take a vector of colors \code{x} and plot them in a way so that
#'   each pair of colors occurs once. This allows us to see if they are unique
#'   and distinct.
#' @param x the colors to plot
#' @param ... the parameters passed to \code{pie} or \code{plot}
#' @importFrom graphics pie plot rect
#' @export plot.colors
#' @example examples/plotColors.R
plot.colors <- function(x, ...) {
  x <- unlist(x, recursive = TRUE);
  n <- length(x);

  if(n <= 3L) {
    # if there are only few colors, plot them in a pie chart
    pie(x=rep(1, n), labels=NA, col=x,
        radius=1, border=FALSE, lty=0, ...);
  } else {
    # get the pairs of colors
    pairs <- .make.pairs(n);
    n <- length(pairs);

    plot(x=c(0,n), y=c(0,1), type="n", xlab="", ylab="", ann=FALSE,
         asp=FALSE, bty="n", xaxt="n", yaxt="n", ...);

    for(i in 1L:n){
      rect((i-1L), 1, i, 0, col=x[pairs[i]], border=NA);
    }
  }
}

library(aitoaEvaluate);

# set the random seed for reproducibility
set.seed(129855L);

if(!exists("x.range")) x.range <- 4L;
if(!exists("y.range")) y.range <- 5L;

# set the number of diagrams appropriately
old.par <- par(mfrow=c(y.range, x.range), mai=c(0.02, 0.02, 0.02, 0.02));

# paint the diagrams
n <- 1L;
for(y in 1L:y.range) {
  for(x in 1L:x.range) {
    plot.colors(aitoa.distinct.colors(n));
    n <- n + 1L;
  }
}

# reset the parameters
invisible(par(old.par));

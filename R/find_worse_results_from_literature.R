.default.load.jssp.results <- function()
  eval(parse(text="stopifnot(require('jsspInstancesAndResults')); jsspInstancesAndResults::jssp.results"));

#' @title Find Rows in a Data Set that are Equal or Worse than the Given Data
#' @description From records with results from literature similar to those used
#'   in our
#'   \href{https://github.com/thomasWeise/jsspInstancesAndResults}{jsspInstancesAndResults}
#'   package, we try to find setups that perform worse than the given statistics
#'   record.
#' @param data the result data loaded via \link{aitoa.load.end.result.stats}.
#'   This must be for one single algorithm only.
#' @param statistics a character vector of statistics to compare
#' @param compare.to the results from literature, e.g., obtained via
#'   \code{jsspInstancesAndResults::jssp.results} from our
#'   \href{https://github.com/thomasWeise/jsspInstancesAndResults}{jsspInstancesAndResults}
#'   package
#' @return a list with two elements: \code{data}, the relevant columns from the
#'   parameter \code{data}, and \code{not.better}, another data frame with the
#'   literature records that are not better in any statistic on any instances
#'   than those in \code{data}, or \code{NULL}, if no such record exists
#' @export aitoa.find.worse.results.from.literature
#' @include load_end_result_stats.R
aitoa.find.worse.results.from.literature <- function(
  data,
  statistics=c("best.f.mean", "best.f.mean"),
  compare.to=.default.load.jssp.results()) {

  stopifnot(is.data.frame(data),
            !is.null(statistics),
            is.character(statistics),
            length(statistics) > 0L,
            all(!is.na(statistics)),
            all(nchar(statistics) > 0L));

  compare.to.cols <- colnames(compare.to);
  compare.to.cols[compare.to.cols == "inst.id"] <- "instance";
  compare.to.cols[compare.to.cols == "algo.id"] <- "algorithm";
  compare.to.cols[compare.to.cols == "ref.id"] <- "reference";
  compare.to.cols[compare.to.cols == "ref.year"] <- "year";
  .med.end <- endsWith(compare.to.cols, "med");
  if(any(.med.end)) {
    compare.to.cols[.med.end] <- paste0(compare.to.cols[.med.end], "ian");
  }
  colnames(compare.to) <- compare.to.cols;
  .check.end.result.stats(data);

  base.cols <- c("algorithm", "instance");
  data <- data[, unname(unlist(c(base.cols, statistics))) ];
  stopifnot(is.data.frame(data),
            ncol(data) == (length(statistics) + 2L));
  if("reference" %in% compare.to.cols) { base.cols <- c(base.cols, "reference"); }
  if("year" %in% compare.to.cols) { base.cols <- c(base.cols, "year"); }
  compare.to.cols <- unname(unlist(c(base.cols, statistics)));
  compare.to <- compare.to[compare.to.cols];
  stopifnot(ncol(compare.to) == length(compare.to.cols),
            all(vapply(compare.to.cols,
             function(n) n %in% colnames(compare.to), FALSE)));

  instances <- sort(unique(unname(unlist(data$instance))));
  stopifnot(!is.null(instances),
            is.character(instances),
            length(instances) > 0L,
            all(!is.na(instances)),
            all(nchar(instances) > 0L),
            is.data.frame(compare.to),
            length(instances) == nrow(data$instance));
  data <- data[order(data$instance), ];
  stopifnot(nrow(data) == length(instances),
            ncol(data) == (2L + length(statistics)),
            all(is.finite(unname(unlist(data[, statistics])))));

  result <- list(data=data, not.better=NULL);

  # keep only these rows for which all statistics are defined
  for(col in statistics) {
    compare.to <- compare.to[!is.na(compare.to[, col]), ];
    if(nrow(compare.to) <= 0L) {
      return(result);
    }
  }

  # keep only the data of algorithms that have results for the
  # specified instances
  algos <- sort(unique(unname(unlist(compare.to$algorithm))));
  stopifnot(is.character(algos),
            length(algos) > 0L);
  algos <- algos[vapply(algos, function(a) {
                    sum( (compare.to$algorithm == a) &
                         (compare.to$instance %in% instances)) == length(instances)
                  }, FALSE)];
  if(is.null(algos) || (length(algos) <= 0L)) {
    return(result);
  }
  compare.to <- compare.to[(compare.to$algorithm %in% algos) &
                           (compare.to$instance %in% instances), ];
  if(nrow(compare.to) <= 0L) {
    return(result);
  }

  # preserve only worse values
  for(col in statistics) {
    for(instance in instances) {
      threshold <- unname(unlist(data[data$instance == instance, col]));
      stopifnot(is.numeric(threshold),
                length(threshold) == 1L,
                is.finite(threshold));
      compare.to <- compare.to[(compare.to$instance != instance) |
                               (compare.to[, col] >= threshold), ];
    }
  }

  algos <- algos[vapply(algos, function(a) {
    sum( (compare.to$algorithm == a) &
           (compare.to$instance %in% instances)) == length(instances)
  }, FALSE)];
  if(is.null(algos) || (length(algos) <= 0L)) {
    return(result);
  }
  compare.to <- compare.to[(compare.to$algorithm %in% algos), ];
  stopifnot(nrow(compare.to) == (length(algos) * length(instances)));
  if("year" %in% compare.to.cols) {
    if("reference" %in% compare.to.cols) {
      compare.to <- compare.to[order(compare.to$instance,
                                     -compare.to$year,
                                     compare.to$reference,
                                     compare.to$algorithm), ];
    } else {
      compare.to <- compare.to[order(compare.to$instance,
                                     -compare.to$year,
                                     compare.to$algorithm), ];
    }
  } else {
    if("reference" %in% compare.to.cols) {
      compare.to <- compare.to[order(compare.to$instance,
                                     compare.to$reference,
                                     compare.to$algorithm), ];
    } else {
      compare.to <- compare.to[order(compare.to$instance,
                                     compare.to$algorithm), ];
    }
  }
  return(list(data=data, not.better=compare.to));
}

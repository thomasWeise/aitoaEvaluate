#' @title Create a Table Comparing End Results
#' @description Create a table with statistical test results
#' @param end.results the end result statistics frame
#' @param algorithms the list of algorithms, whose \code{names} will be
#'   displayed in the table
#' @param instances the list of instances, whose \code{names} will be used
#' @param alpha the significance level
#' @param correction should we apply a (e.g., bonferroni) correction?
#' @param p.value.format the format for printing p-values
#' @param result.char.format the format for the result characters
#' @param instance.format the format for instance names
#' @param algorithm.format the format for algorithm names
#' @param column the column to be compared
#' @include load_end_results.R
#' @include common_styles.R
#' @include compare_vectors_statistically.R
#' @export aitoa.make.end.result.test.table.md
aitoa.make.end.result.test.table.md <- function(
                                           end.results,
                                           algorithms,
                                           instances,
                                           alpha=0.02,
                                           correction=c("bonferroni", "none"),
                                           p.value.format=aitoa.format.fractional.number,
                                           result.char.format=aitoa.format.setup,
                                           instance.format=aitoa.format.setup,
                                           algorithm.format=aitoa.format.setup,
                                           column=c("best.f",
                                                    "total.time",
                                                    "total.fes",
                                                    "last.improvement.time",
                                                    "last.improvement.fes",
                                                    "n.improvements")) {
# check basic args
  stopifnot(is.numeric(alpha),
            length(alpha) == 1L,
            is.finite(alpha),
            alpha >= 0,
            alpha <= 0.5);

  column <- match.arg(column);
  stopifnot(is.character(column),
            length(column) == 1L,
            !is.na(column),
            nchar(column) > 0L);

  correction <- match.arg(correction);
  stopifnot(is.character(correction),
            length(correction) == 1L,
            !is.na(correction),
            nchar(correction) > 0L);

# fix instances and their names
  stopifnot(!is.null(instances),
            is.list(instances) || is.character(instances),
            length(instances) > 0L);
  instance.names <- unname(unlist(names(instances)));
  instances <- unname(unlist(instances));
  stopifnot(is.character(instances),
            length(instances) > 0L,
            !any(is.na(instances)),
            all(nchar(instances) > 0L));
  if(is.null(instance.names)) {
    instance.names <- instances;
  }
  instance.names[is.na(instance.names)] <- instances[is.na(instance.names)];
  stopifnot(is.character(instance.names),
            length(instance.names) > 0L,
            !any(is.na(instance.names)),
            all(nchar(instance.names) > 0L));

# fix algorithms and their names
  stopifnot(!is.null(algorithms),
            is.list(algorithms) || is.character(algorithms),
            length(algorithms) > 0L);
  algorithm.names <- unname(unlist(names(algorithms)));
  algorithms <- unname(unlist(algorithms));
  stopifnot(is.character(algorithms),
            length(algorithms) > 0L,
            !any(is.na(algorithms)),
            all(nchar(algorithms) > 0L));
  if(is.null(algorithm.names)) {
    algorithm.names <- algorithms;
  }
  algorithm.names[is.na(algorithm.names)] <- algorithms[is.na(algorithm.names)];
  stopifnot(is.character(algorithm.names),
            length(algorithm.names) > 0L,
            !any(is.na(algorithm.names)),
            all(nchar(algorithm.names) > 0L));

  # check end results
  .check.end.results(end.results);

  k <- length(algorithms) * length(instances);
  stopifnot(is.integer(k),
            length(k) == 1L,
            is.finite(k),
            k >= 1L);
  N <- (k * (k - 1L)) / 2;
  stopifnot(is.numeric(N),
            length(N) == 1L,
            is.finite(N),
            N >= 1L,
            as.integer(N) == N);
  N <- as.integer(N);
  stopifnot(is.integer(N),
            length(N) == 1L,
            is.finite(N),
            N >= 1L,
            as.integer(N) == N);

  if(startsWith(correction, "b")) {
    alpha.use <- alpha / N;
  } else {
    alpha.use <- alpha;
  }
  stopifnot(is.numeric(alpha.use),
            length(alpha.use) == 1L,
            is.finite(alpha.use),
            alpha.use >= 0,
            alpha.use < 0.5);

# make the header
  header.1 <- paste0("|test|", paste(
                    vapply(instance.names, instance.format, NA_character_),
                    sep="|", collapse="|"), "|");
  header.2 <- paste0("|:--", strrep("|--:", times=length(instances)), "|");

  body <- unname(unlist(lapply(seq.int(from=1L, to=length(algorithms)-1L), function(i1) {
    algo.1 <- algorithms[[i1]];
    algo.1.name <- algorithm.names[[i1]];
    vapply(seq.int(from=(i1+1L), to=length(algorithms)), function(i2) {
      algo.2 <- algorithms[[i2]];
      algo.2.name <- algorithm.names[[i2]];
      paste0("|", algorithm.format(algo.1.name), " vs. ",
             algorithm.format(algo.2.name), "|",
             paste(vapply(seq_along(instances), function(j) {
  instance <- instances[[j]];
  data.1 <- unname(unlist(end.results[(end.results$algorithm==algo.1) &
                                      (end.results$instance==instance),
                                      column]));
  stopifnot(is.numeric(data.1),
            length(data.1) > 0L,
            all(is.finite(data.1)));
  data.2 <- unname(unlist(end.results[(end.results$algorithm==algo.2) &
                                      (end.results$instance==instance),
                                      column]));
  stopifnot(is.numeric(data.2),
            length(data.2) > 0L,
            all(is.finite(data.2)));

  res <- aitoa.compare.vectors.statistically(data.1, data.2, alpha.use);
  stopifnot(is.list(res),
            names(res) == c("p", "r"));
  return(paste0(p.value.format(res$p),
                "&nbsp;",
                result.char.format(res$r)));
               }, NA_character_),
             sep="|", collapse="|"), "|");
    }, NA_character_)
  })));

  res <- unname(unlist(c(header.1, header.2, body)));
  stopifnot(is.character(res),
            length(res) > 2L,
            !any(is.na(res)),
            all(nchar(res) > 0L));
  return(res);
}

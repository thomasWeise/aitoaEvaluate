#' @title Plot the Progress for a Set of Algorithms on a Set of Problem
#'   Instances
#' @description  Plot how a set of algorithms progress over a set of problem
#'   instances. For each instance, one diagram is plotted. The diagrams are
#'   arranged one by one from in a vertical row.
#' @param results.dir the directory where the results can be loaded from
#' @param algorithms the list of algorithm IDs. The \code{names} of this list,
#'   if set, will be used in the legends.
#' @param instances the list of instance IDs. The \code{names} of this list, if
#'   set, will be used in the legend
#' @param time.column the time dimension, either \code{t} or \code{fes}
#' @param max.time an optional limit for the time
#' @param instances.limits an optional vector of lower bounds or best-known
#'   solutions for the instances (the \code{names} of the vector will be used in
#'   the legend)
#' @param algorithm.colors the colors to be used for the different algorithms
#' @param ... parameters to be passed to \link[graphics]{par}
#' @export aitoa.plot.progress.on.multiple.instances
#' @include utils.R
#' @include plot_progress_on_instance.R
aitoa.plot.progress.on.multiple.instances <-
                       function(results.dir,
                                algorithms,
                                instances,
                                time.column=c("t", "fes"),
                                max.time=NA_integer_,
                                instances.limits=NA_integer_,
                                algorithm.colors=aitoa.distinct.colors(length(algorithms)),
                                ...) {

  # validate all the input

  stopifnot(is.character(results.dir),
            length(results.dir) > 0L);
  results.dir <- normalizePath(results.dir, mustWork=TRUE);
  stopifnot(dir.exists(results.dir));

  stopifnot(is.character(algorithms) || is.list(algorithms),
            length(algorithms) > 0L,
            !any(is.na(algorithms)),
            all(nchar(algorithms) > 0L));

  stopifnot(is.character(instances) || is.list(instances),
            length(instances) > 0L,
            !any(is.na(instances)),
            all(nchar(instances) > 0L));

  time.column <- match.arg(time.column);
  stopifnot(is.character(time.column),
            length(time.column) == 1L,
            !is.na(time.column),
            !is.null(time.column),
            time.column %in% c("t", "fes"));

  if(!is.na(max.time)) {
    stopifnot(is.numeric(max.time),
              length(max.time) == 1L,
              max.time > 0,
              is.finite(max.time));
  }

  stopifnot(is.character(algorithm.colors),
            length(algorithm.colors) == length(algorithms),
            !any(is.na(algorithm.colors)),
            length(unique(algorithm.colors)) == length(algorithms));

  if(is.null(names(algorithms))) {
    names(algorithms) <- unname(unlist(algorithms));
  }
  if(is.null(names(instances))) {
    names(instances) <- unname(unlist(instances));
  }

  if(is.na(instances.limits) || is.null(instances.limits)) {
    instances.limits <- NA_integer_;
  } else {
    stopifnot(is.numeric(instances.limits),
              length(instances.limits) == length(instances));
  }

  old.par <- .safe.par();
  mar <- 0.5*old.par$mar;

  mar[[1L]] <- 0.85 * mar[[1L]];
  mar[[2L]] <- 0.8 * mar[[2L]];
  mar[[3L]] <- 0.25 * mar[[3L]];
  mar[[4L]] <- 0.15 * mar[[4L]];
  .safe.par(cex=0.78,
            mar=mar,
            mfrow=c(length(instances), 1L));

  for(i in seq_along(instances)) {
    inst.limit.name <- NA_character_;
    inst.limit <- (if(is.na(instances.limits)) NA_integer_
                   else instances.limits[[i]]);
    if(!is.na(inst.limit)) {
      inst.limit.name <- names(instances.limits)[[i]];
      if(is.null(inst.limit.name)) {
        inst.limit.name <- NA_character_;
      }
    }

    aitoa.plot.progress.on.instance(results.dir,
                                    algorithms,
                                    instances[[i]],
                                    names(instances)[[i]],
                                    time.column,
                                    max.time,
                                    algorithm.colors,
                                    inst.limit,
                                    inst.limit.name,
                                    ...);
  }

  .safe.par(old.par);
  invisible(NULL);
}

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
#' @param max.time an optional limit for the time, can be a vector
#' @param algorithm.colors the colors to be used for the different algorithms
#' @param algorithm.colors a character vector of the same length as
#'   \code{algorithms} providing the colors to be used for the algorithms
#' @param algorithm.lty the line type to be used for the algorithms, can be
#'   vector
#' @param algorithm.lwd the line width to be used for the algorithms, can be
#'   vector
#' @param instances.limit an optional vector of lower bounds or best-known
#'   solutions for the instances
#' @param instances.limit.name an optional name or name vector for the instances
#'   limits
#' @param instance.limit.color the color for the instance limit line
#' @param instance.limit.lty the line type for the instance limit line
#' @param instance.limit.lwd the line width for the instance limit line
#' @param legend.cex the character scaling for the legend
#' @param legend.bg the background color for the legend
#' @param time.axis.text the text to be used for labeling the time axis,
#'   \code{NA} for omit label
#' @param quality.axis.text the text to be used for labeling the quality axis,
#'   \code{NA} for omit label
#' @param mgp the mgp parameter to be passed to \link[graphics]{plot}
#' @param tck the tck parameter to be passed to \link[graphics]{plot}
#' @param cex the default character scaling
#' @param mar the default margins, can be a list of vectors, one for each plot
#' @param mar.single the margin parameter(s) to be passed to the single diagrams
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
                                algorithm.colors=aitoa.distinct.colors(length(algorithms)),
                                algorithm.lty=.default.lty,
                                algorithm.lwd=.default.lwd,
                                instances.limit=NA_integer_,
                                instances.limit.name=NULL,
                                instance.limit.color=.instance.limit.color,
                                instance.limit.lty=.instance.limit.lty,
                                instance.limit.lwd=.instance.limit.lwd,
                                legend.cex=.legend.cex,
                                legend.bg=.legend.bg,
                                time.axis.text=if(time.column=="t") .time.ms.text else .time.fes.text,
                                quality.axis.text=.quality.text,
                                mgp=.default.mgp,
                                tck=.default.tck,
                                cex=.default.cex,
                                mar=.default.mar.without.labels,
                                mar.single=.default.mar.without.labels,
                                ...) {

  # validate all the input
  stopifnot(is.character(algorithms) || is.list(algorithms),
            length(algorithms) > 0L,
            !any(is.na(algorithms)),
            all(nchar(algorithms) > 0L));

  instances <- .split.names(instances);
  instance.names <- instances$names;
  instances <- instances$data;

  time.column <- .time.column(match.arg(time.column));
  results.dir <- .dir.exists(results.dir);

  if(!(is.null(max.time) || all(is.na(max.time)))) {
    stopifnot(is.numeric(max.time),
              length(max.time) > 0L,
              max.time > 0,
              is.finite(max.time));
    max.time <- rep_len(max.time, length(instances));
  } else {
    max.time <- rep_len(NA_integer_, length(instances));
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

  if(is.null(instances.limit) || all(is.na(instances.limit))) {
    instances.limit <- NA_integer_;
    instances.limit.name <- NULL;
  } else {
    stopifnot(is.numeric(instances.limit));
    instances.limit <- rep_len(instances.limit, length(instances));
    if(!(is.null(instances.limit.name) || (all(is.na(instances.limit.name))))) {
      instances.limit.name <- rep_len(instances.limit.name, length(instances));
    } else {
      instances.limit.name <- NULL;
    }
  }

  pars <- list(mfrow=c(length(instances), 1L));
  if(!(is.null(mar) || all(is.na(mar)))) {
    stopifnot(is.numeric(mar),
              length(mar) == 4L,
              all(is.finite(mar)));
    pars$mar <- mar;
  }
  if(!(is.null(cex) || is.na(cex))) {
    stopifnot(is.numeric(cex),
              length(cex) == 1L,
              is.finite(cex),
              cex > 0);
    pars$cex <- cex;
  }

  old.par <- do.call(.safe.par, pars);

  if(!(is.null(mar.single) || all(is.na(mar.single)))) {
    if(is.list(mar.single)) {
      for(m in mar.single) {
        stopifnot(is.numeric(m));
      }
      mar.single <- rep_len(mar.single, length(instances));
    } else {
      if(is.numeric(mar.single)) {
        mar.single <- rep_len(list(mar.single), length(instances));
      }
    }
  } else {
    mar.single <- NULL;
  }

  for(i in seq_along(instances)) {
    inst.limit.name <- NA_character_;

    if(is.null(instances.limit) || is.na(instances.limit)) {
      inst.limit <-  NA_integer_;
      inst.limit.name <- NA_character_;
    } else {
      inst.limit <- instances.limit[[i]];
      if(!is.null(instances.limit.name)) {
        inst.limit.name <- instances.limit.name[[i]];
      }
    }

    aitoa.plot.progress.on.instance(results.dir=results.dir,
                                    algorithms=algorithms,
                                    instance=instances[[i]],
                                    instance.name=instance.names[[i]],
                                    time.column=time.column,
                                    max.time=max.time[[i]],
                                    algorithm.colors=algorithm.colors,
                                    algorithm.lty=algorithm.lty,
                                    algorithm.lwd=algorithm.lwd,
                                    instance.limit=inst.limit,
                                    instance.limit.name=inst.limit.name,
                                    instance.limit.color=instance.limit.color,
                                    instance.limit.lty=instance.limit.lty,
                                    instance.limit.lwd=instance.limit.lwd,
                                    legend.cex=legend.cex,
                                    legend.bg=legend.bg,
                                    time.axis.text=time.axis.text,
                                    quality.axis.text=quality.axis.text,
                                    mgp=mgp,
                                    tck=tck,
                                    cex=cex,
                                    mar=if(is.null(mar.single)) NULL else mar.single[[i]],
                                    ...);
  }

  .safe.par(old.par);
  invisible(NULL);
}

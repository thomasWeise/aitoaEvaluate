#' @title Plot the Progress of a Statistic for a Set of Algorithms on a Set of Problem
#'   Instances
#' @description  Plot how a statistic for each element of set of algorithms progress over a set of problem
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
#' @param quantiles the quantiles to be plotted in a shaded fashion: all values
#'   must be >= 0 and < 0.5 and are mirrored. I.e., if you specify 0.2, then the
#'   region between the 0.2 and 0.8 quantile will be plotted semi-transparent.
#' @param center.stat the central statistic to plot, usually the \link{median}
#'   or \link{mean}
#' @param center.lty the line type to be used for the median lines
#' @param center.lwd the line width to be used for median lines
#' @param quantile.transparency the transparency to be applied to each quantile
#'   polygon
#' @param make.stairs.quantiles should the quantiles be shown as stairs
#'   (\code{TRUE}) or linear interpolation (\code{FALSE})
#' @param make.stairs.center should the center statistic be shown as stairs
#'   (\code{TRUE}) or linear interpolation (\code{FALSE})
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
#' @param make.time.unique should we make the time indices unique (except maybe
#'   for the first and last point)? This makes sense when we want to plot
#'   diagrams over a time axis, as we then have removed redundant points right
#'   away. If \code{make.time.unique==FALSE}, then there may be multiple
#'   improvements at the same time index due to the resolution of the computer
#'   clock (while each improvement will definitely have a unique FE).
#' @param use.f.range.from.raw.data should we use the real data to compute the range of the y-axis, or should the range depend on the computed statistics only (default)?
#' @param mgp the mgp parameter to be passed to \link[graphics]{plot}
#' @param tck the tck parameter to be passed to \link[graphics]{plot}
#' @param cex the default character scaling
#' @param mar the default margins, can be a list of vectors, one for each plot
#' @param mar.single the margin parameter(s) to be passed to the single diagrams
#' @param ... parameters to be passed to \link[graphics]{par}
#' @export aitoa.plot.progress.on.multiple.instances
#' @include utils.R
#' @include plot_progress_stat_on_instance.R
aitoa.plot.progress.stat.on.multiple.instances <-
                       function(results.dir,
                                algorithms,
                                instances,
                                time.column=c("t", "fes"),
                                max.time=NA_integer_,
                                algorithm.colors=aitoa.distinct.colors(length(algorithms)),
                                quantiles=.stat.plot.default.quantiles,
                                center.stat=median,
                                center.lty=.default.lty,
                                center.lwd=.thick.lwd,
                                quantile.transparency=0.8,
                                make.stairs.quantiles=FALSE,
                                make.stairs.center=FALSE,
                                instances.limit=NA_integer_,
                                instances.limit.name=NULL,
                                instance.limit.color=.instance.limit.color,
                                instance.limit.lty=.instance.limit.lty,
                                instance.limit.lwd=.instance.limit.lwd,
                                legend.cex=.legend.cex,
                                legend.bg=.legend.bg,
                                time.axis.text=if(time.column[[1L]]=="t") .time.ms.text else .time.fes.text,
                                quality.axis.text=.quality.text,
                                make.time.unique=(time.column[[1L]]=="t"),
                                use.f.range.from.raw.data=FALSE,
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
            all(nchar(algorithms) > 0L),
            !is.null(make.stairs.quantiles),
            is.logical(make.stairs.quantiles),
            length(make.stairs.quantiles) == 1L,
            isTRUE(make.stairs.quantiles) || isFALSE(make.stairs.quantiles),
            !is.null(make.stairs.center),
            is.logical(make.stairs.center),
            length(make.stairs.center) == 1L,
            isTRUE(make.stairs.center) || isFALSE(make.stairs.center));

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

    aitoa.plot.progress.stat.on.instance(
      results.dir=results.dir,
      algorithms=algorithms,
      instance=instances[[i]],
      instance.name=instance.names[[i]],
      time.column=time.column,
      max.time=max.time[[i]],
      algorithm.colors=algorithm.colors,
      center.stat=median,
      center.lty=.default.lty,
      center.lwd=.thick.lwd,
      quantile.transparency=0.8,
      make.stairs.quantiles=make.stairs.quantiles,
      make.stairs.center=make.stairs.center,
      instance.limit=inst.limit,
      instance.limit.name=inst.limit.name,
      instance.limit.color=instance.limit.color,
      instance.limit.lty=instance.limit.lty,
      instance.limit.lwd=instance.limit.lwd,
      legend.cex=legend.cex,
      legend.bg=legend.bg,
      time.axis.text=time.axis.text,
      quality.axis.text=quality.axis.text,
      make.time.unique=make.time.unique,
      use.f.range.from.raw.data=use.f.range.from.raw.data,
      mgp=mgp,
      tck=tck,
      cex=cex,
      mar=if(is.null(mar.single)) NULL else mar.single[[i]],
      ...);
  }

  .safe.par(old.par);
  invisible(NULL);
}

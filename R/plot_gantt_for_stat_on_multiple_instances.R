#' @title Plot Multiple Gantt Charts for one Statistic into one Diagram
#' @description Multiple Gantt charts fitting to one statistic are plotted into
#'   a single diagram in a column. #' @title Plot a Gantt Chart based on End
#'   Result Statistics
#' @description Plot a Gantt chart extract from a log file matching a given end
#'   result statistic.
#' @param end.result.stats the end result statistics frame, as obtained with
#'   \link{aitoa.load.end.result.stats}
#' @param results.dir the directory where to search the run log files
#' @param algorithm the algorithm id
#' @param instances the instance ids, the names of the list are used as instance
#'   names
#' @param statistic the statistics for which a matching run should be sought
#' @param xlab the x-axis label
#' @param ylab the y-axis label
#' @param machine.name.func a function converting a machine index into a
#'   character string with the machine name. The first machine index is
#'   \code{0L}, the last one \code{length(x) - 1L}.
#' @param job.colors an optional vector of job colors
#' @param print.job.names should the job names be printed into the job
#'   rectangles?
#' @param job.name.func a function converting a job index into a character
#'   string, only used if \code{isTRUE(print.job.names)}, the job indices passed
#'   on depend on the indices present in \code{x}.
#' @param job.name.cex the scaling for job names, only considered if
#'   \code{isTRUE(print.job.names)}
#' @param xlab the label for the x-axis
#' @param ylab the label for the y-axis
#' @param time.max an optional maximal time value
#' @param instances.limit an opional quality limit to be plotted as horizontal
#'   line
#' @param instances.limit.name the optional name of the quality limit, \code{NA}
#'   for omit
#' @param instance.limit.cex the font scaling for the instance limit annotation
#'   (only if \code{!is.na(instance.limit.name)})
#' @param instance.limit.color the color for the instance limit line
#' @param instance.limit.lty the line type for the instance limit line
#' @param instance.limit.lwd the line width for the instance limit line
#' @param instance.limit.adj the adjustment for the instance limit  annotation
#'   (only if \code{!is.na(instance.limit.name)})
#' @param center.label.cex the font size multiplier for the label
#' @param center.label.bg the background for the label
#' @param las the axis label oriantation (this is the default to be passed to
#'   \link[graphics]{plot}, better don't change)
#' @param xaxs the x-axis interval type (this is the default to be passed to
#'   \link[graphics]{plot}, better don't change)
#' @param yaxs the y-axis interval type (this is the default to be passed to
#'   \link[graphics]{plot}, better don't change)
#' @param mgp the mgp parameter to be passed to \link[graphics]{plot}
#' @param tck the tck parameter to be passed to \link[graphics]{plot}
#' @param cex the default character scaling
#' @param mar the default margins
#' @param mar.single potential individual margins to be passed to the single
#'   diagrams
#' @param ... parameters to be passed to \link[graphics]{plot}
#' @include common_styles.R
#' @include plot_gantt_for_stat.R
#' @export aitoa.plot.gantt.for.stat.on.multiple.instances
aitoa.plot.gantt.for.stat.on.multiple.instances <- function(
  end.result.stats,
  results.dir,
  algorithm,
  instances,
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
              "success.fes.max"),
  machine.name.func = as.character,
  job.colors = NA_character_,
  print.job.names = TRUE,
  job.name.func = as.character,
  job.name.cex = .gantt.default.job.name.cex,
  xlab = NA_character_,
  ylab = NA_character_,
  time.max = NA_integer_,
  instances.limit=NA_integer_,
  instances.limit.name=NA_character_,
  instance.limit.cex=.instance.limit.cex,
  instance.limit.color=.instance.limit.color,
  instance.limit.lty=.instance.limit.lty,
  instance.limit.lwd=.instance.limit.lwd,
  instance.limit.adj=.gantt.default.instance.limit.adj,
  center.label.cex=.gantt.label.cex,
  center.label.bg=.gantt.label.bg,
  las = 1L,
  xaxs = "i",
  yaxs = "i",
  mgp=.default.mgp,
  tck=.default.tck,
  cex=.default.cex,
  mar = if((is.null(xlab)||is.na(xlab))&&
           (is.null(ylab)||is.na(ylab)))
    .default.mar.without.labels else NULL,
  mar.single=.default.mar.without.labels,
  ...) {

  stopifnot(is.data.frame(end.result.stats),
            is.character(results.dir),
            length(results.dir) == 1L,
            is.character(algorithm),
            length(algorithm) == 1L,
            is.character(instances) || is.list(instances),
            length(instances) >= 1L);

  # validate all the input
  stopifnot(is.character(algorithm),
            length(algorithm) == 1L,
            !any(is.na(algorithm)),
            nchar(algorithm) > 0L);

  statistic <- match.arg(statistic);

  stopifnot(is.character(instances) || is.list(instances),
            length(instances) > 0L,
            !any(is.na(instances)),
            all(nchar(instances) > 0L));
  results.dir <- .dir.exists(results.dir);

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
      for(i in seq_along(mar.single)) {
        mar.single[[i]] <- .mar(mar.single[[i]], mar);
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

  if(is.null(time.max) || all(is.na(time.max))) {
    time.max <- NULL;
  } else {
    time.max <- rep_len(time.max, length(instances));
    stopifnot(is.numeric(time.max),
              all(is.finite(time.max)));
  }

  if(print.job.names) {
    job.name.cex <- .cex.rep(job.name.cex,
                             .gantt.default.job.name.cex,
                             length(instances));
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

    aitoa.plot.gantt.for.stat(
      end.result.stats=end.result.stats,
      results.dir=results.dir,
      algorithm=algorithm,
      instance=instances[[i]],
      instance.name=names(instances)[[i]],
      statistic=statistic,
      machine.name.func = machine.name.func,
      job.colors = job.colors,
      print.job.names = print.job.names,
      job.name.func = job.name.func,
      job.name.cex = job.name.cex[[i]],
      xlab = xlab,
      ylab = ylab,
      time.max = if(is.null(time.max)) NA_integer_ else time.max[[i]],
      instance.limit=inst.limit,
      instance.limit.name=inst.limit.name,
      instance.limit.cex=instance.limit.cex,
      instance.limit.color=instance.limit.color,
      instance.limit.lty=instance.limit.lty,
      instance.limit.lwd=instance.limit.lwd,
      instance.limit.adj=instance.limit.adj,
      center.label.cex=center.label.cex,
      center.label.bg=center.label.bg,
      las = las,
      xaxs = xaxs,
      yaxs = yaxs,
      mgp=mgp,
      tck=tck,
      cex=cex,
      mar = if(is.null(mar.single)) NULL else mar.single[[i]],
      ...);
  }

  .safe.par(old.par);

  invisible(NULL);
}

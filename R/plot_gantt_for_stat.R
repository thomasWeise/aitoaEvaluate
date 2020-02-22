#' @title Plot a Gantt Chart based on End Result Statistics
#' @description Plot a Gantt chart extract from a log file matching a given end
#'   result statistic.
#' @param end.result.stats the end result statistics frame, as obtained with
#'   \link{aitoa.load.end.result.stats}
#' @param results.dir the directory where to search the run log files
#' @param algorithm the algorithm id
#' @param instance the instance id
#' @param instance.name the name of the instance, as it should be printed
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
#' @param instance.limit an opional quality limit to be plotted as horizontal
#'   line
#' @param instance.limit.name the optional name of the quality limit, \code{NA}
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
#' @param ... parameters to be passed to \link[graphics]{plot}
#' @export aitoa.plot.gantt.for.stat
#' @include common_styles.R
#' @include plot_gantt.R
aitoa.plot.gantt.for.stat <- function(
                          end.result.stats,
                          results.dir,
                          algorithm,
                          instance,
                          instance.name=instance,
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
                          instance.limit=NA_integer_,
                          instance.limit.name=NA_character_,
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
                          ...) {
  stopifnot(is.data.frame(end.result.stats),
            is.character(results.dir),
            length(results.dir) == 1L,
            is.character(algorithm),
            length(algorithm) == 1L,
            is.character(instance),
            length(instance) == 1L);
  statistic <- match.arg(statistic);
  result <- aitoa.load.stat.result(
              end.result.stats = end.result.stats,
              results.dir = results.dir,
              algorithm = algorithm,
              instance = instance,
              statistic = statistic);
  stopifnot(is.list(result));
  data <- aitoa.jssp.parse.result(result$best.y);
  stopifnot(is.list(data));

  if(is.null(instance.name) || all(is.na(instance.name))) {
    instance.name <- instance;
  }
  stopifnot(is.character(instance.name),
            length(instance.name) == 1L,
            !is.na(instance.name),
            nchar(instance.name) > 0L);

  aitoa.plot.gantt(x=data,
                   machine.name.func = machine.name.func,
                   job.colors = job.colors,
                   print.job.names = print.job.names,
                   job.name.func = job.name.func,
                   job.name.cex = job.name.cex,
                   xlab = xlab,
                   ylab = ylab,
                   time.max = time.max,
                   instance.limit=instance.limit,
                   instance.limit.name=instance.limit.name,
                   instance.limit.cex=instance.limit.cex,
                   instance.limit.color=instance.limit.color,
                   instance.limit.lty=instance.limit.lty,
                   instance.limit.lwd=instance.limit.lwd,
                   instance.limit.adj=instance.limit.adj,
                   center.label = paste0(instance.name, " / ", result$best.f),
                   center.label.cex=center.label.cex,
                   center.label.bg=center.label.bg,
                   las = las,
                   xaxs = xaxs,
                   yaxs = yaxs,
                   mgp = mgp,
                   tck = tck,
                   cex = cex,
                   mar = mar,
                   ...);
}

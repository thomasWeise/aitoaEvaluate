#' @title Plot a Statistic for Different Algorthms over Instances
#' @description For each instance, we can compute one x-coordinate. We than plot
#'   the given statistics of the specified algorithms over this coordinate.
#' @param end.result.stats the end results statistics record
#' @param algorithms the algorithms
#' @param instance.selector a function which returns \code{TRUE} of each
#'   instance to be included in the plot
#' @param instance.scaler a function that returns the x-coordinate for a
#'   selected instance
#' @param algorithms the algorithms to plot, where \code{names(algorithms)} are
#'   the names, if provided
#' @param statistic the statistic to plot
#' @param statistic.top a second statistic which should be bigger than
#'   \code{statistic} and together with \code{statistic.bottom} is used to shade
#'   a region behind the \code{statistic}
#' @param statistic.bottom a second statistic which should be smaller than
#'   \code{statistic} and together with \code{statistic.top} is used to shade a
#'   region behind the \code{statistic}
#' @param algorithms.color the colors to be used for the algorithms
#' @param algorithms.lty the line types to be used for the algorithms
#' @param algorithms.pch the plot characters to be used for the algorithms
#' @param algorithms.lwd the line-widths to be used for the algorithms
#' @param statistics.bg.transparency the transparency value (0 for transparent,
#'   1 for normal color) used for making the region behind the statistics
#'   transparent
#' @param legend.pos the position for the legend
#' @param legend.cex the font size for the legend
#' @param legend.bg the background for the legend
#' @param include.bg.stats.in.legend should be include the background statistics
#'   into the legend?
#' @param mgp the mgp parameter to be passed to \link[graphics]{plot}
#' @param tck the tck parameter to be passed to \link[graphics]{plot}
#' @param cex the default character scaling
#' @param mar the default margins
#' @param ... parameters to be passed to \link[graphics]{par}
#' @include distinct_colors.R
#' @include distinct_styles.R
#' @include common_styles.R
#' @include make_color_transparent.R
#' @include legends.R
#' @importFrom graphics lines plot points
#' @export aitoa.plot.stats.over.instance
aitoa.plot.stats.over.instance <- function(end.result.stats,
                                            algorithms,
                                            instance.selector=function(n) TRUE,
                                            instance.scaler=as.integer,
                                            statistic="last.improvement.fes.mean",
                                            statistic.top="last.improvement.fes.q841",
                                            statistic.bottom="last.improvement.fes.q159",
                                            algorithms.color=aitoa.distinct.colors(length(algorithms)),
                                            algorithms.lty=aitoa.distinct.lty(length(algorithms)),
                                            algorithms.pch=aitoa.distinct.pch(length(algorithms)),
                                            algorithms.lwd=.default.lwd,
                                            statistics.bg.transparenty=.default.transparency,
                                            legend.pos="topleft",
                                            legend.cex=.legend.cex,
                                            legend.bg=.legend.bg,
                                            include.bg.stats.in.legend=TRUE,
                                            mgp=.default.mgp,
                                            tck=.default.tck,
                                            cex=.default.cex,
                                            mar=.default.mar.without.labels,
                                            ...) {
  # check input

  algorithms <- .split.names(algorithms);
  algorithms.name <- algorithms$names;
  algorithms <- algorithms$data;

  .check.end.result.stats(end.result.stats);

  stopifnot(!is.null(instance.selector),
            is.function(instance.selector),
            !is.null(instance.scaler),
            is.function(instance.scaler),
            !is.null(statistic),
            is.character(statistic),
            !any(is.na(statistic)),
            length(statistic) == 1L,
            nchar(statistic) > 0L);
  if( (!is.null(statistic.top)) || (!is.null(statistic.bottom))) {
    stopifnot(!is.null(statistic.top),
              is.character(statistic.top),
              !any(is.na(statistic.top)),
              length(statistic.top) == 1L,
              nchar(statistic.top) > 0L,
              !is.null(statistic.bottom),
              is.character(statistic.bottom),
              !any(is.na(statistic.bottom)),
              length(statistic.bottom) == 1L,
              nchar(statistic.bottom) > 0L);
  }

  # extract data

  .extract.fun <- function(algo, stat) {
    d <- end.result.stats[end.result.stats$algorithm==algo &
                          vapply(end.result.stats$instance, instance.selector, FALSE),
                          c("instance", stat)];
    x <- unname(unlist(vapply(d$instance, instance.scaler, NA_real_)));
    stopifnot(is.numeric(x),
              length(x) > 0L,
              all(is.finite(x)),
              length(unique(x)) == length(x));
    y <- unname(unlist(d[stat]));
    stopifnot(is.numeric(y),
              length(y) > 0L,
              length(y) == length(x));
    o <- order(x);
    x <- x[o];
    y <- y[o];
    return(list(x=x, y=y));
  }

  algo.stat <- lapply(algorithms, .extract.fun, stat=statistic);
  stopifnot(length(algo.stat) == length(algorithms));
  x.range <- range(unname(unlist(lapply(algo.stat, function(s) s$x))));
  y.range <- range(unname(unlist(lapply(algo.stat, function(s) s$y))));

  if(!is.null(statistic.top)) {
    algo.stat.top <- lapply(algorithms, .extract.fun, stat=statistic.top);
    algo.stat.bottom <- lapply(algorithms, .extract.fun, stat=statistic.bottom);

    stopifnot(length(algo.stat.top) ==
                length(algo.stat.bottom),
              length(algo.stat.top) ==
                length(algo.stat),
              length(algo.stat.bottom) ==
                length(algorithms));

    x.range <- range(c(x.range,
                       range(unname(unlist(lapply(algo.stat.top, function(s) s$x)))),
                       range(unname(unlist(lapply(algo.stat.bottom, function(s) s$x))))));
    y.range <- range(c(y.range,
                       range(unname(unlist(lapply(algo.stat.top, function(s) s$y)))),
                       range(unname(unlist(lapply(algo.stat.bottom, function(s) s$y))))));
  }


  # prepare diagram

  mgp <- .mgp(mgp, .default.mgp);
  tck <- .tck(tck, .default.tck);
  cex <- .cex(cex, .default.cex);
  mar <- .mar(mar, .default.mar.without.labels);

  old.par <- .safe.par(list(mgp=mgp,
                            tck=tck,
                            cex=cex,
                            mar=mar));

  pars <- list(...);
  log.scale.x <- !is.null(pars$log) &&
    grepl("x", pars$log, fixed=TRUE);
  stopifnot(isTRUE(log.scale.x) || isFALSE(log.scale.x));


  # now we can set up the parameters for the plot

  if(is.null(pars$x) || all(is.na(pars$x))) {
    pars$x <- x.range;
  }
  if(is.null(pars$xlim) || all(is.na(pars$xlim))) {
    pars$xlim <- x.range;
  }
  if(is.null(pars$y) || all(is.na(pars$y))) {
    pars$y <- y.range;
  }
  if(is.null(pars$ylim) || all(is.na(pars$ylim))) {
    pars$ylim <- y.range;
  }
  if(is.null(pars$type) || all(is.na(pars$type))) {
    pars$type <- "n";
  }
  if(is.null(pars$xaxs) || all(is.na(pars$xaxs))) {
    pars$xaxs <- "i";
  }
  if(is.null(pars$xlab) || all(is.na(pars$xlab))) {
    pars$xlab <- NA_character_;
  }
  if(is.null(pars$ylab) || all(is.na(pars$ylab))) {
    pars$ylab <- NA_character_;
  }

  # make the plot
  do.call(plot, pars);

  if(!is.null(statistic.top)) {
    stopifnot(!is.null(statistics.bg.transparenty),
              is.numeric(statistics.bg.transparenty),
              length(statistics.bg.transparenty) == 1L,
              is.finite(statistics.bg.transparenty),
              statistics.bg.transparenty >= 0,
              statistics.bg.transparenty <= 1);
    statistics.bg <- vapply(algorithms.color,
                            aitoa.make.color.transparent,
                            NA_character_,
                            transparency=statistics.bg.transparenty);
    stopifnot(length(statistics.bg) ==
                length(algorithms));
    for(i in seq_along(algorithms)) {
      polygon(x=unname(unlist(c(algo.stat.top[[i]]$x,
                                rev(algo.stat.bottom[[i]]$x)))),
              y=unname(unlist(c(algo.stat.top[[i]]$y,
                                rev(algo.stat.bottom[[i]]$y)))),
              col=statistics.bg[[i]],
              border=NA);
    }
  }

  algorithms.lty <- .lty.rep(algorithms.lty, .default.lty, length(algorithms));
  algorithms.lwd <- .lwd.rep(algorithms.lwd, .default.lwd, length(algorithms));
  algorithms.pch <- .pch.rep(algorithms.pch, 1L, length(algorithms));

  for(i in seq_along(algorithms)) {
    lines(x=algo.stat[[i]]$x,
          y=algo.stat[[i]]$y,
          col=algorithms.color[[i]],
          lwd=algorithms.lwd[[i]],
          lty=algorithms.lty[[i]]);
  }

  for(i in seq_along(algorithms)) {
    points(x=algo.stat[[i]]$x,
           y=algo.stat[[i]]$y,
           col=algorithms.color[[i]],
           pch=algorithms.pch[[i]]);
  }


  legend.text <- algorithms.name;
  legend.col <- algorithms.color;
  legend.text.col <- algorithms.color;
  legend.pch <- algorithms.pch;
  legend.lty <- algorithms.lty;
  legend.lwd <- algorithms.lwd;

  .merge <- function(a, b) unname(unlist(c(rbind(a, b))));

  if(include.bg.stats.in.legend) {
    .stat.text <- function(x) {
      substring(x, 1L +
                as.integer(regexpr("\\.[^\\.]*$", x)));
    }
    legend.text <- paste0(algorithms.name,
                   paste0(" (",
                          .stat.text(statistic),
                          ")"));
    legend.text.2 <- paste0(algorithms.name,
                            paste0(" [",
                                   .stat.text(statistic.bottom),
                                   ",",
                                   .stat.text(statistic.top),
                                   "]"));
    legend.text <- .merge(legend.text, legend.text.2);
    legend.lty.2 <- rep_len(1L, length(algorithms));
    legend.lty <- .merge(legend.lty, legend.lty.2);
    legend.lwd.2 <- rep_len(10L, length(algorithms));
    legend.lwd <- .merge(legend.lwd, legend.lwd.2);
    legend.pch.2 <- rep_len(NA_integer_, length(algorithms));
    legend.pch <- .merge(legend.pch, legend.pch.2);
    legend.col <- .merge(legend.col, statistics.bg);
    legend.text.col <- .merge(legend.text.col, algorithms.color);
  }

  aitoa.legend.main(x=legend.pos,
                    legend=legend.text,
                    col=legend.col,
                    text.col = legend.text.col,
                    pch=legend.pch,
                    lty=legend.lty,
                    lwd=legend.lwd,
                    cex=legend.cex,
                    bg=legend.bg);


  # finalize diagram
  .safe.par(old.par);

  invisible(NULL);
}

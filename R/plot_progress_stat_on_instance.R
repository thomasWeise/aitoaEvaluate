
#' @importFrom stats pnorm
.stat.plot.default.quantiles <- sort(unique(c(0, 1-pnorm(2), 1-pnorm(1), 0.25)));

#' @title Plot the Progress of a Set of Algorithms on One Instance
#' @description Plot the progress of a set of algorithms on one instance.
#' @param results.dir the directory where the results can be loaded from
#' @param algorithms a list of algorithsm, the \code{names} of which (if
#'   provided) are used for the legend
#' @param instance the instance id to be plotted
#' @param instance.name the name of the instance to show in the legend
#' @param time.column the time column
#' @param max.time an optional maximal time limit
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
#' @param instance.limit an opional quality limit to be plotted as horizontal
#'   line
#' @param instance.limit.name the optional name of the quality limit
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
#' @param f.must.be.improving \code{true} if the logged objective values must be
#'   strictly improving? This is the default way logs are generated by aitoa.
#'   However, you can also create a log where every single sampled solution is
#'   logged, so then you must set \code{f.must.be.improving=FALSE} to load the
#'   data.
#' @param use.f.range.from.raw.data should we use the real data to compute the range of the y-axis, or should the range depend on the computed statistics only (default)?
#' @param mgp the mgp parameter to be passed to \link[graphics]{plot}
#' @param tck the tck parameter to be passed to \link[graphics]{plot}
#' @param cex the default character scaling
#' @param mar the default margins
#' @param ... parameters to be passed to \link[graphics]{par}
#' @include load_instance_dir.R
#' @include utils.R
#' @importFrom graphics abline lines plot polygon
#' @importFrom stats median quantile
#' @export aitoa.plot.progress.stat.on.instance
#' @include common_styles.R
#' @include distinct_colors.R
#' @include make_color_transparent.R
#' @include legends.R
aitoa.plot.progress.stat.on.instance <-
     function(results.dir=".",
              algorithms,
              instance,
              instance.name=instance,
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
              instance.limit=NA_integer_,
              instance.limit.name=NA_character_,
              instance.limit.color=.instance.limit.color,
              instance.limit.lty=.instance.limit.lty,
              instance.limit.lwd=.instance.limit.lwd,
              legend.cex=.legend.cex,
              legend.bg=.legend.bg,
              time.axis.text=if(time.column[[1L]]=="t") .time.ms.text else .time.fes.text,
              quality.axis.text=.quality.text,
              make.time.unique=(time.column[[1L]]=="t"),
              f.must.be.improving=TRUE,
              use.f.range.from.raw.data=FALSE,
              mgp=.default.mgp,
              tck=.default.tck,
              cex=.default.cex,
              mar=.default.mar.without.labels,
              ...) {

  stopifnot(!is.null(results.dir),
            is.character(results.dir),
            length(results.dir) == 1L,
            !is.na(results.dir),
            !is.null(make.time.unique),
            is.logical(make.time.unique),
            length(make.time.unique) == 1L,
            isTRUE(make.time.unique) || isFALSE(make.time.unique),
            !is.null(f.must.be.improving),
            is.logical(f.must.be.improving),
            length(f.must.be.improving) == 1L,
            isTRUE(f.must.be.improving) || isFALSE(f.must.be.improving),
            !is.null(center.stat),
            is.function(center.stat),
            !is.null(use.f.range.from.raw.data),
            is.logical(use.f.range.from.raw.data),
            length(use.f.range.from.raw.data)==1L,
            isTRUE(use.f.range.from.raw.data) || isFALSE(use.f.range.from.raw.data),
            !is.null(make.stairs.quantiles),
            is.logical(make.stairs.quantiles),
            length(make.stairs.quantiles) == 1L,
            isTRUE(make.stairs.quantiles) || isFALSE(make.stairs.quantiles),
            !is.null(make.stairs.center),
            is.logical(make.stairs.center),
            length(make.stairs.center) == 1L,
            isTRUE(make.stairs.center) || isFALSE(make.stairs.center));

  time.column <- .time.column(match.arg(time.column));

  algorithms <- .split.names(algorithms);
  algorithm.names <- algorithms$names;
  algorithms <- algorithms$data;

  stopifnot(is.character(instance),
            !is.na(instance),
            nchar(instance) > 0L,
            is.numeric(max.time),
            is.na(max.time) || is.null(max.time) || (
              is.finite(max.time) && (max.time > 1L)
            ),
            is.character(algorithm.colors),
            length(algorithm.colors) == length(algorithms),
            is.na(instance.limit) ||
            is.null(instance.limit) ||
              (is.numeric(instance.limit) &&
              (length(instance.limit) == 1L)),
            is.character(algorithm.colors),
            length(algorithm.colors) == length(algorithms),
            !is.null(quantiles),
            is.numeric(quantiles),
            length(quantiles) > 0L,
            all(is.finite(quantiles)),
            all(quantiles >= 0),
            all(quantiles < 0.5));
  quantiles <- sort(unique(quantiles));
  stopifnot(!is.null(quantiles),
            is.numeric(quantiles),
            length(quantiles) > 0L,
            all(is.finite(quantiles)),
            all(quantiles >= 0),
            all(quantiles < 0.5));

  results.dir <- .dir.exists(results.dir);

  if(is.null(max.time) || is.na(max.time)) {
    max.time <- NA_real_;
  }
  stopifnot(is.numeric(max.time),
            is.na(max.time) || (is.finite(max.time) && max.time > 0L));

  if(is.null(instance.limit) || all(is.na(instance.limit))) {
    instance.limit <- NA_integer_;
    instance.limit.name <- NA_character_;
  } else {
    if((!is.na(instance.limit.name)) &&
       (is.null(instance.limit.name) ||
        (nchar(instance.limit.name) <= 0L))) {
      instance.limit.name <- NA_character_;
    }
  }
  stopifnot(is.numeric(instance.limit),
            is.na(instance.limit) || is.finite(instance.limit));
  if(is.null(instance.name) || all(is.na(instance.name))
     || (nchar(instance.name) <= 0L)) {
    instance.name <- instance;
  }
  stopifnot(is.character(instance.name),
            length(instance.name) == 1L,
            nchar(instance.name) > 0L);

  mgp <- .mgp(mgp, .default.mgp);
  tck <- .tck(tck, .default.tck);
  cex <- .cex(cex, .default.cex);
  mar <- .mar(mar, .default.mar.without.labels);

  old.par <- .safe.par(list(mgp=mgp,
                            tck=tck,
                            cex=cex,
                            mar=mar));

  pars <- list(...);
  log.scale.time <- !is.null(pars$log) &&
    grepl("x", pars$log, fixed=TRUE);
  stopifnot(isTRUE(log.scale.time) || isFALSE(log.scale.time));

  .logger("Now processing instance '", instance, "'.");

  x.min <- 1L;
  if((time.column == "t") && (!log.scale.time)) x.min <- 0L;

  data <- lapply(algorithms, function(algo) {
    dir <- normalizePath(file.path(results.dir,
                                   algo,
                                   instance),
                         mustWork = TRUE);
    res <- aitoa.load.inst.dir(dir, c(time.column, "f"),
                               make.time.unique = make.time.unique,
                               f.must.be.improving=f.must.be.improving);
    stopifnot(length(res) > 0L);

    .q.func <- function(q) {
      if(q <= 0) { return(min); }
      if(q >= 1) { return(max); }
      if(q == 0.5) { return(median); }
      return(function(x) quantile(x, q));
    }

    f.range <- range(unname(unlist(lapply(res,
                     function(z) range(z[, 2L])))));

    s <- lapply(quantiles,
                  function(q) {
                    list(aitoa.create.stat.run(
                           res,
                           x.column = time.column,
                           y.column = "f",
                           stat.func = .q.func(q),
                           x.min = x.min,
                           x.max = max.time,
                           make.stairs = make.stairs.quantiles),
                         aitoa.create.stat.run(
                           res,
                           x.column = time.column,
                           y.column = "f",
                           stat.func = .q.func(1-q),
                           x.min = x.min,
                           x.max = max.time,
                           make.stairs = make.stairs.quantiles));
                  });
    s[[length(s) + 1L]] <- list(aitoa.create.stat.run(
                            res,
                            x.column = time.column,
                            y.column = "f",
                            stat.func = center.stat,
                            x.min = x.min,
                            x.max = max.time,
                            make.stairs = make.stairs.center));
    attr(s, "f.range") <- f.range;
    return(s);
  });

  # done loading the data, now gathering ranges
  # 1. the time range is straightforward
  if(is.na(max.time)) {
    time.range <- max(vapply(data, function(d) {
      max(vapply(d, function(dd) {
        max(vapply(dd, function(ddd) {
          x <- ddd[, 1L];
          max(x[is.finite(x)])
        }, NA_real_))
      }, NA_real_))
    }, NA_real_))
  } else{
    time.range <- max.time;
  }
  stopifnot(is.finite(time.range),
            time.range > 1L);
  time.range <- range(c(time.range, x.min));


  # 2. the function range is more complex
  if(use.f.range.from.raw.data) {
    f.range <- range(unname(unlist(lapply(data, function(d) {
      attr(d, "f.range")
    }))));
  } else {
    f.range <- range(unname(unlist(lapply(data, function(d) {
      range(unname(unlist(lapply(d, function(dd) {
        range(unname(unlist(lapply(dd, function(ddd) {
         x <- ddd[, 2L];
         range(x[is.finite(x)])
        }))))
      }))))
    }))));
  }

  if(!is.na(instance.limit)) {
    f.range <- range(c(instance.limit, f.range));
  }

  # now we can set up the parameters for the plot

  if(is.null(pars$x) || all(is.na(pars$x))) {
    pars$x <- time.range;
  }
  if(is.null(pars$xlim) || all(is.na(pars$xlim))) {
    pars$xlim <- time.range;
  }
  if(is.null(pars$y) || all(is.na(pars$y))) {
    pars$y <- f.range;
  }
  if(is.null(pars$ylim) || all(is.na(pars$ylim))) {
    pars$ylim <- f.range;
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

  add.x.axis <- FALSE;
  if(log.scale.time && (is.null(pars$xaxt) || all(is.na(pars$xaxt)))) {
    pars$xaxt <- "n";
    add.x.axis <- TRUE;
  }


  # make the plot
  do.call(plot, pars);

  if(!(is.null(instance.limit) || is.na(instance.limit))) {
    instance.limit.color <- .color(instance.limit.color,
                                   .instance.limit.color);
    instance.limit.lty <- .lty(instance.limit.lty,
                               .instance.limit.lty);
    instance.limit.lwd <- .lwd(instance.limit.lwd,
                               .instance.limit.lwd);

    abline(h=instance.limit,
           col=instance.limit.color,
           lwd=instance.limit.lwd,
           lty=instance.limit.lty);
  }

  # if necessary, add an x-axis
  if(add.x.axis) {
    src <- range(pars$x);
    x.ticks <- as.integer(10L ^ seq.int(from=as.integer(ceiling(log10(src[[1L]]))),
                                        to=as.integer(floor(log10(src[[2L]])))));
    axis(side = 1L,
         at = x.ticks,
         labels = as.character(x.ticks));
  }

  # prepare a replacement for infinite, since infinite doesn't register
  infinte.rep <- max(f.range);
  for(i in 1L:2L) {
    infinte.rep.2 <- infinte.rep + 1L;
    if(is.finite(infinte.rep.2)) { infinte.rep <- infinte.rep.2; }
    infinte.rep.2 <- infinte.rep * 2L;
    if(is.finite(infinte.rep.2)) { infinte.rep <- infinte.rep.2; }
  }


  # prepare styles
  algorithm.colors.t <- vapply(algorithm.colors,
                               aitoa.make.color.transparent,
                               NA_character_,
                               transparency=quantile.transparency);
  center.lty <- .lty.rep(center.lty, .thick.lwd, length(algorithms));
  center.lwd <- .lwd.rep(center.lwd, .default.lty, length(algorithms));

  clear <- aitoa.make.color.transparent("white",
                                        sqrt(1/length(algorithms)));

  # plot polygons
  for(j in seq_along(data)) {
    for(i in seq_along(quantiles)) {
      sel <- data[[j]][[i]];
      x <- unname(unlist(c(sel[[1L]][, 1L], rev(sel[[2L]][, 1L]))));
      y <- unname(unlist(c(sel[[1L]][, 2L], rev(sel[[2L]][, 2L]))));
      y[!is.finite(y)] <- infinte.rep;
      if((j > 1L) && (i == 1L)) {
        polygon(x, y,
                col=clear,
                border=NA,
                xpd=FALSE);
      }
      polygon(x, y,
              col=algorithm.colors.t[[j]],
              border=NA,
              xpd=FALSE);
    }
  }


  # plot center stat
  for(j in seq_along(data)) {
    sel <- data[[j]];
    sel <- sel[[length(sel)]][[1L]];
    y <- sel[, 2L];
    y[!is.finite(y)] <- infinte.rep;
    lines(sel[,1L], y, lty=1L,
          lwd=(5*center.lwd[[j]])/2,
          col="white",
          xpd=FALSE);
  }

  for(j in seq_along(data)) {
    sel <- data[[j]];
    sel <- sel[[length(sel)]][[1L]];
    y <- sel[, 2L];
    y[!is.finite(y)] <- infinte.rep;
    lines(sel[,1L], y, lty=center.lty[[j]],
          lwd=center.lwd[[j]],
          col=algorithm.colors[[j]],
          xpd=FALSE);
  }

  # adding legend

  legend.text <- c(instance.name, algorithm.names);
  legend.color <- c("black",
                    algorithm.colors[1L:length(algorithms)]);
  legend.lty <- c(NA, center.lty);
  legend.lwd <- as.numeric(c(NA_real_, center.lwd));
  if(!(is.null(instance.limit) || all(is.na(instance.limit)))) {
    if(is.null(instance.limit.name) || is.na(instance.limit.name)) {
      instance.limit.name <- as.character(instance.limit);
    } else {
      instance.limit.name <- paste0(instance.limit.name,
                                    "=", instance.limit);
    }

    legend.text  <- c(legend.text, instance.limit.name);
    legend.color <- c(legend.color, instance.limit.color);
    legend.lty <- c(legend.lty, instance.limit.lty);
    legend.lwd <- c(legend.lwd, instance.limit.lwd);
  }

  legend.cex <- .cex(legend.cex, .legend.cex);
  legend.bg <- .color(legend.bg, .legend.bg);

  aitoa.legend.main(x="topright",
                    cex=legend.cex,
                    legend=legend.text,
                    col = legend.color,
                    lwd=legend.lwd,
                    lty=legend.lty,
                    bg=legend.bg);

  if(!(is.null(quality.axis.text) || all(is.na(quality.axis.text)))) {
    stopifnot(is.character(quality.axis.text),
              length(quality.axis.text) == 1L,
              nchar(quality.axis.text) > 0L);
    aitoa.legend.label(x="topleft",
                       legend=quality.axis.text,
                       cex=legend.cex,
                       bg=legend.bg);
  }
  if(!(is.null(time.axis.text) || all(is.na(time.axis.text)))) {
    stopifnot(is.character(time.axis.text),
              length(time.axis.text) == 1L,
              nchar(time.axis.text) > 0L);
    aitoa.legend.label(x="bottomright",
                       legend=time.axis.text,
                       cex=legend.cex,
                       bg=legend.bg);
  }

  .safe.par(old.par);
  invisible(NULL);
}

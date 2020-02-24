.default.mark.min <- function(min.x,
                              min.y,
                              color,
                              pch,
                              lty,
                              lwd) {
  stopifnot(is.numeric(min.x),
            length(min.x) > 0L,
            all(is.finite(min.x)),
            is.numeric(min.y),
            length(min.y) > 0L,
            all(is.finite(min.y)));
  if(!(is.null(pch) || is.na(pch))) {
    points(min.x,
           min.y,
           col=color,
           pch=pch,
           cex=1.75);
  }
}

#' @title Plot a Statistic over the Values of a Given Parameter
#' @description Plot an end statistic over the values of a given parameter.
#' @param end.result.stats the end result statistics frame
#' @param algorithm.template the algorithm template: a string where \code{$arg}
#'   will be replaced with the parameter value and which will then be matched
#'   with the \code{algorithm} column in the data frame
#' @param algorithm.args the values of the argument
#' @param instances a potentially named list of instances; if names are given,
#'   the names are diplayed in the legend, otherwise the instance strings
#' @param statistic the statistic to plot
#' @param instance.colors the colors to use for the instances
#' @param instance.lty the line types to be used for the instances
#' @param instance.lwd the line widths to be used for the instances
#' @param instance.pch the symbol to be used for the instances
#' @param legend.pos the legend position
#' @param legend.cex the legend character size
#' @param legend.bg the legend background
#' @param mgp the mgp value
#' @param tck the tck value
#' @param cex the base character scaling
#' @param mar the margin value
#' @param divide.by optionally, a per-instance value to divide the statistic by
#' @param mark.min.fun the function for marking the minima. If not \code{NULL}, this must be a function with six arguments, namely\describe{
#' \item{min.x}{the vector with the x-coordinates minal values}
#' \item{min.y}{the vector with the y-coordinates minal values}
#' \item{color}{the color for these points}
#' \item{pch}{the pch value for these points, or \code{NULL} if none}
#' \item{lty}{the line type for these points}
#' \item{lwd}{the line width for these points}}
#' This function, if not \code{NULL}, is called once for each instance
#' @param x.axis.at the location of the x-axis ticks: optional
#' @param ... parameters to be passed to \link[graphics]{plot}
#' @include utils.R
#' @include common_styles.R
#' @include load_stat_result.R
#' @include legends.R
#' @importFrom graphics legend lines plot points
#' @export aitoa.plot.stat.over.param
aitoa.plot.stat.over.param <- function(end.result.stats,
                                       algorithm.template,
                                       algorithm.args,
                                       instances,
                                       statistic,
                                       instance.colors=aitoa.distinct.colors(length(instances)),
                                       instance.lty=.default.lty,
                                       instance.lwd=.default.lwd,
                                       instance.pch=NULL,
                                       legend.pos="topright",
                                       legend.cex=.legend.cex,
                                       legend.bg=.legend.bg,
                                       mgp=.default.mgp,
                                       tck=.default.tck,
                                       cex=.default.cex,
                                       mar=.default.mar.without.labels,
                                       divide.by=NULL,
                                       mark.min.fun=.default.mark.min,
                                       x.axis.at=NULL,
                                       ...) {
  .check.end.result.stats(end.result.stats);

  # set up the name templates

  stopifnot(!is.null(algorithm.template),
            is.character(algorithm.template),
            length(algorithm.template) == 1L,
            !is.na(algorithm.template),
            nchar(algorithm.template) > 0L);

  stopifnot(is.list(algorithm.args) || is.vector(algorithm.args),
            length(algorithm.args) > 0L);

  stopifnot(!is.null(statistic),
            is.character(statistic),
            length(statistic) == 1L,
            nchar(statistic) > 0L);

  # get the setups

  algorithm.args <- sort(unique(unname(unlist(algorithm.args))));
  stopifnot(!is.null(algorithm.args),
            length(algorithm.args) > 0L);

  algorithms <- vapply(algorithm.args, function(t) .internal.gsub(
                   "$arg", t, algorithm.template, fixed=TRUE),
                   NA_character_);
  stopifnot(is.character(algorithms),
            length(algorithms) == length(algorithm.args));

  # setup graph
  mgp <- .mgp(mgp, .default.mgp);
  tck <- .tck(tck, .default.tck);
  cex <- .cex(cex, .default.cex);
  mar <- .mar(mar, .default.mar.without.labels);

  old.par <- .safe.par (list(mgp=mgp,
                            tck=tck,
                            cex=cex,
                            mar=mar));

  # get the data
  data <- lapply(seq_along(instances), function(z) {
    instance <- unname(unlist(instances[[z]]));
    stopifnot(is.character(instance),
              length(instance) == 1L,
              nchar(instance) > 0L);
    r <- vapply(algorithms, function(algorithm) {
      v <- unname(unlist(
             end.result.stats[(end.result.stats$instance == instance) &
                              (end.result.stats$algorithm == algorithm),
                               statistic]));
      stopifnot(length(v) == 1L,
                is.numeric(v),
                is.finite(v));
      return(v);
    }, NA_real_);
    stopifnot(is.numeric(r),
              length(r) == length(algorithms),
              all(is.finite(r)));

    if(!is.null(divide.by)) {
      r <- r / divide.by[[z]];
      stopifnot(is.numeric(r),
                length(r) == length(algorithms),
                all(is.finite(r)));
    }

    if(all((r > (-.Machine$integer.max)) &
           (r < .Machine$integer.max))) {
      t <- as.integer(round(r));
      if(all(t == r)) {
        r <- t;
      }
    }
    stopifnot(is.numeric(r),
              length(r) == length(algorithms),
              all(is.finite(r)));
    return(r);
  });

  # setup the plot
  pars <- list(...);
  xlim <- pars$xlim;
  if(is.null(xlim) || all(is.na(xlim))) {
    xlim <- range(algorithm.args);
    pars$xlim <- xlim;
  }
  stopifnot(is.numeric(xlim),
            length(xlim) == 2L,
            all(is.finite(xlim)));
  pars$x <- xlim;

  ylim <- pars$ylim;
  if(is.null(ylim) || all(is.na(ylim))) {
    ylim <- range(unname(unlist(data)));
    pars$ylim <- ylim;
  }
  stopifnot(is.numeric(ylim),
            length(ylim) == 2L,
            all(is.finite(ylim)));
  pars$y <- ylim;

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
  if(!is.null(x.axis.at)) {
    stopifnot(is.numeric(x.axis.at),
              length(x.axis.at) > 0L,
              all(is.finite(x.axis.at)));
    pars$xaxt <- "n";
  }

  do.call(plot, pars);

  # prepare plot parameters

  instance.lty <- .lty.rep(instance.lty,
                           .default.lty,
                           length(instances));
  instance.lwd <- .lwd.rep(instance.lwd,
                           .default.lwd,
                           length(instances));

  if(!is.null(instance.pch)) {
    instance.pch <- .pch.rep(instance.pch,
                             1L,
                             length(instances));
  }

  instance.name <- names(instances);
  if(is.null(instance.name)) {
    instance.name <- unname(unlist(instances));
    if(!is.null(divide.by)) {
      instance.name <- paste0(instance.name, " / ",
                              divide.by);
    }
  }
  stopifnot(length(instance.name) == length(instances));
  if(any(is.na(instance.name))) {
    instance.name[is.na(instance)] <-
      unname(unlist(instances))[is.na(instance)];
  }
  stopifnot(is.character(instance.name),
            length(instance.name) == length(instances),
            !any(is.na(instance.name)),
            all(nchar(instance.name) > 0L));

  # plot

  for(i in seq_along(instances)) {
    x <- algorithm.args;
    y <- unname(unlist(data[[i]]));
    col <- instance.colors[[i]];
    lty <- instance.lty[[i]];
    lwd <- instance.lwd[[i]];
    pch <- if(is.null(instance.pch)) NULL else instance.pch[[i]];
    lines(x=x,
          y=y,
          col=col,
          lty=lty,
          lwd=lwd);
    if(!is.null(pch)) {
      points(x=x,
             y=y,
             col=col,
             pch=pch);
    }
    if(!is.null(mark.min.fun)) {
      mn <- min(y);
      stopifnot(is.numeric(mn),
                length(mn) == 1L,
                is.finite(mn));
      sel <- (y <= mn);
      stopifnot(is.logical(sel),
                length(sel) == length(y),
                sum(sel) > 0L);
      mark.min.fun(x[sel],
                   y[sel],
                   col,
                   pch,
                   lty,
                   lwd);
    }
  }

  # add x-axis?
  if(!is.null(x.axis.at)) {
    axis(1, x.axis.at);
  }

  # add legend
  legend.args <- list(x=legend.pos,
                      legend=instance.name,
                      col=instance.colors,
                      lwd=instance.lwd,
                      lty=instance.lty,
                      cex=.cex(legend.cex, .legend.cex),
                      bg=.color(legend.bg, .legend.bg));
  if(!is.null(instance.pch)) {
    legend.args$pch <- instance.pch;
  }

  do.call(aitoa.legend.main, legend.args);

  .safe.par(old.par);
  invisible(NULL);
}

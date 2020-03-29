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
#' @param algorithm.primary.args the values of the primary argument
#' @param algorithm.secondary.args values of an optional secondary argument
#' @param algorithm.secondary.args.name the optional name of the optional secondary argument
#' @param instances a potentially named list of instances; if names are given,
#'   the names are diplayed in the legend, otherwise the instance strings
#' @param statistic the statistic to plot
#' @param instance.colors the colors to use for the instances
#' @param secondary.arg.lty the line types to be used for the secondary argument
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
#' @include distinct_styles.R
aitoa.plot.stat.over.param <- function(
         end.result.stats,
         algorithm.template,
         algorithm.primary.args,
         algorithm.secondary.args=NULL,
         algorithm.secondary.args.name=NULL,
         instances,
         statistic,
         instance.colors=aitoa.distinct.colors(length(instances)),
         instance.lwd=.default.lwd,
         instance.pch=NULL,
         secondary.arg.lty=if(is.null(algorithm.secondary.args)) .default.lty else c(1L, aitoa.distinct.lty(length(algorithm.secondary.args) - 1L)),
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

  stopifnot(is.list(algorithm.primary.args) || is.vector(algorithm.primary.args),
            length(algorithm.primary.args) > 0L,
            is.null(algorithm.secondary.args) || (is.list(algorithm.primary.args) || is.vector(algorithm.primary.args)),
            is.null(algorithm.secondary.args) || (length(algorithm.secondary.args) > 0L),
            !is.null(statistic),
            is.character(statistic),
            length(statistic) == 1L,
            nchar(statistic) > 0L);

  # get the setups

  algorithm.primary.args <- sort(unique(unname(unlist(algorithm.primary.args))));
  stopifnot(!is.null(algorithm.primary.args),
            length(algorithm.primary.args) > 0L);

  if(!is.null(algorithm.secondary.args)) {
    algorithm.secondary.args <- sort(unique(unname(unlist(algorithm.secondary.args))));
    stopifnot(!is.null(algorithm.secondary.args),
              length(algorithm.secondary.args) > 0L,
              length(algorithm.secondary.args) == 1L,
              nchar(algorithm.secondary.args) > 0L);
  }

  algorithms <- unique(vapply(algorithm.primary.args, function(t)
    .internal.gsub("$arg1", t, algorithm.template, fixed=TRUE),
                   NA_character_));
  stopifnot(is.character(algorithms),
            length(algorithms) == length(algorithm.primary.args));
  if(is.null(algorithm.secondary.args)) {
    algorithms <- list(algorithms);
  } else {
    algorithms <- lapply(algorithm.secondary.args, function(tt) {
      vapply(algorithms, function(t)
        .internal.gsub("$arg2", tt, t, fixed=TRUE),
      NA_character_)
    });
  }

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
    lapply(algorithms, function(setups) {
      r <- vapply(setups, function(algorithm) {
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
                length(r) == length(setups),
                all(is.finite(r)));

      if(!is.null(divide.by)) {
        r <- r / divide.by[[z]];
        stopifnot(is.numeric(r),
                  length(r) == length(setups),
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
                length(r) == length(setups),
                all(is.finite(r)));
      return(r);
    })
  });

  # setup the plot
  pars <- list(...);
  xlim <- pars$xlim;
  if(is.null(xlim) || all(is.na(xlim))) {
    xlim <- range(algorithm.primary.args);
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
  instance.lwd <- .lwd.rep(instance.lwd,
                           .default.lwd,
                           length(instances));

  if(!is.null(algorithm.secondary.args)) {
    if(is.null(secondary.arg.lty)) {
      use.lty <- unique(c(1L, aitoa.distinct.lty(length(algorithms) - 1L)));
      if(length(secondary.arg.lty) < length(algorithms)) {
        use.lty <- aitoa.distinct.lty(length(algorithms));
      }
    }
  } else {
    use.lty <- .lty(secondary.arg.lty, .default.lty);
  }

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
    instance.name[is.na(instance.name)] <-
      unname(unlist(instances))[is.na(instance.name)];
  }
  stopifnot(is.character(instance.name),
            length(instance.name) == length(instances),
            !any(is.na(instance.name)),
            all(nchar(instance.name) > 0L));

  # plot

  for(i in seq_along(instances)) {
    x <- algorithm.primary.args;
    col <- instance.colors[[i]];
    lwd <- instance.lwd[[i]];
    pch <- if(is.null(instance.pch)) NULL else instance.pch[[i]];
    for(j in seq_along(data[[i]])) {
      y <- unname(unlist(data[[i]][[j]]));
      lty <- if(is.null(algorithm.secondary.args)) use.lty else use.lty[[j]];
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
  }

  # add x-axis?
  if(!is.null(x.axis.at)) {
    axis(1, x.axis.at);
  }

  legend <- instance.name;
  pch <- instance.pch;
  col <- instance.colors;
  lwd <- instance.lwd;
  lty <- lty[[1L]];
  if(!is.null(algorithm.secondary.args)) {
    if((!is.null(algorithm.secondary.args.name)) &&
       (!is.na(algorithm.secondary.args.name))) {
      legend <- unname(unlist(c(legend,
                vapply(algorithm.secondary.args, function(t) {
                  paste0(algorithm.secondary.args.name, "=", t)
                }, NA_character_))));
    } else {
      legend <- unname(unlist(c(legend, algorithm.secondary.args)));
    }
    col <- unname(unlist(c(col, rep_len("black",
                   length(algorithm.secondary.args)))));
    lwd <- unname(unlist(c(lwd, rep_len(.default.lwd,
                   length(algorithm.secondary.args)))));
    lty <- unname(unlist(c(rep_len(lty[[1L]], length(instance.name)),
                           use.lty)));
    if(!is.null(pch)) {
      pch <- unname(unlist(c(pch, rep_len(NA_integer_,
                        length(algorithm.secondary.args)))));
    }
  }

  # add legend
  legend.args <- list(x=legend.pos,
                      legend=legend,
                      col=col,
                      lwd=lwd,
                      lty=lty,
                      cex=.cex(legend.cex, .legend.cex),
                      bg=.color(legend.bg, .legend.bg));
  if(!is.null(pch)) {
    legend.args$pch <- pch;
  }

  do.call(aitoa.legend.main, legend.args);

  .safe.par(old.par);
  invisible(NULL);
}

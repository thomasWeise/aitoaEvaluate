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
#' @param algorithm.lty the line type to be used for the algorithms, can be
#'   vector
#' @param algorithm.lwd the line width to be used for the algorithms, can be
#'   vector
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
#' @param mgp the mgp parameter to be passed to \link[graphics]{plot}
#' @param tck the tck parameter to be passed to \link[graphics]{plot}
#' @param cex the default character scaling
#' @param mar the default margins
#' @param ... parameters to be passed to \link[graphics]{par}
#' @include load_instance_dir.R
#' @include utils.R
#' @importFrom graphics abline legend lines mtext plot
#' @export aitoa.plot.progress.on.instance
#' @include common_styles.R
aitoa.plot.progress.on.instance <- function(results.dir=".",
                                            algorithms,
                                            instance,
                                            instance.name=instance,
                                            time.column=c("t", "fes"),
                                            max.time=NA_integer_,
                                            algorithm.colors=aitoa.distinct.colors(length(algorithms)),
                                            algorithm.lty=.default.lty,
                                            algorithm.lwd=.default.lwd,
                                            instance.limit=NA_integer_,
                                            instance.limit.name=NA_character_,
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
                                            ...) {

  time.column = match.arg(time.column);

  stopifnot(is.character(results.dir),
            !is.na(results.dir),
            length(results.dir) == 1L,
            nchar(results.dir) > 0L,
            is.character(algorithms) || is.list(algorithms),
            length(algorithms) > 0L,
            !any(is.na(algorithms)),
            all(nchar(algorithms) > 0L),
            is.character(instance),
            !is.na(instance),
            nchar(instance) > 0L,
            is.character(time.column),
            length(time.column) == 1L,
            !is.na(time.column),
            is.numeric(max.time),
            is.na(max.time) || is.null(max.time) || (
              is.finite(max.time) && (max.time > 1L)
            ),
            is.character(algorithm.colors),
            length(algorithm.colors) == length(algorithms),
            is.na(instance.limit) ||
            is.null(instance.limit) ||
              (is.numeric(instance.limit) &&
              (length(instance.limit) == 1L)));
  results.dir <- normalizePath(results.dir, mustWork = TRUE);
  stopifnot(dir.exists(results.dir));

  if(is.null(max.time) || all(is.na(max.time))) {
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

  if(is.null(instance.name) || all(is.na(instance.name))) {
    instance.name <- instance;
  }
  stopifnot(is.character(instance.name),
            length(instance.name) == 1L,
            nchar(instance.name) > 0L);

  if(is.null(mgp) || all(is.na(mgp))) {
    mgp <- .default.mgp;
  } else {
    mgp[is.na(mgp)] <- .default.mgp[is.na(mgp)];
    if(length(mgp) < 3L) {
      m <- .default.mgp;
      m[1L:length(mgp)] <- mgp;
      mgp <- m;
    }
  }
  stopifnot(is.numeric(mgp),
            length(mgp) == 3L,
            all(is.finite(mgp)));
  if(is.null(tck) || all(is.na(tck))) {
    tck <- .default.tck;
  }
  stopifnot(is.numeric(tck),
            length(tck) == 1L,
            is.finite(tck));
  if(is.null(cex) || all(is.na(cex))) {
    cex <- .default.cex;
  }
  stopifnot(is.numeric(cex),
            length(cex) == 1L,
            is.finite(cex),
            cex > 0);
  if(is.null(mar) || all(is.na(mar))) {
    mar <- .default.mar.without.labels;
  } else {
    mar[is.na(mar)] <- .default.mar.without.labels[is.na(mar)];
    if(length(mar) < 4L) {
      m <- .default.mar.without.labels;
      m[1L:length(mar)] <- mar;
    }
  }
  stopifnot(is.numeric(mar),
            length(mar) == 4L,
            all(is.finite(mar)),
            all(mar >= 0));

  pars <- list(mgp=mgp,
               tck=tck,
               cex=cex,
               mar=mar,
               ...);
  log.scale.time <- !is.null(pars$log) &&
    grepl("x", pars$log, fixed=TRUE);
  stopifnot(isTRUE(log.scale.time) || isFALSE(log.scale.time));

  .logger("Now processing instance '", instance, "'.");
  data <- lapply(algorithms, function(algo) {
    dir <- normalizePath(file.path(results.dir,
                                   algo,
                                   instance),
                         mustWork = TRUE);
    res <- aitoa.load.inst.dir(dir, c(time.column, "f"),
                               makeTimeUnique = TRUE);
    stopifnot(length(res) > 0L);

    return(lapply(res, function(frame) {
      stopifnot(nrow(frame) > 1L);
      frame <- as.matrix(frame);
      if(!is.na(max.time)) {
        stopifnot(is.finite(max.time),
                  max.time > 0L);
        frame <- frame[frame[, 1L] <= max.time, ];
        stopifnot(nrow(frame) >= 1L);
        if(frame[nrow(frame), 1L] < max.time) {
          frame <- rbind(frame,
                         c(max.time, frame[nrow(frame), 2L]));
        }
      }
      if(isTRUE(log.scale.time)) {
        frame[frame[, 1L] < 1L, 1L] <- 1L;
        del <- frame[, 1L] <= 1L;
        if(sum(del) > 2) {
          del <- which(del);
          stopifnot(length(del) >= 3L);
          del <- del[-1L];
          stopifnot(length(del) >= 2L);
          del <- del[-length(del)];
          stopifnot(length(del) >= 1L);
          frame <- frame[-del, ];
        }
      }
      return(frame);
    }));
  });

  # done loading the data, now gathering ranges
  # 1. the time range is straightforward
  if(is.na(max.time)) {
    time.range <- max(vapply(data, function(d) {
      max(vapply(d, function(dd) {
        dd[nrow(dd), 1L]
      }, NA_real_))
    }, NA_real_))
  } else{
    time.range <- max.time;
  }
  stopifnot(is.finite(time.range),
            time.range > 1L);
  time.range <- range(c(time.range, if(isTRUE(log.scale.time)) 1L else 0L));


  # 2. the function range is more complex
  f.range <- range(unname(unlist(lapply(data, function(d) {
    range(unname(unlist(lapply(d, function(dd) {
      range(dd[c(1L, nrow(dd)), 2L])
    }))))
  }))));
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
    if(is.null(instance.limit.color) ||
       all(is.na(instance.limit.color))) {
      instance.limit.color <- .instance.limit.color;
    }
    stopifnot(!is.na(instance.limit.color),
              is.character(instance.limit.color),
              length(instance.limit.color) == 1L,
              nchar(instance.limit.color) > 0L);

    if(is.null(instance.limit.lty) ||
       all(is.na(instance.limit.lty))) {
      instance.limit.lty <- .instance.limit.lty;
    }
    stopifnot(!is.na(instance.limit.lty),
              is.character(instance.limit.lty) ||
                is.numeric(instance.limit.lty),
              length(instance.limit.lty) == 1L);

    if(is.null(instance.limit.lwd) ||
       all(is.na(instance.limit.lwd))) {
      instance.limit.lwd <- .instance.limit.lwd;
    }
    stopifnot(!is.na(instance.limit.lwd),
              is.numeric(instance.limit.lwd),
              is.finite(instance.limit.lwd),
              instance.limit.lwd > 0);

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
    axis(side = 1L, at = x.ticks, labels = as.character(x.ticks));
  }

  if(is.null(algorithm.lty) ||
     all(is.na(algorithm.lty))) {
    algorithm.lty <- .default.lty;
  }
  algorithm.lty <- rep_len(algorithm.lty, length(algorithms));
  stopifnot(is.character(algorithm.lty) || is.integer(algorithm.lty),
            length(algorithm.lty) == length(algorithms));

  if(is.null(algorithm.lwd) ||
     all(is.na(algorithm.lwd))) {
    algorithm.lwd <- .default.lwd;
  }
  algorithm.lwd <- rep_len(algorithm.lwd, length(algorithms));
  stopifnot(is.character(algorithm.lwd) || is.integer(algorithm.lwd),
            length(algorithm.lwd) == length(algorithms));

  # plot the lines
  for(i in seq_along(data)) {
    color <- algorithm.colors[[i]];
    for(d in data[[i]]) {
      lines(d, lty=algorithm.lty[[i]], lwd=algorithm.lwd[[i]],
            type="s", col=color);
    }
  }

  # adding legend

  legend.text <- c(instance.name,
                   vapply(seq_along(algorithms),
                          function(i) {
                            n <- names(algorithms)[[i]];
                            if(is.na(n) || is.null(n)) {
                              n <- algorithms[[i]];
                            }
                            stopifnot(is.character(n),
                                      length(n) == 1L,
                                      nchar(n) > 0L);
                            return(n);
                          }, NA_character_));
  legend.color <- c("black",
                    algorithm.colors[1L:length(algorithms)]);
  legend.lty <- c(NA, algorithm.lty);
  legend.lwd <- as.numeric(c(NA_real_, algorithm.lwd));
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
    legend.lwd <-c(legend.lwd, instance.limit.lwd);
  }

  if(is.null(legend.cex) || all(is.na(legend.cex))) {
    legend.cex <- .legend.cex;
  }
  stopifnot(is.numeric(legend.cex),
            length(legend.cex) == 1L,
            is.finite(legend.cex),
            legend.cex > 0);

  if(is.null(legend.bg) || all(is.na(legend.bg))) {
    legend.bg <- .legend.bg;
  }
  stopifnot(is.character(legend.bg),
            length(legend.bg) == 1L,
            nchar(legend.bg) > 0L);

  legend(x="topright",
         cex=legend.cex,
         legend=legend.text,
         col = legend.color,
         text.col = legend.color,
         lwd=legend.lwd,
         lty=legend.lty,
         bty="o",
         bg=legend.bg,
         box.lwd=0L,
         inset=0.005);

  if(!(is.null(quality.axis.text) || all(is.na(quality.axis.text)))) {
    stopifnot(is.character(quality.axis.text),
              length(quality.axis.text) == 1L,
              nchar(quality.axis.text) > 0L);
    legend(x="topleft",
           legend=quality.axis.text,
           cex=legend.cex,
           bty="0",
           bg=legend.bg,
           box.lwd=0L,
           seg.len = -0.6,
           y.intersp = 0,
           lwd = 0,
           pch = NA,
           lty = NA,
           pt.lwd = 0,
           pt.cex = 0,
           inset = 0.01);
  }
  if(!(is.null(time.axis.text) || all(is.na(time.axis.text)))) {
    stopifnot(is.character(time.axis.text),
              length(time.axis.text) == 1L,
              nchar(time.axis.text) > 0L);
    legend(x="bottomright",
           legend=time.axis.text,
           cex=legend.cex,
           bty="0",
           bg=legend.bg,
           box.lwd=0L,
           seg.len = -0.6,
           y.intersp = 0,
           lty = NA,
           lwd = 0,
           pch = NA,
           pt.lwd = 0,
           pt.cex = 0,
           inset = 0.01);
  }

  invisible(NULL);
}

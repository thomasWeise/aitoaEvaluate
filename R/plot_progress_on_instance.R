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
#' @param instance.limit an opional quality limit to be plotted as horizontal
#'   line
#' @param instance.limit.name the optional name of the quality limit
#' @param ... parameters to be passed to \link[graphics]{par}
#' @include load_instance_dir.R
#' @include utils.R
#' @importFrom graphics abline legend lines mtext plot
#' @export aitoa.plot.progress.on.instance
aitoa.plot.progress.on.instance <- function(results.dir=".",
                                            algorithms,
                                            instance,
                                            instance.name=instance,
                                            time.column=c("t", "fes"),
                                            max.time=NA_integer_,
                                            algorithm.colors=aitoa.distinct.colors(length(algorithms)),
                                            instance.limit=NA_integer_,
                                            instance.limit.name=NA_character_,
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

  if(is.na(max.time) || is.null(max.time)) {
    max.time <- NA_real_;
  }
  stopifnot(is.numeric(max.time),
            is.na(max.time) || (is.finite(max.time) && max.time > 0L));

  if(is.na(instance.limit) || is.null(instance.limit)) {
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

  if(is.na(instance.name) || is.null(instance.name)) {
    instance.name <- instance;
  }
  stopifnot(is.character(instance.name),
            length(instance.name) == 1L,
            nchar(instance.name) > 0L);

  pars <- list(...);
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

  if(is.null(pars$x)) {
    pars$x <- time.range;
  }
  if(is.null(pars$xlim)) {
    pars$xlim <- time.range;
  }
  if(is.null(pars$y)) {
    pars$y <- f.range;
  }
  if(is.null(pars$ylim)) {
    pars$ylim <- f.range;
  }
  if(is.null(pars$type)) {
    pars$type <- "n";
  }
  if(is.null(pars$xaxs)) {
    pars$xaxs <- "i";
  }
  if(is.null(pars$xlab)) {
    pars$xlab <- NA_character_;
  }
  if(is.null(pars$ylab)) {
    pars$ylab <- NA_character_;
  }

  add.x.axis <- FALSE;
  if(log.scale.time && is.null(pars$xaxt)) {
    pars$xaxt <- "n";
    add.x.axis <- TRUE;
  }

  if(is.null(pars$tck)) {
    pars$tck <- -0.02;
  }

  if(is.null(pars$mgp)) {
    pars$mgp <- c(1, 0.5, 0);
  }

  # make the plot
  do.call(plot, pars);

  if(!is.na(instance.limit)) {
    bks.color <- "darkgray";
    bks.lwd <- 4/3;
    bks.lty <- 2L;
    abline(h=instance.limit, col=bks.color, lwd=bks.lwd, lty=bks.lty);
  }

  # if necessary, add an x-axis
  if(add.x.axis) {
    src <- range(pars$x);
    x.ticks <- as.integer(10L ^ seq.int(from=as.integer(ceiling(log10(src[[1L]]))),
                                        to=as.integer(floor(log10(src[[2L]])))));
    axis(side = 1L, at = x.ticks, labels = as.character(x.ticks));
  }

  # plot the lines
  for(i in seq_along(data)) {
    color <- algorithm.colors[[i]];
    for(d in data[[i]]) {
      lines(d, lty=1L, lwd=1L, type="s", col=color);
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
  legend.lty <- as.integer(c(NA_integer_, rep_len(1L, length(algorithms))));
  legend.lwd <- as.numeric(c(NA_real_, rep_len(1L, length(algorithms))));
  if(!is.na(instance.limit)) {
    if(is.na(instance.limit.name)) {
      instance.limit.name <- as.character(instance.limit);
    } else {
      instance.limit.name <- paste0(instance.limit.name,
                                    ": ", instance.limit);
    }
    legend.text  <- c(legend.text, instance.limit.name);
    legend.color <- c(legend.color, bks.color);
    legend.lty <- as.integer(c(legend.lty, bks.lty));
    legend.lwd <-c(legend.lwd, bks.lwd);
  }

  legend.bg <- "#FFFFFFAA";
  legend(x="topright",
         cex=1.05,
         legend=legend.text,
         col = legend.color,
         text.col = legend.color,
         lwd=legend.lwd,
         lty=legend.lty,
         bty="o",
         bg=legend.bg,
         box.lwd=0L,
         inset=0.005);

  legend(x="topleft",
         legend="f",
         cex=1.05,
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
  legend(x="bottomright",
         legend=(if(time.column=="t") "time in ms"
                 else "time in FEs"),
         cex=1.05,
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

  invisible(NULL);
}

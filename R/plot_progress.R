#' @include load_instance_dir.R
#' @include utils.R
#' @importFrom graphics abline legend lines mtext plot
.aitoa.plot.progress.inst <- function(results.dir,
                                      algorithms,
                                      instance,
                                      instance.name,
                                      time.column,
                                      max.time,
                                      algorithm.colors,
                                      instance.limit,
                                      instance.limit.name,
                                      ...) {

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
}

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
#' @param max.time an optional limit for the time
#' @param instances.limits an optional vector of lower bounds or best-known
#'   solutions for the instances (the \code{names} of the vector will be used in
#'   the legend)
#' @param algorithm.colors the colors to be used for the different algorithms
#' @export aitoa.plot.progress
#' @include utils.R
aitoa.plot.progress <- function(results.dir,
                                algorithms,
                                instances,
                                time.column=c("t", "fes"),
                                max.time=NA_integer_,
                                instances.limits=NA_integer_,
                                algorithm.colors=aitoa.distinct.colors(length(algorithms)),
                                ...) {

  # validate all the input

  stopifnot(is.character(results.dir),
            length(results.dir) > 0L);
  results.dir <- normalizePath(results.dir, mustWork=TRUE);
  stopifnot(dir.exists(results.dir));

  stopifnot(is.character(algorithms) || is.list(algorithms),
            length(algorithms) > 0L,
            !any(is.na(algorithms)),
            all(nchar(algorithms) > 0L));

  stopifnot(is.character(instances) || is.list(instances),
            length(instances) > 0L,
            !any(is.na(instances)),
            all(nchar(instances) > 0L));

  time.column <- match.arg(time.column);
  stopifnot(is.character(time.column),
            length(time.column) == 1L,
            !is.na(time.column),
            !is.null(time.column),
            time.column %in% c("t", "fes"));

  if(!is.na(max.time)) {
    stopifnot(is.numeric(max.time),
              length(max.time) == 1L,
              max.time > 0,
              is.finite(max.time));
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

  if(is.na(instances.limits) || is.null(instances.limits)) {
    instances.limits <- NA_integer_;
  } else {
    stopifnot(is.numeric(instances.limits),
              length(instances.limits) == length(instances));
  }

  old.par <- .safe.par();
  mar <- 0.5*old.par$mar;

  mar[[1L]] <- 0.85 * mar[[1L]];
  mar[[2L]] <- 0.8 * mar[[2L]];
  mar[[3L]] <- 0.25 * mar[[3L]];
  mar[[4L]] <- 0.15 * mar[[4L]];
  .safe.par(cex=0.78,
            mar=mar,
            mfrow=c(length(instances), 1L));

  for(i in seq_along(instances)) {
    inst.limit.name <- NA_character_;
    inst.limit <- (if(is.na(instances.limits)) NA_integer_
                   else instances.limits[[i]]);
    if(!is.na(inst.limit)) {
      inst.limit.name <- names(instances.limits)[[i]];
      if(is.null(inst.limit.name)) {
        inst.limit.name <- NA_character_;
      }
    }

    .aitoa.plot.progress.inst(results.dir,
                              algorithms,
                              instances[[i]],
                              names(instances)[[i]],
                              time.column,
                              max.time,
                              algorithm.colors,
                              inst.limit,
                              inst.limit.name,
                              ...);
  }

  .safe.par(old.par);
  invisible(NULL);
}

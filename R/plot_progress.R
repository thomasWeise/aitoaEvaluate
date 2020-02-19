#' @include load_instance_dir.R
#' @include utils.R
#' @importFrom graphics abline legend lines mtext par plot
.aitoa.plot.progress.inst <- function(results.dir,
                                      algorithms,
                                      instance,
                                      instance.name,
                                      time.column,
                                      max.time,
                                      algorithm.colors,
                                      instance.bks,
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
            is.na(instance.bks) ||
            is.null(instance.bks) ||
              (is.numeric(instance.bks) &&
              (length(instance.bks) == 1L)));
  results.dir <- normalizePath(results.dir, mustWork = TRUE);
  stopifnot(dir.exists(results.dir));

  if(is.na(max.time) || is.null(max.time)) {
    max.time <- NA_real_;
  }
  stopifnot(is.numeric(max.time),
            is.na(max.time) || (is.finite(max.time) && max.time > 0L));

  if(is.na(instance.bks) || is.null(instance.bks)) {
    instance.bks <- NA_real_;
  }
  stopifnot(is.numeric(instance.bks),
            is.na(instance.bks) || is.finite(instance.bks));

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
  if(!is.na(instance.bks)) {
    f.range <- range(c(instance.bks, f.range));
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

  # make the plot
  do.call(plot, pars);

  if(!is.na(instance.bks)) {
    bks.color <- "darkgray";
    bks.lwd <- 4/3;
    bks.lty <- 2L;
    abline(h=instance.bks, col=bks.color, lwd=bks.lwd, lty=bks.lty);
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
  if(!is.na(instance.bks)) {
    legend.text  <- c(legend.text, paste0("bks=", instance.bks));
    legend.color <- c(legend.color, bks.color);
    legend.lty <- as.integer(c(legend.lty, bks.lty));
    legend.lwd <-c(legend.lwd, bks.lwd);
  }

  legend(x="topright",
         cex=1.05,
         legend=legend.text,
         col = legend.color,
         text.col = legend.color,
         lwd=legend.lwd,
         lty=legend.lty,
         bty="n",
         box.lwd=0L);

  mtext("f", side=2L, line=-1.1, adj=0.15, cex=1.05);
  mtext(if(time.column=="t") "time in ms" else "FEs",
        side=1L, line=-1.1, adj=0.15, cex=1.05);
}

#' @title Plot the Progress for a Set of Algorithms on a Set of Problem Instances
#' @description  Plot how a set of algorithms progress over a set of problem instances. For each instance, one diagram is plotted. The diagrams are arranged one by one from in a vertical row.
#' @param results.dir the directory where the results can be loaded from
#' @param algorithms the list of algorithm IDs. The \code{names} of this list, if set, will be used in the legends.
#' @param instances the list of instance IDs. The \code{names} of this list, if set, will be used in the legend
#' @param time.column the time dimension, either \code{t} or \code{fes}
#' @param max.time an optional limit for the time
#' @param instances.bks an optional vector of best-known solutions for the instances
#' @param algorithm.colors the colors to be used for the different algorithms
#' @export aitoa.plot.progress
#' @importFrom graphics par
aitoa.plot.progress <- function(results.dir,
                                algorithms,
                                instances,
                                time.column=c("t", "fes"),
                                max.time=NA_integer_,
                                instances.bks=NA_integer_,
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

  if(is.na(instances.bks) || is.null(instances.bks)) {
    instances.bks <- NA_integer_;
  } else {
    stopifnot(is.numeric(instances.bks),
              length(instances.bks) == length(instances));
  }

  old.par <- par();
  mar <- 0.5*old.par$mar;

  mar[[1L]] <- 0.85 * mar[[1L]];
  mar[[3L]] <- 0.25 * mar[[3L]];
  mar[[4L]] <- 0.15 * mar[[4L]];
  par(cex=0.78, mar=mar,
      mfrow=c(length(instances), 1L));

  for(i in seq_along(instances)) {
    .aitoa.plot.progress.inst(results.dir,
                              algorithms,
                              instances[[i]],
                              names(instances)[[i]],
                              time.column,
                              max.time,
                              algorithm.colors,
                              (if(is.na(instances.bks)) NA_integer_
                               else instances.bks[[i]]),
                              ...);
  }

  par(old.par);
}

.gantt.min <- -1/3;
.gantt.max <- 1/3;


.check.gantt.data <- function(data) {
  stopifnot(is.list(data),
  length(data) > 0L,
  all(vapply(data, is.list, FALSE)),
  all(vapply(data, length, NA_integer_) >= 0L),
  all(vapply(data, function(xx) all(vapply(xx, is.list, FALSE)), FALSE)),
  all(vapply(data, function(xx) all(vapply(xx, length, NA_integer_)==3L), FALSE)));

  dev.names <- c("job", "start", "end");
  for(machine in data) {
    stopifnot(is.list(machine),
              length(machine) >= 0L);
    for(task in machine) {
      stopifnot(is.list(task),
                length(task) >= 3L,
                all(names(task)[1L:3L] == dev.names));
      ii <- as.integer(unlist(task[1L:3L]));
      stopifnot(all(is.finite(ii)),
                all(ii >= 0L));
    }
  }
}


#' @title Plot a Gantt Chart
#' @description Plot a Gantt chart based on a list \code{x} of lists of data.
#'   The list contains one list for each machine. Each machine list, in turn, is
#'   a list of lists, too. Each of their elements has the form \code{list(job=,
#'   start=, end=)}, with elements denoting the job ID, start, and end time,
#'   respectively.
#' @param x the data to be plotted
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
#' @param center.label an optional label to be plotted in the lower center
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
#' @export aitoa.plot.gantt
#' @importFrom graphics axis grconvertX grconvertY plot rect text
#' @importFrom grDevices col2rgb
#' @include distinct_colors.R
#' @include common_styles.R
aitoa.plot.gantt <- function(x,
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
                       center.label=NA_character_,
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

# validate input data
  .check.gantt.data(x);

# set up machine data
  machines <- as.integer(seq.int(from=0L, to=(length(x) - 1L)));
  stopifnot(is.integer(machines),
            length(machines) == length(x),
            length(machines) > 0L,
            all(is.finite(machines)),
            all(machines >= 0L));
  stopifnot(is.function(machine.name.func));
  machine.names <- vapply(machines, machine.name.func, NA_character_);
  stopifnot(is.character(machine.names),
            !any(is.na(machine.names)),
            all(nchar(machine.names) > 0L));

# set up jobs and job names
  jobs <- sort(unique(as.integer(unname(unlist(lapply(x, function(xx) {
    vapply(xx, function(xxx) as.integer(xxx$job), NA_integer_)
  }))))));
  stopifnot(is.integer(jobs),
            length(jobs) > 0L,
            all(is.finite(jobs)),
            all(jobs >= 0L));

  if(is.null(job.colors) || is.na(job.colors)) {
    job.colors <- aitoa.distinct.colors(length(jobs));
  }
  stopifnot(is.character(job.colors),
            !any(is.na(job.colors)),
            length(job.colors)>= (length(jobs)),
            all(nchar(job.colors) > 0L));

  if(is.null(print.job.names) || is.na(print.job.names)) {
    print.job.names <- FALSE;
  }
  stopifnot(is.logical(print.job.names),
            length(print.job.names) == 1L,
            isTRUE(print.job.names) || isFALSE(print.job.names));

  if(print.job.names) {
    stopifnot(is.function(job.name.func));
    job.names <- vapply(jobs, job.name.func, NA_character_);
    stopifnot(is.character(job.names),
              !any(is.na(job.names)),
              length(job.names) == length(jobs),
              all(nchar(job.names) > 0L));

    job.name.cex <- .cex(job.name.cex, .gantt.default.job.name.cex);
  }

# set up the graph
  mgp <- .mgp(mgp, .default.mgp);
  tck <- .tck(tck, .default.tck);
  cex <- .cex(cex, .default.cex);
  xlab <- force(xlab);
  ylab <- force(ylab);
  las <- force(las);
  xaxs <- force(xaxs);
  yaxs <- force(yaxs);

  pars <- list(..., xlab=xlab, ylab=ylab,
               xaxs=xaxs, yaxs=yaxs,
               yaxt = "n", type = "n");

  par2 <- list(mgp=mgp, las=las, tck=tck, cex=cex);

  if(is.null(mar) || all(is.na(mar))) {
    if((is.null(xlab)||is.na(xlab)) &&
       (is.null(ylab)||is.na(ylab))) {
      par2$mar <- .default.mar.without.labels;
    }
  } else {
    stopifnot(is.numeric(mar),
              length(mar) > 0L);
    par2$mar <- .mar(mar, .default.mar.without.labels);
  }

  old.par <- .safe.par(par2);

  xlim <- pars$xlim;
  if(is.null(xlim)) {
    if(!(is.na(instance.limit) ||
         is.null(instance.limit))) {
      stopifnot(is.numeric(instance.limit),
                is.finite(instance.limit),
                instance.limit >= 0);
      xlim <- range(c(xlim, instance.limit));
    }
    if(!(is.na(time.max) ||
         is.null(time.max))) {
      stopifnot(is.numeric(time.max),
                is.finite(time.max),
                time.max >= 0);
      xlim <- range(c(xlim, time.max));
    }

    xlim <- range(c(xlim, unlist(lapply(x,
                    function(d) {
                      range(unname(unlist(lapply(d,
                          function(dd) c(dd$start, dd$end)))))
                    }))));
    ofs.x <- max(c(sum(c(1, -1)*xlim*0.00025),
                   abs(sum(c(1, -1)*grconvertX(c(1.3, 0),
                          from="device", to="user")))));
    if(is.finite(ofs.x)) {
      xlim[[1L]] <- xlim[[1L]] - ofs.x;
      xlim[[2L]] <- xlim[[2L]] + ofs.x;
    }
  }
  pars$xlim <- xlim;
  pars$x <- xlim;
  stopifnot(is.numeric(xlim),
            length(xlim) == 2L,
            all(is.finite(xlim)),
            xlim[[2L]] > xlim[[1L]]);

  ylim <- pars$ylim;
  no.ylim <- is.null(ylim);
  if(no.ylim) {
    ylim <- range(c(.gantt.min, length(machines) - 1L + .gantt.max));
  }
    ofs.y <- max(c(sum(c(1, -1)*ylim*0.00025),
                   abs(sum(c(1, -1)*grconvertY(c(1.3, 0),
                            from="device", to="user")))));
  if(no.ylim) {
    if(is.finite(ofs.y)) {
      ylim[[1L]] <- ylim[[1L]] - ofs.y;
      ylim[[2L]] <- ylim[[2L]] + ofs.y;
    } else {
      ofs.y <- 0.01*.gantt.min;
    }
    pars$ylim <- ylim;
  }
  pars$y <- ylim;

  stopifnot(is.numeric(ylim),
            length(ylim) == 2L,
            all(is.finite(ylim)),
            ylim[[2L]] > ylim[[1L]]);


  # paint plot area, but without y axis
  do.call(plot, pars);

  axis(2L, at = machines,
       labels = machine.names);

  # now paint the chart
  for(i in seq_along(x)) {
    # compute y range for machine
    y.min <- (i - 1 + .gantt.min);
    y.max <- (i - 1 + .gantt.max);
    # iterate over jobs
    for(task in x[[i]]) {
      end <- task$end;
      start <- task$start;

      # only plot non-empty jobs
      if(end > start) {
        job <- task$job;
        job.index <- which(jobs == job);
        stopifnot(length(job.index) == 1L,
                  is.integer(job.index),
                  is.finite(job.index),
                  job.index > 0L);

        # get job color
        col <- job.colors[[job.index]];

        # paint job
        rect(start, y.min, end, y.max, col=col, border=NA);

        if(print.job.names) {
          # try to choose a good text color
          if(aitoa.rgb2gray(col2rgb(col), limit=255L) < 100) {
            text.col = "white";
          } else {
            text.col = "black";
          }

          # add label
          text(x=(0.5*(end + start)),
               y=(i-1), adj=c(0.5, 0.5),
               cex=job.name.cex,
               labels=job.names[[job.index]],
               col=text.col);
        }
      }
    }
  }

  if(!(is.na(instance.limit) ||
       is.null(instance.limit))) {
    stopifnot(is.numeric(instance.limit),
              is.finite(instance.limit),
              instance.limit >= 0);

    instance.limit.cex <- .cex(instance.limit.cex,
                               .instance.limit.cex);
    instance.limit.color <- .color(instance.limit.color,
                                   .instance.limit.color);
    instance.limit.lty <- .lty(instance.limit.lty,
                               .instance.limit.lty);
    instance.limit.lwd <- .lwd(instance.limit.lwd,
                               .instance.limit.lwd);

    abline(v=instance.limit,
           col=instance.limit.color,
           lty=instance.limit.lty,
           lwd=instance.limit.lwd);

    if(!is.na(instance.limit.name)) {
      stopifnot(is.character(instance.limit.name),
                length(instance.limit.name) == 1L);
      if(nchar(instance.limit.name) > 0L) {
        instance.limit.name <- paste0(instance.limit.name,
                                      "=", instance.limit);
      } else {
        instance.limit.name <- as.character(instance.limit);
      }

      instance.limit.adj <- .adj(instance.limit.adj,
                                 .gantt.default.instance.limit.adj);

      text(x= instance.limit,
           y= .gantt.min,
           labels = instance.limit.name,
           adj = instance.limit.adj,
           col = instance.limit.color,
           cex = instance.limit.cex,
           srt = 90);
    }
  }

  if(!(is.null(center.label) || any(is.na(center.label)))) {
    stopifnot(is.character(center.label),
              length(center.label) == 1L);
    center.label.bg <- .color(center.label.bg, .gantt.label.bg);
    center.label.cex <- .cex(center.label.cex, .gantt.label.cex);

    legend(x=mean(xlim),
           y=.gantt.min+ofs.y,
           legend=center.label,
           xjust=0.5,
           yjust=0,
           box.lwd=0L,
           seg.len = -0.7,
           y.intersp = 0,
           lwd = 0,
           pch = NA,
           lty = NA,
           pt.lwd = 0,
           pt.cex = 0,
           bty="o",
           cex=center.label.cex,
           bg=center.label.bg);
  }

  .safe.par(old.par);
  invisible(NULL);
}

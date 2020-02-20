.gantt.min <- -1/3;
.gantt.max <- 1/3;



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
#' @param las the axis label oriantation (this is the default to be passed to
#'   \link[graphics]{plot}, better don't change)
#' @param xaxs the x-axis interval type (this is the default to be passed to
#'   \link[graphics]{plot}, better don't change)
#' @param yaxs the y-axis interval type (this is the default to be passed to
#'   \link[graphics]{plot}, better don't change)
#' @param ... parameters to be passed to \link[graphics]{plot}
#' @export aitoa.plot.gantt
#' @importFrom graphics axis grconvertX grconvertY plot rect text
#' @importFrom grDevices col2rgb
#' @include distinct_colors.R
aitoa.plot.gantt <- function(x,
                       machine.name.func = as.character,
                       job.colors = NA_character_,
                       print.job.names = TRUE,
                       job.name.func = as.character,
                       job.name.cex = 0.9,
                       xlab = "Time",
                       ylab = "Machine",
                       las = 1L,
                       xaxs = "i",
                       yaxs = "i",
                       ...) {

# validate input data
  stopifnot(is.list(x),
            length(x) > 0L,
            all(vapply(x, is.list, FALSE)),
            all(vapply(x, length, NA_integer_) > 0L),
            all(vapply(x, function(xx) all(vapply(xx, is.list, FALSE)), FALSE)),
            all(vapply(x, function(xx) all(vapply(xx, length, NA_integer_)==3L), FALSE)));

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

  if(is.na(job.colors)) {
    job.colors <- aitoa.distinct.colors(length(jobs));
  }
  stopifnot(is.character(job.colors),
            !any(is.na(job.colors)),
            length(job.colors)>= (length(jobs)),
            all(nchar(job.colors) > 0L));

  if(is.na(print.job.names)) {
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

    if(is.na(job.name.cex)) {
      job.name.cex <- 0.9;
    }
    stopifnot(is.numeric(job.name.cex),
              length(job.name.cex) == 1L,
              is.finite(job.name.cex),
              job.name.cex > 0);
  }

# set up the graph
  pars <- list(..., xlab=xlab, ylab=ylab,
               las=1L, xaxs=xaxs, yaxs=yaxs,
               yaxt = "n", type = "n");


  xlim <- pars$xlim;
  if(is.null(xlim)) {
    xlim <- range(unlist(lapply(x,
                    function(d) {
                      range(unname(unlist(lapply(d,
                          function(dd) c(dd$start, dd$end)))))
                    })));
    ofs.x <- max(c(sum(c(1, -1)*xlim*0.00025),
                   abs(sum(c(1, -1)*grconvertX(c(1.3, 0),
                          from="device", to="user")))));
    xlim[[1L]] <- xlim[[1L]] - ofs.x;
    xlim[[2L]] <- xlim[[2L]] + ofs.x;
    pars$xlim <- xlim;
  }
  pars$x <- xlim;
  stopifnot(is.numeric(xlim),
            length(xlim) == 2L,
            all(is.finite(xlim)),
            xlim[[2L]] > xlim[[1L]]);

  ylim <- pars$ylim;
  if(is.null(ylim)) {
    ylim <- range(c(.gantt.min, length(machines) - 1L + .gantt.max));
    ofs.y <- max(c(sum(c(1, -1)*ylim*0.00025),
                   abs(sum(c(1, -1)*grconvertY(c(1.3, 0),
                            from="device", to="user")))));
    ylim[[1L]] <- ylim[[1L]] - ofs.y;
    ylim[[2L]] <- ylim[[2L]] + ofs.y;
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
       labels = machine.names,
       las = pars$las);

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

  invisible(NULL);
}

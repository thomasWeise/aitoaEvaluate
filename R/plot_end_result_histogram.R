#' @title Plot a Density Histogram of the End Results
#' @description This function plots a density histogram of the end results
#' @param end.results the end results loaded from the data
#' @param instance the instance to plot
#' @param algorithm the algorithm to plot
#' @param mean.add should a marker for the arithmetic mean be added?
#' @param mean.lwd the line width for the marker for the mean
#' @param mean.col the color for the marker for the mean
#' @param mean.minus.sd.add should a marker for mean-standard deviation be
#'   added?
#' @param mean.minus.sd.lwd the line width for the marker for mean-sd
#' @param mean.minus.sd.col the color for the marker for mean-sd
#' @param mean.plus.sd.add should a marker for the mean+standard deviation be
#'   added?
#' @param mean.plus.sd.lwd the line width for the marker for mean+sd
#' @param mean.plus.sd.col the color for the marker for mean+sd
#' @param median.add should a marker for the median be added?
#' @param median.lwd the line width for the marker for the median
#' @param median.col the color for the marker for the median
#' @param q159.add should a marker for the 15.9\% quantile be added?
#' @param q159.lwd the line width for the marker for the 15.9\% quantile
#' @param q159.col the color for the marker for the 15.9\% quantile
#' @param q841.add should a marker for the 84.1\% quantile be added?
#' @param q841.lwd the line width for the marker for the 84.1\% quantile
#' @param q841.col the color for the marker for the 84.1\% quantile
#' @param density.add should a curve illustrating a density estimate be added?
#' @param density.lwd the line width for the density estimate curve
#' @param density.col the color for the density estimate curve
#' @param normal.add should a normal distribution probability density function
#'   be added?
#' @param normal.lwd the line width for the normal PDF
#' @param normal.col the color for the normal PDF
#' @param dimension the dimension to be plotted
#' @param legend.cex the character scaling for the legend
#' @param legend.bg the background color for the legend
#' @param mgp the mgp parameter to be passed to \link[graphics]{plot}
#' @param tck the tck parameter to be passed to \link[graphics]{plot}
#' @param cex the default character scaling
#' @param mar the default margins
#' @param legend.pos the position for the legend (set to \code{NULL} or
#'   \code{NA} to omit legend)
#' @param label.y the y-label (set to \code{NULL} or \code{NA} to omit y-label)
#' @param label.x the x-label (set to \code{NULL} or \code{NA} to omit x-label)
#' @param ... parameters to be passed to \link[graphics]{hist}
#' @include load_end_results.R
#' @include common_styles.R
#' @include legends.R
#' @importFrom graphics hist lines curve
#' @importFrom stats density dnorm sd
#' @export aitoa.plot.end.result.histogram
aitoa.plot.end.result.histogram <- function(end.results,
                                            instance,
                                            algorithm,
                                            mean.add=TRUE,
                                            mean.lwd=if(median.add) 4L else 3L,
                                            mean.col="#FF0000",
                                            mean.minus.sd.add=TRUE,
                                            mean.minus.sd.lwd=if(q159.add) 4L else 3L,
                                            mean.minus.sd.col="#FF7777",
                                            mean.plus.sd.add=TRUE,
                                            mean.plus.sd.lwd=if(q841.add) 4L else 3L,
                                            mean.plus.sd.col="#880000",
                                            median.add=TRUE,
                                            median.lwd=3L,
                                            median.col="#0000FF",
                                            q159.add=TRUE,
                                            q159.lwd=3L,
                                            q159.col="#7777FF",
                                            q841.add=TRUE,
                                            q841.lwd=3L,
                                            q841.col="#000088",
                                            density.add=TRUE,
                                            density.lwd=3L,
                                            density.col="#00FF00",
                                            normal.add=TRUE,
                                            normal.lwd=3L,
                                            normal.col="#008800",
                                            dimension="best.f",
                                            legend.cex=.legend.cex,
                                            legend.bg=.legend.bg,
                                            mgp=.default.mgp,
                                            tck=.default.tck,
                                            cex=.default.cex,
                                            mar=.default.mar.without.labels,
                                            legend.pos="topright",
                                            label.y="density",
                                            label.x=if(dimension=="best.f") "f" else dimension,
                                            ...) {
  .check.end.results(end.results);
  stopifnot(is.character(instance),
            !any(is.na(instance)),
            length(instance) == 1L,
            nchar(instance) > 0L,
            is.character(algorithm),
            !any(is.na(algorithm)),
            length(algorithm) == 1L,
            nchar(algorithm) > 0L);

  data <- end.results[(end.results$algorithm == algorithm) &
                      (end.results$instance == instance),
                      dimension];
  stopifnot(is.data.frame(data) || is.vector(data),
            nrow(data) > 0L,
            ncol(data) == 1L);
  data <- unname(unlist(data));
  stopifnot(is.numeric(data),
            length(data) > 0L,
            all(is.finite(data)));
  if(all(data > (-.Machine$integer.max)) &&
     all(data < .Machine$integer.max)) {
    d <- as.integer(data);
    if(all(is.finite(d)) &&
       all(d == data)) {
      data <- d;
    }
  }
  data <- sort(data);

  mgp <- .mgp(mgp, .default.mgp);
  tck <- .tck(tck, .default.tck);
  cex <- .cex(cex, .default.cex);
  mar <- .mar(mar, .default.mar.without.labels);

  old.par <- .safe.par(list(mgp=mgp,
                            tck=tck,
                            cex=cex,
                            mar=mar));

  pars <- list(...);

  if(is.null(pars$xlab) || all(is.na(pars$xlab))) {
    pars$xlab <- NA_character_;
  }
  if(is.null(pars$ylab) || all(is.na(pars$ylab))) {
    pars$ylab <- NA_character_;
  }
  if(is.null(pars$main) || all(is.na(pars$main))) {
    pars$main <- NA_character_;
  }

  .add <- numeric(length = 0L);
  .legend <- character(length=0L);
  .colors <- character(length=0L);
  .lwd <- numeric(length=0L);

  if(isTRUE(mean.add) ||
     isTRUE(mean.minus.sd.add) ||
     isTRUE(mean.plus.sd.add) ||
     isTRUE(normal.add)) {
    .mean <- mean(data);
    stopifnot(is.finite(.mean));

    if(isTRUE(mean.minus.sd.add) ||
       isTRUE(mean.minus.sd.add) ||
       isTRUE(normal.add)) {
      .sd <- sd(data);
      stopifnot(is.finite(.sd));
      if(isTRUE(mean.minus.sd.add)) {
        .mean.minus.sd <- .mean - .sd;
        stopifnot(is.numeric(.mean.minus.sd),
                  length(.mean.minus.sd) == 1L,
                  is.finite(.mean.minus.sd));
        .add <- c(.add, .mean.minus.sd);
        .legend <- c(.legend, "mean-sd");
        .colors <- c(.colors, mean.minus.sd.col);
        .lwd <- c(.lwd, mean.minus.sd.lwd);
      }
    }

    if(isTRUE(mean.add)) {
      .add <- c(.add, .mean);
      .legend <- c(.legend, "mean");
      .colors <- c(.colors, mean.col);
      .lwd <- c(.lwd, mean.lwd);
    }

    if(isTRUE(mean.plus.sd.add)) {
      .mean.plus.sd <- .mean + .sd;
      stopifnot(is.numeric(.mean.plus.sd),
                length(.mean.plus.sd) == 1L,
                is.finite(.mean.plus.sd));
      .add <- c(.add, .mean.plus.sd);
      .legend <- c(.legend, "mean+sd");
      .colors <- c(.colors, mean.plus.sd.col);
      .lwd <- c(.lwd, mean.plus.sd.lwd);
    }
  }
  if(isTRUE(q159.add)) {
    .q159 <- pnorm(-1);
    stopifnot(is.numeric(.q159),
              length(.q159) == 1L,
              is.finite(.q159));
    .q159 <- quantile(data, .q159);
    stopifnot(is.numeric(.q159),
              length(.q159) == 1L,
              is.finite(.q159));
    .add <- c(.add, .q159);
    .legend <- c(.legend, "15.9% quantile");
    .colors <- c(.colors, q159.col);
    .lwd <- c(.lwd, q159.lwd);
  }

  if(isTRUE(median.add)) {
    .median <- median(data);
    stopifnot(is.finite(.median));
    .add <- c(.add, .median);
    .legend <- c(.legend, "median");
    .colors <- c(.colors, median.col);
    .lwd <- c(.lwd, median.lwd);
  }

  if(isTRUE(q841.add)) {
    .q841 <- pnorm(1);
    stopifnot(is.numeric(.q841),
              length(.q841) == 1L,
              is.finite(.q841));
    .q841 <- quantile(data, .q841);
    stopifnot(is.numeric(.q841),
              length(.q841) == 1L,
              is.finite(.q159));
    .add <- c(.add, .q841);
    .legend <- c(.legend, "84.1% quatile");
    .colors <- c(.colors, q841.col);
    .lwd <- c(.lwd, q841.lwd);
  }

  pars$x <- data;
  if((is.null(pars$xlim) || all(is.na(pars$xlim))) &&
     (length(.add) > 0L)) {
    pars$xlim <- range(unname(unlist(c(data, .add))));
  }

  pars$freq <- FALSE;
  pars$col <- c("#AAAAAA", "#CCCCCC");
  result <- do.call(hist, pars);
  stopifnot(all(is.finite(result$counts)),
            all(is.finite(result$density)),
            sum(result$counts) == length(data));
  ds <- sum(vapply(seq_along(result$density),
                   function(i) result$density[[i]]*
                     (result$breaks[[i+1L]]-
                     result$breaks[[i]]), NA_real_));
  stopifnot(ds > 0.999999,
            ds < 1.000001);


  back <- "#FFFFFFBB";
  .make.lwd <- function(x) round(max(2+x, 1+(1.75*x)));
  if(isTRUE(density.add)) {
    dens <- density(data);
    lines(dens, col=back, lwd=.make.lwd(density.lwd));
  }
  if(isTRUE(normal.add)) {
    norm.f <- function(x) dnorm(x, mean=.mean, sd=.sd);
    curve(norm.f, col=back,
          lwd=.make.lwd(normal.lwd), add=TRUE);
  }

  if(isTRUE(median.add) ||
     isTRUE(mean.add) ||
     isTRUE(mean.minus.sd.add) ||
     isTRUE(mean.plus.sd.add) ||
     isTRUE(q159.add) ||
     isTRUE(q841.add)) {
    y.max <- max(result$density)
    stopifnot(is.finite(y.max),
              y.max > 0,
              y.max <= 1);
    y.min <- -0.075 * y.max;
    y.max <- 2*y.max/3;
    for(i in seq_along(.add)) {
      lines(x=c(.add[[i]], .add[[i]]),
            y=c(y.min, y.max),
            col=back,
            lwd=.make.lwd(.lwd[[i]]));
    }
  }


  if(isTRUE(density.add)) {
    lines(dens, col=density.col,
          lwd=density.lwd);
  }
  if(isTRUE(normal.add)) {
    curve(norm.f, col=normal.col, lwd=normal.lwd, add=TRUE);
  }

  if(isTRUE(median.add) ||
     isTRUE(mean.add) ||
     isTRUE(mean.minus.sd.add) ||
     isTRUE(mean.plus.sd.add) ||
     isTRUE(q159.add) ||
     isTRUE(q841.add)) {
    for(i in seq_along(.add)) {
      lines(x=c(.add[[i]], .add[[i]]),
            y=c(y.min, y.max),
            col=.colors[[i]],
            lwd=.lwd[[i]]);
    }
  }

  if(isTRUE(density.add)) {
    .legend <- c(.legend, "est. density");
    .colors <- c(.colors, density.col);
    .lwd <- c(.lwd, density.lwd);
  }
  if(isTRUE(normal.add)) {
    .legend <- c(.legend, paste0("N(\u03BC\u2248",
                                 signif(.mean, 3L),
                                 ",\u03C3\u2248",
                                 signif(.sd, 3L),
                                 ")"));
    .colors <- c(.colors, normal.col);
    .lwd <- c(.lwd, normal.lwd);
  }

  if(!(is.null(legend.pos) || is.na(legend.pos))) {
    aitoa.legend.main(x=legend.pos,
                      legend=.legend,
                      col=.colors,
                      lwd=.lwd,
                      bg="#FFFFFFDD");
  }

  if(!(is.null(label.y) || is.na(label.y))) {
    aitoa.legend.label(x="topleft",
                       legend=label.y);
  }
  if(!(is.null(label.x) || is.na(label.x))) {
    aitoa.legend.label(x="bottomright",
                       label.x);
  }

  .safe.par(old.par);
  invisible(NULL);
}

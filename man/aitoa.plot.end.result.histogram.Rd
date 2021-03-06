% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_end_result_histogram.R
\name{aitoa.plot.end.result.histogram}
\alias{aitoa.plot.end.result.histogram}
\title{Plot a Density Histogram of the End Results}
\usage{
aitoa.plot.end.result.histogram(
  end.results,
  instance,
  algorithm,
  mean.add = TRUE,
  mean.lwd = if (median.add) 4L else 3L,
  mean.col = "#FF0000",
  mean.minus.sd.add = TRUE,
  mean.minus.sd.lwd = if (q159.add) 4L else 3L,
  mean.minus.sd.col = "#FF7777",
  mean.plus.sd.add = TRUE,
  mean.plus.sd.lwd = if (q841.add) 4L else 3L,
  mean.plus.sd.col = "#880000",
  median.add = TRUE,
  median.lwd = 3L,
  median.col = "#0000FF",
  q159.add = TRUE,
  q159.lwd = 3L,
  q159.col = "#7777FF",
  q841.add = TRUE,
  q841.lwd = 3L,
  q841.col = "#000088",
  density.add = TRUE,
  density.lwd = 3L,
  density.col = "#00FF00",
  normal.add = TRUE,
  normal.lwd = 3L,
  normal.col = "#008800",
  dimension = "best.f",
  legend.cex = .legend.cex,
  legend.bg = .legend.bg,
  mgp = .default.mgp,
  tck = .default.tck,
  cex = .default.cex,
  mar = .default.mar.without.labels,
  legend.pos = "topright",
  label.y = "density",
  label.x = if (dimension == "best.f") "f" else dimension,
  ...
)
}
\arguments{
\item{end.results}{the end results loaded from the data}

\item{instance}{the instance to plot}

\item{algorithm}{the algorithm to plot}

\item{mean.add}{should a marker for the arithmetic mean be added?}

\item{mean.lwd}{the line width for the marker for the mean}

\item{mean.col}{the color for the marker for the mean}

\item{mean.minus.sd.add}{should a marker for mean-standard deviation be
added?}

\item{mean.minus.sd.lwd}{the line width for the marker for mean-sd}

\item{mean.minus.sd.col}{the color for the marker for mean-sd}

\item{mean.plus.sd.add}{should a marker for the mean+standard deviation be
added?}

\item{mean.plus.sd.lwd}{the line width for the marker for mean+sd}

\item{mean.plus.sd.col}{the color for the marker for mean+sd}

\item{median.add}{should a marker for the median be added?}

\item{median.lwd}{the line width for the marker for the median}

\item{median.col}{the color for the marker for the median}

\item{q159.add}{should a marker for the 15.9\% quantile be added?}

\item{q159.lwd}{the line width for the marker for the 15.9\% quantile}

\item{q159.col}{the color for the marker for the 15.9\% quantile}

\item{q841.add}{should a marker for the 84.1\% quantile be added?}

\item{q841.lwd}{the line width for the marker for the 84.1\% quantile}

\item{q841.col}{the color for the marker for the 84.1\% quantile}

\item{density.add}{should a curve illustrating a density estimate be added?}

\item{density.lwd}{the line width for the density estimate curve}

\item{density.col}{the color for the density estimate curve}

\item{normal.add}{should a normal distribution probability density function
be added?}

\item{normal.lwd}{the line width for the normal PDF}

\item{normal.col}{the color for the normal PDF}

\item{dimension}{the dimension to be plotted}

\item{legend.cex}{the character scaling for the legend}

\item{legend.bg}{the background color for the legend}

\item{mgp}{the mgp parameter to be passed to \link[graphics]{plot}}

\item{tck}{the tck parameter to be passed to \link[graphics]{plot}}

\item{cex}{the default character scaling}

\item{mar}{the default margins}

\item{legend.pos}{the position for the legend (set to \code{NULL} or
\code{NA} to omit legend)}

\item{label.y}{the y-label (set to \code{NULL} or \code{NA} to omit y-label)}

\item{label.x}{the x-label (set to \code{NULL} or \code{NA} to omit x-label)}

\item{...}{parameters to be passed to \link[graphics]{hist}}
}
\description{
This function plots a density histogram of the end results
}

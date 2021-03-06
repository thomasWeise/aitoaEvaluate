% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_rgb_luminoscity.R
\name{aitoa.scale.rgb.luminosity}
\alias{aitoa.scale.rgb.luminosity}
\title{Scale the Luminosity of a Color}
\usage{
aitoa.scale.rgb.luminosity(color, scale, limit = 255L, make.int = (limit > 1L))
}
\arguments{
\item{color}{the r-g-b vector}

\item{scale}{the scale value, in \code{[-1,1]}}

\item{limit}{the maximum permitted value for each rgb-coordinate: defaults to
255}

\item{make.int}{should the result be converted to integer?}
}
\value{
a color with similar chromotographical properties but either lighter
  (scale>0) or darker (scale<0) complexion.
}
\description{
The luminosity of a color is scaled in a range of \code{[-1,1]},
  where -1 means black, 1 means white, and 0 means the color at its current
  luminosity. A value in \code{(0, 1]} will make the color brighter while
  trying to preserve its chromotographical properties. A value in \code{[-1,
  0)} will make the color darker while trying to preserve its
  chromotographical properties. See
  \url{https://stackoverflow.com/questions/6615002}
}

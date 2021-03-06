% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_numbers.R
\name{aitoa.format.fractional.number}
\alias{aitoa.format.fractional.number}
\title{Format a Fractional Number to a Text}
\usage{
aitoa.format.fractional.number(v, put.dollars = TRUE)
}
\arguments{
\item{v}{the number}

\item{put.dollars}{if it contains maths, should the number be surrounded in
\code{$} signs?}
}
\value{
the string representation
}
\description{
Transform a finite number from (-1, 1) to a pleasant string.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_jssp_experiment.R
\name{aitoa.evaluate.jssp.experiment}
\alias{aitoa.evaluate.jssp.experiment}
\title{Evaluate the Results of the JSSP Experiment}
\usage{
aitoa.evaluate.jssp.experiment(
  results.dir = ".",
  evaluation.dir = file.path(results.dir, "..", "evaluation"),
  graphics.type = c("svg", "pdf", "eps", "png")
)
}
\arguments{
\item{results.dir}{the results directory}

\item{evaluation.dir}{the evaluation directory}

\item{graphics.type}{the type of graphics to generate}
}
\description{
Process the results of the JSSP experiment.
}
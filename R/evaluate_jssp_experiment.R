#' @title Evaluate the Results of the JSSP Experiment
#' @description Process the results of the JSSP experiment.
#' @param results.dir the results directory
#' @param evaluation.dir the evaluation directory
#' @param graphics.type the type of graphics to generate
#' @include utils.R
#' @include load_end_result_stats.R
#' @export aitoa.evaluate.jssp.experiment
aitoa.evaluate.jssp.experiment <- function(results.dir=".",
                                           evaluation.dir=file.path(results.dir, "..", "evaluation"),
                                           graphics.type=c("svg", "pdf", "eps", "png")) {
  results.dir <- .dir.exists(results.dir);
  evaluation.dir <- .dir.exists(evaluation.dir);
  graphics.type <- match.arg(graphics.type);

  .logger("Now processing the Results of the JSSP Experiment.");

  end.result.stats <- aitoa.load.end.result.stats(file.path(evaluation.dir, "endResultStatistics.txt"));
  .check.end.result.stats(end.result.stats);

  .logger("Done processing the Results of the JSSP Experiment.");
  invisible(NULL);
}

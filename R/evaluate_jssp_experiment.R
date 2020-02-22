#' @title Evaluate the Results of the JSSP Experiment
#' @description Process the results of the JSSP experiment.
#' @param results.dir the results directory
#' @param evaluation.dir the evaluation directory
#' @param graphics.type the type of graphics to generate
#' @include utils.R
#' @include load_end_result_stats.R
#' @include graphic_out.R
#' @include plot_gantt_for_stat_on_multiple_instances.R
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

  instances <- c("abz7", "la24", "swv15", "yn4");
  instances.limit <- c(656L, 935L, 2885L, 929L);
  instances.limit.name <- "LB\u066D"
  width <- 6;
  height <- 8.6;

  larger.mar <- .default.mar.without.labels;
  larger.mar[[4L]] = 1;

  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_1rs_med",
                type = graphics.type,
                width = width,
                height = height,
                body = {
    aitoa.plot.gantt.for.stat.on.multiple.instances(
      end.result.stats = end.result.stats,
      results.dir = results.dir,
      algorithm = "1rs",
      instances = instances,
      print.job.names = FALSE);
                });


  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_rs_med",
                type = graphics.type,
                width = width,
                height = height,
                body = {
                  aitoa.plot.gantt.for.stat.on.multiple.instances(
                    end.result.stats = end.result.stats,
                    results.dir = results.dir,
                    algorithm = "rs",
                    instances = instances,
                    print.job.names = FALSE,
                    mar.single = list(NA, larger.mar, NA, NA));
                });


  .logger("Done processing the Results of the JSSP Experiment.");
  invisible(NULL);
}

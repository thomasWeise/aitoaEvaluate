#' @title Evaluate the Results of the JSSP Experiment
#' @description Process the results of the JSSP experiment.
#' @param results.dir the results directory
#' @param evaluation.dir the evaluation directory
#' @param skip.if.exists skip all existing evaluation diagrams and files
#' @param graphics.type the type of graphics to generate
#' @include utils.R
#' @include load_end_result_stats.R
#' @include graphic_out.R
#' @include plot_gantt_for_stat_on_multiple_instances.R
#' @export aitoa.evaluate.jssp.experiment
aitoa.evaluate.jssp.experiment <- function(results.dir=".",
                                           evaluation.dir=file.path(results.dir, "..", "evaluation"),
                                           skip.if.exists = TRUE,
                                           graphics.type=c("svg", "pdf", "eps", "png")) {
  results.dir <- .dir.exists(results.dir);
  evaluation.dir <- .dir.exists(evaluation.dir);
  graphics.type <- match.arg(graphics.type);

  .logger("Now processing the Results of the JSSP Experiment.");

  end.result.stats <- aitoa.load.end.result.stats(file.path(evaluation.dir, "endResultStatistics.txt"));
  .check.end.result.stats(end.result.stats);

  max.time <- as.integer(1000L * 3L * 60L);

  instances <- c("abz7", "la24", "swv15", "yn4");
  instances.limit <- c(656L, 935L, 2885L, 929L);
  instances.limit.name <- "LB*"
  width <- 6;
  height <- 8.6;

  larger.mar <- .default.mar.without.labels;
  larger.mar[[4L]] = 0.87;

  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_1rs_med",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
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
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.gantt.for.stat.on.multiple.instances(
                    end.result.stats = end.result.stats,
                    results.dir = results.dir,
                    algorithm = "rs",
                    instances = instances,
                    print.job.names = FALSE,
                    mar.single = list(NA, larger.mar, NA, NA));
                });

  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_rs_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                aitoa.plot.progress.on.multiple.instances(
                  results.dir=results.dir,
                  algorithms="rs",
                  instances=instances,
                  time.column = "t",
                  max.time = max.time,
#                 instances.limit = instances.limit,
#                 instances.limit.name = instances.limit.name,
                  log = "x"
                )
              });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_1rs_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
    aitoa.make.stat.table.md(
    end.result.stats,
    algorithms="1rs",
    instances=instances,
    instances.limit=instances.limit,
    mark.smallest.stat = FALSE
  ) } );

  aitoa.text(directory = evaluation.dir,
             name = "jssp_rs_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
  aitoa.make.stat.table.md(
    end.result.stats,
    algorithms=c("1rs", "rs"),
    instances=instances,
    instances.limit=instances.limit
  ) } );

  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_hc_1swap_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.progress.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=c("rs", "hc_1swap"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });

  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_hc_1swap_med",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.gantt.for.stat.on.multiple.instances(
                    end.result.stats = end.result.stats,
                    results.dir = results.dir,
                    algorithm = "hc_1swap",
                    instances = instances,
                    print.job.names = FALSE#,
#                   mar.single = list(NA, larger.mar, NA, NA))
                  )
                });


  .logger("Done processing the Results of the JSSP Experiment.");
  invisible(NULL);
}

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
  instances.limit.name <- "lb*"
  instances.symbols <- c(1L, 2L, 5L, 6L);
  width <- 6;
  height <- 8.6;

  larger.mar.1 <- .default.mar.without.labels;
  larger.mar.1[[4L]] = 0.87;
  larger.mar.2 <- .default.mar.without.labels;
  larger.mar.2[[4L]] = 1.38;

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
                    mar.single = list(NA, larger.mar.1, NA, NA));
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
                  )
                });


  aitoa.graphic(evaluation.dir,
                name = "jssp_hc_rs_med_over_l",
                type = graphics.type,
                width = width,
                skip.if.exists = skip.if.exists,
                body = {
                  x <- as.integer(2^(7L:18L));
                  aitoa.plot.stat.over.param(
                    end.result.stats,
                    algorithm.template = "hc_rs_$arg_1swap",
                    algorithm.args=x,
                    instances=instances,
                    log="x",
                    instance.pch=instances.symbols,
                    statistic="best.f.median",
                    divide.by=instances.limit,
                    x.axis.at=x,
                    mar=larger.mar.2);
                  aitoa.legend.label("topleft",
                                     paste0("best f / ",
                                            instances.limit.name));
                  aitoa.legend.label("bottomright",
                                     "restart limit L");
                });


  .logger("Done processing the Results of the JSSP Experiment.");
  invisible(NULL);
}

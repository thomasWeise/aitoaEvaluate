
# An internal function to create a table with the
# Key statistics for simulated annealing
.make.sa.table <- function(end.result.stats,
                           algorithm="hc_1swap",
                           instances=c("abz7", "la24", "swv15", "yn4")) {
  end.result.stats <- end.result.stats[
    end.result.stats$algorithm == algorithm,
    c("instance", "total.fes.median",
      "best.f.sd")
  ];

  formats <- aitoa.statistics.cols.to.formats(c("total.fes.median",
                                                "best.f.sd"));

  stopifnot(nrow(end.result.stats) == length(instances));

  end.result.stats <- end.result.stats[order(end.result.stats$instance), ];
  unname(unlist(
            c("|$\\instance$|med(total FEs)|sd|",
              "|--:|--:|--:|",
            vapply(seq_len(nrow(end.result.stats)),
                   function(r) {
              row <- end.result.stats[r, ];
              paste0("|`", row$instance, "`|",
                     formats[[1L]](row$total.fes.median),
                     "|",
                     formats[[2L]](row$best.f.sd),
                     "|");
                   }, NA_character_),
            paste0("|median|",
                   formats[[1L]](median(end.result.stats$total.fes.median)),
                   "|",
                   formats[[2L]](median(end.result.stats$best.f.sd)),
                   "|"))));

}


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
#' @include distinct_styles.R
#' @export aitoa.evaluate.jssp.experiment
aitoa.evaluate.jssp.experiment <- function(results.dir=".",
                                           evaluation.dir=file.path(results.dir, "..", "evaluation"),
                                           skip.if.exists = TRUE,
                                           graphics.type=c("svg", "pdf", "eps", "png")) {
  results.dir <- .dir.exists(results.dir);
  evaluation.dir <- .dir.exists(evaluation.dir);
  graphics.type <- match.arg(graphics.type);

  .logger("Now processing the Results of the JSSP Experiment.");

  end.result.stats <- aitoa.load.end.result.stats(
    file.path(evaluation.dir, "endResultStatistics.txt"));
  .check.end.result.stats(end.result.stats);

  end.results <- aitoa.load.end.results(
    file.path(evaluation.dir, "endResults.txt"));
  .check.end.results(end.results);

  max.time <- as.integer(1000L * 3L * 60L);

  instances <- c("abz7", "la24", "swv15", "yn4");
  instances.limit <- c(656L, 935L, 2885L, 929L);
  instances.limit.name <- "lb*"
  instances.symbols <- aitoa.distinct.pch(4L);
  instance.gantt.job.name.cex <- c(0.7, 0.8, 0.4, 0.6);
  width <- 6;
  height <- 8.6;

  larger.mar.1 <- .default.mar.without.labels;
  larger.mar.1[[4L]] = 0.87;
  larger.mar.2 <- .default.mar.without.labels;
  larger.mar.2[[4L]] = 1.38;
  larger.mar.3 <- .default.mar.without.labels;
  larger.mar.3[[4L]] = 1.5;

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
      print.job.names = TRUE,
      job.name.cex = instance.gantt.job.name.cex);
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
                    mar.single = list(NA, larger.mar.1, NA, NA),
                    print.job.names = TRUE,
                    job.name.cex = instance.gantt.job.name.cex);
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
                  aitoa.plot.progress.stat.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=c("rs", "hc_1swap"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_hc_1swap_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.stat.table.md(
                 end.result.stats,
                 algorithms=c("1rs", "rs", "hc_1swap"),
                 instances=instances,
                 instances.limit=instances.limit
               ) } );

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
                    print.job.names = TRUE,
                    job.name.cex = instance.gantt.job.name.cex
                  )
                });


  aitoa.graphic(evaluation.dir,
                name = "jssp_hcr_1swap_med_over_l",
                type = graphics.type,
                width = width,
                skip.if.exists = skip.if.exists,
                body = {
                  x <- as.integer(2^(7L:18L));
                  aitoa.plot.stat.over.param(
                    end.result.stats,
                    algorithm.template = "hc_rs_$arg1_1swap",
                    algorithm.primary.args=x,
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
                  aitoa.legend.label("top",
                                     "hcr_L_1swap");
                });


  aitoa.text(directory = evaluation.dir,
             name = "jssp_hcr_1swap_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.stat.table.md(
                 end.result.stats,
                 algorithms=list(hc_1swap="hc_1swap",
                                 hcr_8192_1swap="hc_rs_8192_1swap",
                                 hcr_16384_1swap="hc_rs_16384_1swap",
                                 hcr_32768_1swap="hc_rs_32768_1swap"),
                 instances=instances,
                 instances.limit=instances.limit
               ) } );


  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_hcr_1swap_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.progress.stat.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=list(rs="rs",
                                    hc_1swap="hc_1swap",
                                    hcr_16384_1swap="hc_rs_16384_1swap"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });

  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_hcr_16384_1swap_med",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.gantt.for.stat.on.multiple.instances(
                    end.result.stats = end.result.stats,
                    results.dir = results.dir,
                    algorithm = "hc_rs_16384_1swap",
                    instances = instances,
                    print.job.names = TRUE,
                    job.name.cex = instance.gantt.job.name.cex
                  )
                });


  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_hc_nswap_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.progress.stat.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=list(hc_1swap="hc_1swap",
                                    hc_nswap="hc_nswap"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_hc_nswap_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.stat.table.md(
                 end.result.stats,
                 algorithms=list(hc_1swap="hc_1swap",
                                 hcr_16384_1swap="hc_rs_16384_1swap",
                                 hc_nswap="hc_nswap"),
                 instances=instances,
                 instances.limit=instances.limit
               ) } );

  aitoa.graphic(evaluation.dir,
                name = "jssp_hcr_nswap_med_over_l",
                type = graphics.type,
                width = width,
                skip.if.exists = skip.if.exists,
                body = {
                  x <- as.integer(2^(7L:18L));
                  aitoa.plot.stat.over.param(
                    end.result.stats,
                    algorithm.template = "hc_rs_$arg1_nswap",
                    algorithm.primary.args=x,
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
                  aitoa.legend.label("top",
                                     "hcr_L_nswap");
                });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_hcr_nswap_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.stat.table.md(
                 end.result.stats,
                 algorithms=list(hcr_16384_1swap="hc_rs_16384_1swap",
                                 hc_nswap="hc_nswap",
                                 hcr_32768_nswap="hc_rs_32768_nswap",
                                 hcr_65536_nswap="hc_rs_65536_nswap"),
                 instances=instances,
                 instances.limit=instances.limit
               ) } );

  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_hcr_nswap_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.progress.stat.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=list(hcr_16384_1swap="hc_rs_16384_1swap",
                                    hc_nswap="hc_nswap",
                                    hcr_32768_nswap="hc_rs_32768_nswap",
                                    hcr_65536_nswap="hc_rs_65536_nswap"),
                      #list(hcr_16384_1swap="hc_rs_16384_1swap",
                      #              hcr_65536_nswap="hc_rs_65536_nswap"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });

  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_hcr_65536_nswap_med",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.gantt.for.stat.on.multiple.instances(
                    end.result.stats = end.result.stats,
                    results.dir = results.dir,
                    algorithm = "hc_rs_65536_nswap",
                    instances = instances,
                    print.job.names = TRUE,
                    job.name.cex = instance.gantt.job.name.cex
                  )
                });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_hcr_comparison",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.end.result.test.table.md(
                 end.results,
                 algorithms=list(hcr_16384_1swap="hc_rs_16384_1swap",
                                 hcr_65536_nswap="hc_rs_65536_nswap"),
                 instances=instances
               ) } );


  aitoa.graphic(evaluation.dir,
                name = "jssp_ea_nocr_med_over_mu",
                type = graphics.type,
                width = width,
                skip.if.exists = skip.if.exists,
                body = {
                  x <- as.integer(2^(7L:16L));
                  aitoa.plot.stat.over.param(
                    end.result.stats,
                    algorithm.template = "ea_$arg1+$arg1@0d0_$arg2_sequence",
                    algorithm.primary.args=x,
                    algorithm.secondary.args = c("1swap", "nswap"),
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
                                     "\u03BC=\u03BB");
                  aitoa.legend.label("top",
                                     "ea_mu_unary");
                });

  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_ea_nocr_nswap_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.progress.stat.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=list(hcr_65536_nswap="hc_rs_65536_nswap",
                                    hc_nswap="hc_nswap",
                                    ea_16384_nswap="ea_16384+16384@0d0_nswap_sequence",
                                    ea_1024_nswap="ea_1024+1024@0d0_nswap_sequence"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });

  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_ea_16384_nocr_nswap_med",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.gantt.for.stat.on.multiple.instances(
                    end.result.stats = end.result.stats,
                    results.dir = results.dir,
                    algorithm = "ea_16384+16384@0d0_nswap_sequence",
                    instances = instances,
                    print.job.names = TRUE,
                    job.name.cex = instance.gantt.job.name.cex
                  )
                });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_ea_nocr_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.stat.table.md(
                 end.result.stats,
                 algorithms=list(hc_nswap="hc_nswap",
                                 hcr_65536_nswap="hc_rs_65536_nswap",
                                 ea_16384_nswap="ea_16384+16384@0d0_nswap_sequence",
                                 ea_1024_nswap="ea_1024+1024@0d0_nswap_sequence"),
                 instances=instances,
                 instances.limit=instances.limit
               ) } );

  aitoa.graphic(evaluation.dir,
                name = "jssp_ea_cr_med_over_cr",
                type = graphics.type,
                width = width,
                skip.if.exists = skip.if.exists,
                body = {
                  x <- as.integer(2^(7L:16L));
                  aitoa.plot.stat.over.param(
                    end.result.stats,
                    algorithm.template = "ea_$arg1+$arg1@XXX_nswap_sequence",
                    algorithm.primary.args=x,
                    algorithm.secondary.args = c("cr=0", "cr=0.05", "cr=0.3", "cr=0.98"),
                    algorithm.secondary.filler = function(t, a, b) {
                      b <- as.numeric(substr(b, 4L, nchar(b)));
                      rep <- gsub(".", "d", as.character(b), fixed=TRUE);
                      if(rep == "0") { rep <- "0d0"; }
                      gsub("XXX", rep, t, fixed=TRUE);
                    },
                    instances=instances,
                    log="x",
                    instance.pch=instances.symbols,
                    statistic="best.f.median",
                    divide.by=instances.limit,
                    x.axis.at=x,
                    ylim=c(1.025,1.5),
                    mar=larger.mar.2,);
                  aitoa.legend.label("topleft",
                                     paste0("best f / ",
                                            instances.limit.name));
                  aitoa.legend.label("bottomright",
                                     "\u03BC=\u03BB");
                  aitoa.legend.label("top",
                                     "ea_mu_cr_nswap");
                });

  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_ea_cr_nswap_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.progress.stat.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=list(ea_16384_nswap="ea_16384+16384@0d0_nswap_sequence",
                                    ea_8192_nswap="ea_8192+8192@0d0_nswap_sequence",
                                    `ea_8192_5%_nswap`="ea_8192+8192@0d05_nswap_sequence"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });


  aitoa.text(directory = evaluation.dir,
             name = "jssp_ea_cr_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.stat.table.md(
                 end.result.stats,
                 algorithms=list(ea_16384_nswap="ea_16384+16384@0d0_nswap_sequence",
                                 ea_1024_nswap="ea_1024+1024@0d0_nswap_sequence",
                                 ea_8192_nswap="ea_8192+8192@0d0_nswap_sequence",
                                 `ea_8192_5%_nswap`="ea_8192+8192@0d05_nswap_sequence"),
                 instances=instances,
                 instances.limit=instances.limit
               ) } );

  aitoa.text(directory = evaluation.dir,
             name = "jssp_ea_cr_comparison_8192",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.end.result.test.table.md(
                 end.results,
                 algorithms=list(ea_8192_nswap="ea_8192+8192@0d0_nswap_sequence",
                                 `ea_8192_5%_nswap`="ea_8192+8192@0d05_nswap_sequence"),
                 instances=instances
               ) } );

  aitoa.text(directory = evaluation.dir,
             name = "jssp_ea_cr_comparison_16384",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.end.result.test.table.md(
                 end.results,
                 algorithms=list(ea_16384_nswap="ea_16384+16384@0d0_nswap_sequence",
                                 `ea_8192_5%_nswap`="ea_8192+8192@0d05_nswap_sequence"),
                 instances=instances
               ) } );


  aitoa.graphic(evaluation.dir,
                name = "jssp_eac_med_over_mu",
                type = graphics.type,
                width = width,
                skip.if.exists = skip.if.exists,
                body = {
                  x <- as.integer(2^(2L:16L));
                  aitoa.plot.stat.over.param(
                    end.result.stats,
                    algorithm.template = "ea$clearing_$arg1+$arg1@0d05_nswap_sequence",
                    algorithm.primary.args=x,
                    algorithm.secondary.args = c("normal", "with clearing"),
                    algorithm.secondary.filler = function(t, a, b) {
                      clearing <- "";
                      if(grepl("clearing", b, fixed=TRUE)) clearing <- "c";
                      t <- gsub("$clearing", clearing, t, fixed=TRUE);
                      return(t);
                    },
                    instances=instances,
                    log="x",
                    instance.pch=instances.symbols,
                    statistic="best.f.median",
                    divide.by=instances.limit,
                    x.axis.at=x,
                    mar=larger.mar.3);
                  aitoa.legend.label("topleft",
                                     paste0("best f / ",
                                            instances.limit.name));
                  aitoa.legend.label("bottomright",
                                     "\u03BC=\u03BB");
                  aitoa.legend.label("top",
                                     "ea[c]_mu_5%_nswap");
                });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_eac_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.stat.table.md(
                 end.result.stats,
                 algorithms=list(`ea_8192_5%_nswap`="ea_8192+8192@0d05_nswap_sequence",
                                 `eac_4_5%_nswap`="eac_4+4@0d05_nswap_sequence"),
                 instances=instances,
                 instances.limit=instances.limit
               ) } );


  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_eac_nswap_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.progress.stat.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=list(hcr_65536_nswap="hc_rs_65536_nswap",
                                    `ea_8192_5%_nswap`="ea_8192+8192@0d05_nswap_sequence",
                                    `eac_4_5%_nswap`="eac_4+4@0d05_nswap_sequence"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });


  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_eac_4_0d05_nswap_med",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.gantt.for.stat.on.multiple.instances(
                    end.result.stats = end.result.stats,
                    results.dir = results.dir,
                    algorithm = "eac_4+4@0d05_nswap_sequence",
                    instances = instances,
                    print.job.names = TRUE,
                    job.name.cex = instance.gantt.job.name.cex
                  )
                });


  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_eac_4_0d05_nswap_la24_min",
                type = graphics.type,
                width = width,
                height = 0.25 * height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.gantt.for.stat(
                    end.result.stats = end.result.stats,
                    statistic = "best.f.min",
                    results.dir = results.dir,
                    algorithm = "eac_4+4@0d05_nswap_sequence",
                    instance = "la24",
                    print.job.names = TRUE,
                    job.name.cex = 1.2*instance.gantt.job.name.cex[[2L]]
                  )
                });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_hc1_swap_sa_params",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               .make.sa.table(
                 end.result.stats,
                 algorithm="hc_1swap",
                 instances=instances
               ) } );


  aitoa.graphic(evaluation.dir,
                name = "jssp_sa_1swap_med_over_epsilon",
                type = graphics.type,
                width = width,
                skip.if.exists = skip.if.exists,
                body = {
                  x <- c(0.25,
                         0.5,
                         1,
                         1.5,
                         2,
                         4,
                         8);
                  n <- c("000000025",
                         "00000005",
                         "0000001",
                         "00000015",
                         "0000002",
                         "0000004",
                         "0000008");
                  aitoa.plot.stat.over.param(
                    end.result.stats,
                    algorithm.template = "sa_exp_20_0d$eps_1swap",
                    algorithm.primary.args=x,
                    algorithm.primary.filler=function(t, a) {
                      gsub("$eps", n[x==a], t, fixed=TRUE);
                    },
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
                                     expression(paste("\u03B5*",10^7)));
                  aitoa.legend.label("top",
                                     "sa_exp_20_\u03B5_1swap");
                });

  aitoa.graphic(evaluation.dir,
                name = "sa_temperature_schedules",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.sa.temperature.plot()
                });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_sa_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.stat.table.md(
                 end.result.stats,
                 algorithms=list(hcr_16384_1swap="hc_rs_16384_1swap",
                                 `eac_4_5%_nswap`="eac_4+4@0d05_nswap_sequence",
                                 sa_exp_20_2_1swap="sa_exp_20_0d0000002_1swap"),
                 instances=instances,
                 instances.limit=instances.limit
               ) } );

  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_sa_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.progress.stat.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=list(hcr_16384_1swap="hc_rs_16384_1swap",
                                    `ea_8192_5%_nswap`="ea_8192+8192@0d05_nswap_sequence",
                                    `eac_4_5%_nswap`="eac_4+4@0d05_nswap_sequence",
                                     sa_exp_20_2_1swap="sa_exp_20_0d0000002_1swap"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });

  aitoa.graphic(evaluation.dir,
                name = "jssp_gantt_sa_exp_20_2_1swap_med",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.gantt.for.stat.on.multiple.instances(
                    end.result.stats = end.result.stats,
                    results.dir = results.dir,
                    algorithm = "sa_exp_20_0d0000002_1swap",
                    instances = instances,
                    print.job.names = TRUE,
                    job.name.cex = instance.gantt.job.name.cex
                  )
                });

  aitoa.graphic(evaluation.dir,
                name = "jssp_progress_hc2r_log",
                type = graphics.type,
                width = width,
                height = height,
                skip.if.exists = skip.if.exists,
                body = {
                  aitoa.plot.progress.stat.on.multiple.instances(
                    results.dir=results.dir,
                    algorithms=list(hcr_16384_1swap="hc_rs_16384_1swap",
                                    hc2r_1swap="hc2f_rs_1swap",
                                    hc2r_1swapU="hc2f_rs_1swapU"),
                    instances=instances,
                    time.column = "t",
                    max.time = max.time,
                    log = "x"
                  )
                });

  aitoa.text(directory = evaluation.dir,
             name = "jssp_hc2r_results",
             type = "md",
             trim.ws = TRUE,
             skip.if.exists = skip.if.exists,
             body = {
               aitoa.make.stat.table.md(
                 end.result.stats,
                 algorithms=list(hcr_16384_1swap="hc_rs_16384_1swap",
                                 hc2r_1swap="hc2f_rs_1swap",
                                 hc2r_1swapU="hc2f_rs_1swapU"),
                 instances=instances,
                 instances.limit=instances.limit
               ) } );

  .logger("Done processing the Results of the JSSP Experiment.");
  invisible(NULL);
}

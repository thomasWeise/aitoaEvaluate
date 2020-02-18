library("aitoaEvaluate");
library("testthat");
context("aitoa.load.log.file");

dir <- tempfile();
dir.create(dir);

.make.log.file <- function(values, consumedFEs, consumedTime) {
  algo <- paste0("algo", as.integer(runif(n=1,min=0,max=9999)));
  inst <- paste0("inst", as.integer(runif(n=1,min=0,max=9999)));
  file <- paste0(algo, "_", inst, "_0x");
  for(i in seq_len(16)) {
    file <- paste0(file, as.integer(runif(n=1L, min=0L, max=9L)));
  }
  file <- paste0(file, ".txt");
  file <- normalizePath(file.path(dir, algo, inst, file), mustWork = FALSE);
  dir.create(dirname(file), recursive = TRUE);
  writeLines(text=c(
    "",
    "# ALGORITHM_SETUP",
    "algorithm: rs",
    "algorithm_class: aitoa.algorithms.RandomSampling",
    "# END_ALGORITHM_SETUP",
    "",
    "# BEGIN_LOG",
    "# fbest;consumedFEs;consumedTimeMS",
    vapply(values, function(v) paste(v, sep=";", collapse=";"), ""),
    "# END_OF_LOG",
    "",
    "# BEGIN_SETUP",
    "# SEARCH_SPACE: jssp:int[100]:abz5:aitoa.examples.jssp.JSSPSearchSpace",
    "# NULLARY_OP: uniform",
    "# UNARY_OP: null",
    "# BINARY_OP: null",
    "# TERNARY_OP: null",
    "# SOLUTION_SPACE: jssp:gantt:abz5:aitoa.examples.jssp.JSSPSolutionSpace",
    "# REPRESENTATION_MAPPING: jssp:int[]-to-Gantt:aitoa.examples.jssp.JSSPRepresentationMapping",
    "# OBJECTIVE_FUNCTION: jssp:makespan:aitoa.examples.jssp.JSSPMakespanObjectiveFunction",
    "# MAX_FES: 9223372036854775807",
    "# MAX_TIME: 240000",
    "# GOAL_F: 1000.0",
    "# RANDOM_SEED: 0x1d831acc2bbfcf63",
    "# END_SETUP",
    "# BEGIN_SYSTEM",
    "# JAVA_VERSION: 13",
    "# END_SYSTEM",
    "# BEGIN_STATE",
    paste("# CONSUMED_FES: ", consumedFEs, sep="", collapse=""),
    "# LAST_IMPROVEMENT_FE: 26622668",
    paste("# CONSUMED_TIME: ", consumedTime, sep="", collapse=""),
    "# LAST_IMPROVEMENT_TIME: 126902",
    "# BEST_F: 1356.0",
    "# END_STATE",
    "",
    "# BEST_X",
    "new int[]{ 8, 5, 9, 3, 8, 0, 0, 9, 2, 1, 7, 4, 1, 9, 2, 6, 4, 5, 3, 8, 1, 8, 5, 0, 2, 7, 9, 6, 6, 5, 8, 7, 0, 2, 6, 4, 9, 1, 8, 7, 9, 6, 4, 3, 7, 2, 2, 2, 3, 3, 7, 0, 9, 5, 5, 0, 4, 3, 5, 7, 8, 6, 3, 3, 5, 2, 1, 0, 1, 3, 1, 5, 6, 8, 4, 6, 4, 8, 0, 3, 7, 8, 9, 7, 6, 1, 4, 6, 9, 1, 4, 1, 2, 7, 2, 5, 0, 0, 9, 4}",
    "# END_BEST_X",
    "# BEST_Y",
    "new aitoa.examples.jssp.JSSPCandidateSolution(new int[][]",
    "# END_BEST_Y"
  ), con=file);
  return(file);
}


test_that("Test aitoa.load.log.file 1", {
  file <- .make.log.file(list(c(11, 1, 33)),
                         1000,
                         180000);
  data <- aitoa.load.log.file(file);
  unlink(file);
  file2 <- attr(data, "file");
  expect_equal(file2, file);
  attr(data, "file") <- NULL;
  seed <- attr(data, "seed");
  expect_true(nchar(seed) == 18L);
  attr(data, "seed") <- NULL;
  algorithm <- attr(data, "algorithm");
  expect_true(nchar(algorithm) > 0L,
              startsWith(algorithm, "algo"));
  attr(data, "algorithm") <- NULL;
  instance <- attr(data, "instance");
  expect_true(nchar(instance) > 0L,
              startsWith(instance, "inst"));
  attr(data, "instance") <- NULL;
  expect_false(file.exists(file));

  expect_equal(nrow(data), 2L);
  expect_identical(colnames(data), c("fes", "t", "f"));
  expect_true(is.integer(data$f));
  expect_true(is.integer(data$fes));
  expect_true(is.integer(data$t));

  expect_identical(as.list(data[1L,]), list(fes=1L,    t=33L,     f=11L));
  expect_identical(as.list(data[2L,]), list(fes=1000L, t=180000L, f=11L));
})


test_that("Test aitoa.load.log.file 2", {
  file <- .make.log.file(list(c(11, 1, 33),
                              c( 9, 2, 35)),
                         1000,
                         180000);
  data <- aitoa.load.log.file(file);
  unlink(file);
  file2 <- attr(data, "file");
  expect_equal(file2, file);
  attr(data, "file") <- NULL;
  seed <- attr(data, "seed");
  expect_true(nchar(seed) == 18L);
  attr(data, "seed") <- NULL;
  expect_false(file.exists(file));
  algorithm <- attr(data, "algorithm");
  expect_true(nchar(algorithm) > 0L,
              startsWith(algorithm, "algo"));
  attr(data, "algorithm") <- NULL;
  instance <- attr(data, "instance");
  expect_true(nchar(instance) > 0L,
              startsWith(instance, "inst"));
  attr(data, "instance") <- NULL;

  expect_equal(nrow(data), 3L);
  expect_identical(colnames(data), c("fes", "t", "f"));
  expect_true(is.integer(data$f));
  expect_true(is.integer(data$fes));
  expect_true(is.integer(data$t));

  expect_identical(as.list(data[1L,]), list(fes=1L,    t=33L,     f=11L));
  expect_identical(as.list(data[2L,]), list(fes=2L,    t=35L,     f=9L));
  expect_identical(as.list(data[3L,]), list(fes=1000L, t=180000L, f=9L));
})



test_that("Test aitoa.load.log.file 3", {
  file <- .make.log.file(list(c(11, 1, 33),
                              c( 9, 2, 35),
                              c( 8, 1000, 180000)),
                         1000,
                         180000);
  data <- aitoa.load.log.file(file);
  unlink(file);
  file2 <- attr(data, "file");
  expect_equal(file2, file);
  attr(data, "file") <- NULL;
  seed <- attr(data, "seed");
  expect_true(nchar(seed) == 18L);
  attr(data, "seed") <- NULL;
  algorithm <- attr(data, "algorithm");
  expect_true(nchar(algorithm) > 0L,
              startsWith(algorithm, "algo"));
  attr(data, "algorithm") <- NULL;
  instance <- attr(data, "instance");
  expect_true(nchar(instance) > 0L,
              startsWith(instance, "inst"));
  attr(data, "instance") <- NULL;
  expect_false(file.exists(file));

  expect_equal(nrow(data), 3L);
  expect_identical(colnames(data), c("fes", "t", "f"));
  expect_true(is.integer(data$f));
  expect_true(is.integer(data$fes));
  expect_true(is.integer(data$t));

  expect_identical(as.list(data[1L,]), list(fes=1L,    t=33L,     f=11L));
  expect_identical(as.list(data[2L,]), list(fes=2L,    t=35L,     f=9L));
  expect_identical(as.list(data[3L,]), list(fes=1000L, t=180000L, f=8L));
})



test_that("Test aitoa.load.log.file 4", {
  file <- .make.log.file(list(c(11, 1, 33),
                              c( 9, 2, 35),
                              c( 8, 1000, 180000)),
                         1000,
                         180003);
  data <- aitoa.load.log.file(file);
  unlink(file);
  file2 <- attr(data, "file");
  expect_equal(file2, file);
  attr(data, "file") <- NULL;
  seed <- attr(data, "seed");
  expect_true(nchar(seed) == 18L);
  attr(data, "seed") <- NULL;
  algorithm <- attr(data, "algorithm");
  expect_true(nchar(algorithm) > 0L,
              startsWith(algorithm, "algo"));
  attr(data, "algorithm") <- NULL;
  instance <- attr(data, "instance");
  expect_true(nchar(instance) > 0L,
              startsWith(instance, "inst"));
  attr(data, "instance") <- NULL;
  expect_false(file.exists(file));

  expect_equal(nrow(data), 3L);
  expect_identical(colnames(data), c("fes", "t", "f"));
  expect_true(is.integer(data$f));
  expect_true(is.integer(data$fes));
  expect_true(is.integer(data$t));

  expect_identical(as.list(data[1L,]), list(fes=1L,    t=33L,     f=11L));
  expect_identical(as.list(data[2L,]), list(fes=2L,    t=35L,     f=9L));
  expect_identical(as.list(data[3L,]), list(fes=1000L, t=180000L, f=8L));
})


test_that("Test aitoa.load.log.file 5", {
  file <- .make.log.file(list(c(11, 1, 33),
                              c( 9, 2, 35),
                              c( 8, 1000, 180000)),
                         1002,
                         180003);
  data <- aitoa.load.log.file(file);
  unlink(file);
  file2 <- attr(data, "file");
  expect_equal(file2, file);
  attr(data, "file") <- NULL;
  seed <- attr(data, "seed");
  expect_true(nchar(seed) == 18L);
  attr(data, "seed") <- NULL;
  algorithm <- attr(data, "algorithm");
  expect_true(nchar(algorithm) > 0L,
              startsWith(algorithm, "algo"));
  attr(data, "algorithm") <- NULL;
  instance <- attr(data, "instance");
  expect_true(nchar(instance) > 0L,
              startsWith(instance, "inst"));
  attr(data, "instance") <- NULL;
  expect_false(file.exists(file));

  expect_equal(nrow(data), 4L);
  expect_identical(colnames(data), c("fes", "t", "f"));
  expect_true(is.integer(data$f));
  expect_true(is.integer(data$fes));
  expect_true(is.integer(data$t));

  expect_identical(as.list(data[1L,]), list(fes=1L,    t=33L,     f=11L));
  expect_identical(as.list(data[2L,]), list(fes=2L,    t=35L,     f=9L));
  expect_identical(as.list(data[3L,]), list(fes=1000L, t=180000L, f=8L));
  expect_identical(as.list(data[4L,]), list(fes=1002L, t=180003L, f=8L));
})


unlink(dir, force=TRUE, recursive=TRUE);
expect_false(dir.exists(dir));

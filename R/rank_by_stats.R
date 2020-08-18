#' @title Rank Algorithms by Statistics over Instances
#' @description Rank a set of algorithms over a set of instances by their statstics and return a data frame.
#' @param end.result.stats the data frame with the statistics
#' @param algorithms the list of algorithms
#' @param instances the instances
#' @param stat.cols the statistics columns to use for ranking
#' @return a data frame with the ranks
#' @include load_end_result_stats.R
#' @export aitoa.rank.by.stats
aitoa.rank.by.stats <- function(end.result.stats,
                                algorithms,
                                instances,
                                stat.cols=c("best.f.min",
                                            "best.f.mean",
                                            "best.f.median")) {
  .check.end.result.stats(end.result.stats);

  stopifnot(!is.null(algorithms),
            !any(is.na(algorithms)),
            is.character(algorithms),
            length(algorithms) > 0,
            all(vapply(algorithms, nchar, NA_integer_) > 0L),
            !is.null(instances),
            !any(is.na(instances)),
            is.character(instances),
            length(instances) > 0,
            all(vapply(instances, nchar, NA_integer_) > 0L),
            !is.null(stat.cols),
            !any(is.na(stat.cols)),
            is.character(stat.cols),
            length(stat.cols) > 0,
            all(vapply(stat.cols, nchar, NA_integer_) > 0L));

  # prepare statistic columns
  need.cols <- unname(unlist(c("instance", "algorithm", stat.cols)));
  end.result.stats <- end.result.stats[end.result.stats$algorithm %in% algorithms, need.cols];
  stopifnot(ncol(end.result.stats) == length(need.cols),
            all(colnames(end.result.stats) == need.cols));

  inst.rank.col.names <- unname(unlist(lapply(instances, function(instance) {
    vapply(stat.cols, function(stat) paste0("rank.", instance, ".", stat), NA_character_)
  })));
  stopifnot(is.character(inst.rank.col.names),
            length(inst.rank.col.names) > 0L,
            !any(is.na(inst.rank.col.names)),
            all(vapply(inst.rank.col.names, nchar, NA_integer_) > 0L));

  inst.rank.col.values <- unname(unlist(lapply(instances, function(instance) {
    lapply(stat.cols, function(stat) {
      data.raw <- vapply(algorithms,
                         function(algorithm) end.result.stats[
                           (end.result.stats$algorithm == algorithm) &
                           (end.result.stats$instance == instance), stat],
                         NA_real_);
      stopifnot(is.numeric(data.raw),
                !any(is.na(data.raw)),
                all(is.finite(data.raw) | ((is.infinite(data.raw) && (data.raw >= +Inf)))),
                length(data.raw) == length(algorithms));
      return(rank(data.raw));
    })
  }), recursive = FALSE));

  rank.mean <- vapply(seq_along(algorithms), function(i) {
    mean(vapply(inst.rank.col.values, `[[`, NA_real_, i))
  }, NA_real_);
  stopifnot(is.numeric(rank.mean),
            all(is.finite(rank.mean)),
            all(rank.mean > 0L),
            length(rank.mean) == length(algorithms));

  or <- order(rank.mean);

  col.names <- unname(unlist(c("algorithms", "rank.mean", inst.rank.col.names)));
  col.values <- unname(unlist(list(list(algorithms, rank.mean), inst.rank.col.values), recursive=FALSE));

  names(col.values) <- col.names;
  result <- do.call(data.frame, col.values);
  stopifnot(is.data.frame(result),
            ncol(result) == length(col.values),
            nrow(result) == length(algorithms),
            all(result$algorithms == algorithms),
            all(result$rank.mean == rank.mean));

  result <- result[or, ];
  rownames(result) <- NULL;

  return(result);
}

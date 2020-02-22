#' @title Make a Statistics Table
#' @description Create a table with standard statistics.
#' @param end.result.stats the data frame with the statistics
#' @param algorithms the list of algorithms, using \code{names} as their names
#' @param instances the instances, using \code{names} as their names
#' @param stat.cols the statistics columns to plot
#' @param instance.col the print name of the instance column
#' @param instance.format the format for the instance name
#' @param instances.limit an optional vector of instance limits
#' @param instances.limit.col an optional title for the instances limit column
#' @param instances.limit.format the formatter for the instances limits
#' @param algorithms.col the print name of the algorithms column
#' @param algorithm.format the algorithm format
#' @param stat.cols.format the formats for the statistics columns
#' @param mark.smallest.stat should the smallest statistic be marked (per
#'   column)
#' @param smallest.stat.start the start string of the smallest stat mark
#' @param smallest.stat.end the end string of the smallest stat mark
#' @return a character array with the data in markdown format
#' @include load_end_result_stats.R
#' @include format_numbers.R
#' @export aitoa.make.stat.table.md
aitoa.make.stat.table.md <- function(end.result.stats,
                                  algorithms,
                                  instances,
                                  stat.cols=list(best="best.f.min",
                                                 mean="best.f.mean",
                                                 med="best.f.median",
                                                 sd="best.f.sd",
                                                 `med(t)`="last.improvement.time.median",
                                                 `med(FEs)`="last.improvement.fes.median"),
                                  instance.col=if(length(instances) > 1L) "$\\inst.id$" else NULL,
                                  instance.format=aitoa.format.setup,
                                  instances.limit=NULL,
                                  instances.limit.col=if(is.null(instances.limit)) NULL else "$\\lowerBound{\\objf}$",
                                  instances.limit.format=aitoa.format.small.integer.objective.value,
                                  algorithms.col=if(length(algorithms) > 1L) "setup" else NULL,
                                  algorithm.format=aitoa.format.setup,
                                  stat.cols.format=aitoa.statistics.cols.to.formats(stat.cols),
                                  mark.smallest.stat=rep_len(TRUE, length(stat.cols)),
                                  smallest.stat.start="**",
                                  smallest.stat.end="**") {
  .check.end.result.stats(end.result.stats);

  stopifnot(!is.null(algorithms),
            !any(is.na(algorithms)),
            length(algorithms) > 0,
            all(vapply(algorithms, nchar, NA_integer_) > 0L),
            !is.null(instances),
            !any(is.na(instances)),
            length(instances) > 0,
            all(vapply(instances, nchar, NA_integer_) > 0L),
            is.list(stat.cols) || is.character(stat.cols),
            length(stat.cols) > 0);

# prepare statistic columns
  stat.cols.names <- names(stat.cols);
  stat.cols <- as.character(unname(unlist(stat.cols)));
  stopifnot(!is.null(stat.cols),
            is.character(stat.cols),
            length(stat.cols) > 0L,
            all(nchar(stat.cols) > 0L));
  if(is.null(stat.cols.names)) {
    stat.cols.names <- stat.cols;
  }
  stat.cols.names[is.na(stat.cols.names)] <- stat.cols[is.na(stat.cols.names)];
  stopifnot(!is.null(stat.cols.names),
            is.character(stat.cols.names),
            length(stat.cols.names) > 0L,
            all(nchar(stat.cols.names) > 0L));

# prepare instances
  instances.names <- names(instances);
  instances <- as.character(unname(unlist(instances)));
  stopifnot(!is.null(instances),
            is.character(instances),
            length(instances) > 0L,
            all(nchar(instances) > 0L));
  if(is.null(instances.names)) {
    instances.names <- instances;
  }
  instances.names[is.na(instances.names)] <- instances[is.na(instances.names)];
  stopifnot(!is.null(instances.names),
            is.character(instances.names),
            length(instances.names) > 0L,
            all(nchar(instances.names) > 0L));
  instances.names <- vapply(instances.names,
                            instance.format,
                            NA_character_);
  stopifnot(!is.null(instances.names),
            is.character(instances.names),
            length(instances.names) > 0L,
            all(nchar(instances.names) > 0L));

# prepare algorithms
  algorithms.names <- names(algorithms);
  algorithms <- as.character(unname(unlist(algorithms)));
  stopifnot(!is.null(algorithms),
            is.character(algorithms),
            length(algorithms) > 0L,
            all(nchar(algorithms) > 0L));
  if(is.null(algorithms.names)) {
    algorithms.names <- algorithms;
  }
  algorithms.names[is.na(algorithms.names)] <- algorithms[is.na(algorithms.names)];
  stopifnot(!is.null(algorithms.names),
            is.character(algorithms.names),
            length(algorithms.names) > 0L,
            all(nchar(algorithms.names) > 0L));
  algorithms.names <- vapply(algorithms.names,
                             algorithm.format,
                             NA_character_);
  stopifnot(!is.null(algorithms.names),
            is.character(algorithms.names),
            length(algorithms.names) > 0L,
            all(nchar(algorithms.names) > 0L));

  if(is.null(mark.smallest.stat)) {
    mark.smallest.stat <- FALSE;
  }
  mark.smallest.stat <- rep_len(mark.smallest.stat, length(stat.cols));
  stopifnot(!is.null(mark.smallest.stat),
            is.logical(mark.smallest.stat),
            !any(is.na(mark.smallest.stat)));


# select only the data
  need.cols <- unname(unlist(c("instance", "algorithm", stat.cols)));
  end.result.stats <- end.result.stats[, need.cols];
  stopifnot(ncol(end.result.stats) == length(need.cols),
            all(colnames(end.result.stats) == need.cols));

# set up what to print
  print.instances <- (length(instances) > 1) &&
                     (!is.null(instance.col)) &&
                     (!is.na(instance.col));
  if(print.instances) {
    stopifnot(is.character(instance.col),
              length(instance.col) == 1L,
              nchar(instance.col) > 0L);
  }
  print.instances.limit <- print.instances &&
    (!is.null(instances.limit)) &&
    (!is.null(instances.limit.col)) &&
    (!is.na(instances.limit.col));
  if(print.instances.limit) {
    stopifnot(is.character(instances.limit.col),
              length(instances.limit.col) == 1L,
              nchar(instances.limit.col) > 0L);
  }
  print.algorithms <- (length(algorithms) > 1L) &&
    (!is.null(algorithms.col) &&
       (!is.na(algorithms.col)));
  if(print.algorithms) {
    stopifnot(is.character(algorithms.col),
              length(algorithms.col) == 1L,
              nchar(algorithms.col) > 0L);
  }

# create header
  header.1 <- "";
  header.2 <- "";
  if(print.instances) {
    header.1 <- paste0("|", instance.col);
    header.2 <- "|:-:";
    if(print.instances.limit) {
      header.1 <- paste0(header.1, "|", instances.limit.col);
      header.2 <- paste0(header.2, "|--:");
    }
  }

  if(print.algorithms) {
    header.1 <- paste0(header.1, "|", algorithms.col);
    header.2 <- paste0(header.2, "|:--");
  }

  for(i in seq_along(stat.cols)) {
    header.1 <- paste0(header.1, "|", stat.cols.names[[i]]);
    header.2 <- paste0(header.2, "|--:");
  }

  header.1 <- paste0(header.1, "|");
  header.2 <- paste0(header.2, "|");

  text <- lapply(seq_along(instances),
                 function(i) {
    instance <- instances[[i]];
    stopifnot(is.character(instance),
              length(instance) == 1L,
              !is.na(instance),
              nchar(instance) > 0L);

    data <- end.result.stats[end.result.stats$instance == instance, ];
    stopifnot(nrow(data) > 0L);
    data <- lapply(algorithms,
      function(algorithm) {
        sel <- data[data$algorithm == algorithm, ];
        stopifnot(nrow(sel) == 1L);
        return(sel)
      });
    stopifnot(length(data) == length(data));
    data <- do.call(rbind, data);
    stopifnot(is.data.frame(data),
              nrow(data) == length(algorithms));

    stat.col.min <- vapply(seq_along(stat.cols), function(k) {
      if(mark.smallest.stat[[k]]) {
        return(min(unname(unlist(data[, k + 2L]))));
      } else {
        -Inf;
      }
    }, NA_real_);

    rows <- vapply(seq_along(algorithms), function(j) {
      stopifnot(identical(data$algorithm[[j]], algorithms[[j]]));
      if(print.algorithms) {
        row <- paste0(algorithm.format(algorithms.names[[j]]), "|");
      } else {
        row <- "";
      }

      return(paste0(row,
      paste(vapply(seq_along(stat.cols), function(k) {
        val <- data[j, k + 2L];
        stopifnot(is.numeric(val),
                  !is.na(val),
                  is.finite(val) || (val >= +Inf));
        ttt <- stat.cols.format[[k]](val);
        stopifnot(is.character(ttt));
        if(mark.smallest.stat[[k]]) {
          if(val <= stat.col.min[[k]]) {
            ttt <- paste0(smallest.stat.start, ttt, smallest.stat.end);
          }
        }
        return(ttt);
      }, NA_character_), sep="|", collapse="|")));
    }, NA_character_);

    stopifnot(length(rows) == length(algorithms));

    if(print.algorithms) {
      rows <- paste0(algorithms.names, "|", rows);
    }
    stopifnot(length(rows) == length(algorithms));

    if(print.instances.limit) {
      rows <- paste0("|", rows);
      rows[[1L]] <- paste0(instances.limit.format(instances.limit[[i]]),
                          rows[[1L]]);
    }
    stopifnot(length(rows) == length(algorithms));
    if(print.instances) {
      rows <- paste0("|", rows);
      rows[[1L]] <- paste0(instances.names[[i]],
                          rows[[1L]]);
    }
    stopifnot(length(rows) == length(algorithms));
    rows <- paste0("|", rows, "|");
    stopifnot(length(rows) == length(algorithms));
    return(rows);
  });

  stopifnot(length(text) == length(instances));

  text <- unname(unlist(c(header.1, header.2, unname(unlist(text)))));

  return(text);
}

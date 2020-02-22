
.legend.bg <- "#FFFFFFAA";
.legend.cex <- 1.05;

.default.lty <- 1L;
.default.lwd <- 1L;

.instance.limit.color <- "darkgray";
.instance.limit.lwd <- 4/3 * .default.lwd;
.instance.limit.lty <- 2L;
.instance.limit.cex <- .legend.cex;

.time.ms.text <- "time in ms";
.time.fes.text <- "time in FEs";
.quality.text <- "f";

.default.cex <- 0.8;
.default.mgp <- c(1, 0.43, 0);
.default.tck <- -0.02;

.default.mar.without.labels <- c(2.16, 1.6, 0.51, 0.15);

.gantt.default.instance.limit.adj <- c(-0.1, -0.1)
.gantt.default.job.name.cex <- 0.9;

.gantt.label.bg <- "#FFFFFFD5";
.gantt.label.cex <- .legend.cex;

.default.time.column <- "t";

.default.num.vec <- function(vec, default, len, min, max) {
  if(is.null(vec) || all(is.na(vec))) {
    vec <- default;
  } else {
    stopifnot(is.numeric(vec),
              length(vec) >= 0L,
              length(vec) <= len);
    vec[is.na(vec)] <- default[is.na(vec)];
    if(length(vec) < len) {
      m <- default;
      m[1L:length(vec)] <- vec;
      vec <- m;
    }
  }
  stopifnot(!is.null(vec),
            is.numeric(vec),
            length(vec) == len,
            all(is.finite(vec)),
            all(vec >= min),
            all(vec <= max));
  return(vec);
}

.default.num <- function(v, default, min, max) {
  if(is.null(v) || all(is.na(v))) {
    v <- default;
  }
  stopifnot(!is.null(v),
            is.numeric(v),
            length(v) == 1L,
            !is.na(v),
            is.finite(v),
            v >= min,
            v <= max);
  return(v);
}

.default.num.rep <- function(v, default, len, min, max) {
  stopifnot(len > 0L);
  if(is.null(v) || all(is.na(v))) {
    v <- default;
  }
  stopifnot(is.numeric(v),
            length(v) > 0L,
            length(v) <= len);
  v <- rep_len(v, len);
  if(any(is.na(v))) {
    default <- rep_len(default, len);
    v[is.na(v)] <- default[is.na(v)];
  }
  stopifnot(is.numeric(v),
            length(v) == len,
            all(is.finite(v)),
            all(v >= min),
            all(v <= max));
}

.default.char <- function(v, default) {
  if(is.null(v) || all(is.na(v))) {
    v <- default;
  }
  stopifnot(!is.null(v),
            is.character(v),
            length(v) == 1L,
            !is.na(v),
            nchar(v) > 0L);
  return(v);
}


.mgp <- function(mgp, default) .default.num.vec(mgp, default, 3L, -10, 10);
.tck <- function(tck, default) .default.num(tck, default, -2, 2);
.cex <- function(cex, default) .default.num(cex, default, 1e-2, 10);
.mar <- function(mar, default) .default.num.vec(mar, default, 4L, -10, 10);
.adj <- function(adj, default) .default.num.vec(adj, default, 2L, -10, 10);

.lwd <- function(lwd, default) .default.num(lwd, default, 0, 10);
.lwd.rep <- function(lwd, default, len) .default.num.rep(lwd, default, len, 0, 10);
.color <- .default.char;

.lty <- function(lty, default) {
  if(is.null(lty) || is.na(lty)) {
    lty <- default;
  }
  stopifnot(!is.null(lty),
            is.vector(lty),
            is.integer(lty) || is.character(lty),
            length(lty) == 1L);
  if(is.integer(lty)) {
    stopifnot(!is.na(lty),
              is.finite(lty),
              lty >= 0L,
              lty <= 6L);
  } else {
    stopifnot(!is.na(lty),
              nchar(lty) > 0L);
  }
  return(lty);
}

.lty.rep <- function(lty, default, len) {
  stopifnot(len > 0L);
  if(is.null(lty) || is.na(lty)) {
    lty <- default;
  }
  stopifnot(!is.null(lty),
            is.list(lty) || (is.vector(lty) &&
            (is.integer(lty) || is.character(lty))),
            length(lty) > 0L,
            length(lty) <= len);
  lty <- rep_len(lty, len);
  stopifnot(!is.null(lty),
            is.list(lty) || (is.vector(lty) &&
                               (is.integer(lty) || is.character(lty))),
            length(lty) > 0L,
            length(lty) <= len);
  if(any(is.na(lty))) {
    default <- rep_len(default, len);
    for(i in seq_along(lty)) {
      lty[[i]] <- .lty(lty[[i]], default[[i]]);
    }
  }
  return(lty);
}

.time.column <- function(time.column) {
  if(is.null(time.column) || all(is.na(time.column))) {
    time.column <- .default.time.column;
  }
  stopifnot(!is.null(time.column),
            is.character(time.column),
            length(time.column) == 1L,
            !is.na(time.column),
            time.column %in% c("t", "fes"));
  return(time.column);
}


# internal utility functions

# internally repeat gsub until all occurences of a pattern have been removed
.internal.gsub <- function(expr, repl, text, fixed=FALSE) {
  l2 <- nchar(text);
  stopifnot(l2 > 0L);
  repeat {
    text <- gsub(expr, repl, text, fixed=fixed);
    l1 <- l2;
    l2 <- nchar(text);
    stopifnot(l2 > 0L);
    if(l1 == l2) { break; }
  }
  return(text);
}

# create a logger string from a set of strings
.logger <- function(...) {
  cat(as.character(Sys.time()), ": ", paste0(..., sep="", collapse=""), "\n", sep="", collapse="");
  invisible(TRUE);
}

#' @importFrom graphics par
.safe.par <- function(...) {
  args <- list(...);
  args$cin <- NULL;
  args$cra <- NULL;
  args$csi <- NULL;
  args$cxy <- NULL;
  args$din <- NULL;
  args$page <- NULL;
  args <- suppressMessages(suppressWarnings(do.call(par, args)));
  args$cin <- NULL;
  args$cra <- NULL;
  args$csi <- NULL;
  args$cxy <- NULL;
  args$din <- NULL;
  args$page <- NULL;
  return(args);
}


.dir.exists <- function(dir) {
  stopifnot(!is.null(dir),
            is.character(dir),
            length(dir) == 1L,
            !is.na(dir),
            nchar(dir) > 0L);
  dir <- normalizePath(dir, mustWork = TRUE);
  stopifnot(!is.null(dir),
            is.character(dir),
            length(dir) == 1L,
            !is.na(dir),
            nchar(dir) > 0L,
            dir.exists(dir));
  return(dir);
}


.dir.ensure <- function(dir) {
  stopifnot(!is.null(dir),
            is.character(dir),
            length(dir) == 1L,
            !is.na(dir),
            nchar(dir) > 0L);
  dir <- normalizePath(dir, mustWork = FALSE);
  stopifnot(is.character(dir),
            nchar(dir) > 0L);
  dir.create(dir, showWarnings=FALSE, recursive=TRUE);
  dir <- normalizePath(dir, mustWork = TRUE);
  stopifnot(!is.null(dir),
            is.character(dir),
            length(dir) == 1L,
            !is.na(dir),
            nchar(dir) > 0L,
            dir.exists(dir));
  return(dir);
}

.file.name <- function(name) {
  stopifnot(is.character(name),
            length(name) == 1L,
            !is.na(name),
            nchar(name) > 0L);
  name <- .internal.gsub(" ", "_", name, fixed=TRUE);
  name <- .internal.gsub(".", "_", name, fixed=TRUE);
  name <- .internal.gsub("%", "_", name, fixed=TRUE);
  name <- .internal.gsub("__", "_", name, fixed=TRUE);
  stopifnot(is.character(name),
            length(name) == 1L,
            !is.na(name),
            nchar(name) > 0L);
  return(name);
}

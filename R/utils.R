
# internal utility functions

# internally repeat gsub until all occurences of
# a pattern have been removed
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

# invoke graphics::par, but omit all items that can cause
# warnings
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

# canonicalize a directory path and throw an error if the
# directory does not exist
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

# canonicalize a file name and throw an error if the
# file does not exist
.file.exists <- function(file) {
  stopifnot(!is.null(file),
            is.character(file),
            length(file) == 1L,
            !is.na(file),
            nchar(file) > 0L);
  file <- normalizePath(file, mustWork = TRUE);
  stopifnot(!is.null(file),
            is.character(file),
            length(file) == 1L,
            !is.na(file),
            nchar(file) > 0L,
            file.exists(file),
            file.size(file) > 0L);
  return(file);
}

# canonicalize a directory path and make sure that
# the directory exist - create it if necessary
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

# create a sanitized file name
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

# split a named list into a data and a names vector
.split.names <- function(lst) {
  stopifnot(is.list(lst) || is.character(lst),
            length(lst) > 0L);
  names <- names(lst);
  lst <- unname(unlist(lst));
  stopifnot(is.character(lst),
            length(lst) > 0L,
            !any(is.na(lst)),
            all(nchar(lst) > 0L));
  if(is.null(names)) { names <- lst; }
  stopifnot(!is.null(names),
            is.character(names),
            length(names) == length(lst));
  s <- vapply(names, function(z)
                       is.null(z) ||
                       is.na(z) ||
                       (nchar(z) <= 0L), FALSE);
  if(any(s)) {
    names[s] <- lst[s];
  }
  stopifnot(is.character(names),
            length(names) == length(lst),
            !any(is.na(names)),
            all(nchar(names) > 0L));
  return(list(data=lst, names=names));
}

#' @title Parse a File Name
#' @description Accepts the file name of a log file, returns a list with its
#'   components: \code{(algorithm, instance, seed)}.
#' @param file the file name
#' @return a list with the following components: \code{(algorithm, instance,
#'   seed)}
#' @export aitoa.parse.file.name
aitoa.parse.file.name <- function(file) {
  old.options <- options(warn=2);

  stopifnot(is.character(file),
            !is.null(file),
            length(file) == 1L,
            nchar(file) > 6L,
            !is.na(file));

  file <- normalizePath(file, mustWork = FALSE);
  stopifnot(is.character(file),
            !is.null(file),
            length(file) == 1L,
            nchar(file) > 10L,
            !is.na(file),
            endsWith(file, ".txt"));

  dir <- dirname(file);
  stopifnot(is.character(dir),
            !is.na(dir),
            !is.null(dir),
            !identical(dir, "."));

  instance <- trimws(basename(dir));
  stopifnot(is.character(instance),
            !is.null(instance),
            length(instance) == 1L,
            nchar(instance) > 0L,
            !is.na(instance));

  dir <- dirname(dir);
  stopifnot(is.character(dir),
            !is.na(dir),
            !is.null(dir),
            !identical(dir, "."));

  algorithm <- trimws(basename(dir));
  stopifnot(is.character(algorithm),
            !is.null(algorithm),
            length(algorithm) == 1L,
            nchar(algorithm) > 0L,
            !is.na(algorithm));

  name <- basename(file);
  stopifnot(is.character(name),
            length(name) == 1L,
            nchar(name) > 10L,
            !is.na(name),
            endsWith(name, ".txt"));
  name <- substr(name, 1L, nchar(name) - 4L);
  stopifnot(is.character(name),
            length(name) == 1L,
            nchar(name) > 6L,
            !is.na(name));

  stopifnot(startsWith(name,
                       paste0(algorithm,
                              "_",
                              instance,
                              "_0x")));

  parts = strsplit(name, "_", fixed=TRUE);

  stopifnot(length(parts) == 1L);
  parts <- parts[[1L]];
  stopifnot(is.character(parts),
            !any(is.na(parts)),
            length(parts) >= 3L,
            all(nchar(parts) > 0L));

  seed <- trimws(parts[[length(parts)]]);
  stopifnot(!is.na(seed),
            startsWith(seed, "0x"),
            nchar(seed) > 2L,
            nchar(seed) <= 18L,
            all(vapply(seq.int(3L, nchar(seed)),
                       function(i) substr(seed, i, i),
                       NA_character_) %in%
                  c("0", "1", "2", "3", "4", "5", "6", "7",
                    "8", "9", "a", "b", "c", "d", "e", "f")));

  options(old.options);

  res <- list(algorithm=algorithm,
              instance=instance,
              seed=seed);
  res <- force(res);

  return(res);
}

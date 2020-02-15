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
            length(file) == 1L,
            nchar(file) > 6L,
            !is.na(file));

  file <- basename(file);

  stopifnot(is.character(file),
            length(file) == 1L,
            nchar(file) > 10L,
            !is.na(file),
            endsWith(file, ".txt"));
  file <- substr(file, 1L, nchar(file) - 4L);
  stopifnot(is.character(file),
            length(file) == 1L,
            nchar(file) > 6L,
            !is.na(file));

  parts = strsplit(file, "_", fixed=TRUE);
  stopifnot(length(parts) == 1L);
  parts <- parts[[1L]];
  stopifnot(is.character(parts),
            !any(is.na(parts)),
            length(parts) >= 3L,
            all(nchar(parts) > 0L));
  parts <- trimws(parts);
  stopifnot(is.character(parts),
            !any(is.na(parts)),
            length(parts) >= 3L,
            all(nchar(parts) > 0L));

  seed <- parts[[length(parts)]];
  stopifnot(!is.na(seed),
            startsWith(seed, "0x"),
            nchar(seed) > 2L,
            nchar(seed) <= 18L,
            all(vapply(seq.int(3L, nchar(seed)),
                       function(i) substr(seed, i, i),
                       NA_character_) %in%
                  c("0", "1", "2", "3", "4", "5", "6", "7",
                    "8", "9", "a", "b", "c", "d", "e", "f")));

  inst <- parts[[length(parts) - 1L]];
  stopifnot(nchar(inst) > 0L,
            !is.na(inst));

  parts <- parts[1L:(length(parts)-2L)];
  stopifnot(length(parts) > 0L,
            !any(is.na(parts)),
            all(nchar(parts) > 0L));

  algo <- paste(parts,
                sep="_", collapse="_");
  stopifnot(nchar(algo) > 0L,
            !is.na(algo),
            nchar(algo) >= length(parts));

  options(old.options);

  res <- list(algorithm=algo,
              instance=inst,
              seed=seed);
  res <- force(res);

  return(res);
}

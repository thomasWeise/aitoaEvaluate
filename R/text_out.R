#' @title Create a Text File
#' @description  Create a text file.
#' @param directory the directory where the text will be stored
#' @param name the name chosen for the text
#' @param type the type of the graphic; supported are \code{txt} and \code{md}
#' @param trim.ws should whitespace be trimmed?
#' @param skip.if.exists should existing files be skipped?
#' @param body the expression to be executed for getting the text.
#' @return the path to the newly created text file
#' @include utils.R
#' @export aitoa.text
aitoa.text <- function(directory=".",
                       name,
                       type=c("md", "txt"),
                       trim.ws = TRUE,
                       skip.if.exists = TRUE,
                          body={ }) {

  name <- .file.name(name);
  directory <- .dir.ensure(directory);

  type <- match.arg(type);
  stopifnot(is.character(type),
            nchar(type) %in% c(2L, 3L));

  file <- normalizePath(file.path(directory,
                        paste0(name, ".", type)),
                        mustWork = FALSE);
  stopifnot(is.character(file),
            length(file) == 1L);
  if(file.exists(file) && (file.size(file) > 10L)) {
    if(skip.if.exists) {
      file <- normalizePath(file, mustWork = TRUE);
      return(file);
    }
    file.remove(file);
  }

  .logger("Now creating file '", file, "'.");

  text <- eval(body);
  stopifnot(!is.null(text),
            is.character(text),
            length(text) >= 1L,
            sum(nchar(text)) > 0L);
  if(trim.ws) {
    text <- trimws(text);
  }

  writeLines(text=text, con=file);
  size <- sum(nchar(text));
  stopifnot(length(text) > 0L,
            size > 0L);

  stopifnot(file.exists(file),
            file.size(file) >= (size + length(text)));

  .logger("Finished creating file '", file, "'.");
  return(file);
}

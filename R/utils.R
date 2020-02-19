
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
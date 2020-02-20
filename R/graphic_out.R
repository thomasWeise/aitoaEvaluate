.goldenRatio <- ((1 + sqrt(5)) / 2)

#' @include utils.R
.post.process.svg <- function(file) {
  stopifnot(file.exists(file));
  text <- readLines(con=file);
  stopifnot(length(text) > 0L);
  text <- paste(trimws(text), sep="", collapse="");
  text <- .internal.gsub('g clip-path=\\".*?\\" clip-rule=\\"nonzero\\"', "g", text);
  text <- .internal.gsub('<clipPath.*?</clipPath>', "", text);
  text <- .internal.gsub('</g>\\s*<g>', "", text);
  writeLines(text, con=file);
  return(file);
}

#' @title Create a Graphic File
#' @description  Create a graphic in the given format and write it into the
#'   given path. First, the proper graphics driver is selected and the output
#'   file is allocated. Then, the \code{body} expression is executed. This
#'   expression is expected to draw the graphic's contents. Finally, the
#'   graphics file is closed and post-processed. The path to the new graphic is
#'   returned.
#' @param directory the directory where the graphic will be stored
#' @param name the name chosen for the graphic
#' @param type the type of the graphic; supported are \code{svg}, \code{pdf},
#'   \code{eps}, and \code{png}
#' @param width the width of the graphic; set to \code{NA} for default (based on
#'   \code{height}, if available)
#' @param height the height of the graphic; set to \code{NA} for default (based
#'   on \code{width}, if available)
#' @param font.size the default font size
#' @param dpi the dots per inch to use if pixel-based rendering is required
#' @param body the expression to be executed for painting the graphic.
#' @return the path to the newly created graphic
#' @importFrom grDevices cairo_pdf cairo_ps dev.off png svg
#' @include utils.R
#' @export aitoa.graphic
aitoa.graphic <- function(directory=".",
                          name,
                          type=c("svg", "pdf", "eps", "png"),
                          width=7L,
                          height=width/.goldenRatio,
                          font.size=10L,
                          dpi=900L,
                          body={ }) {

  stopifnot(is.character(name),
            length(name) == 1L,
            nchar(name) > 0L);

  name <- .internal.gsub(" ", "_", name, fixed=TRUE);
  name <- .internal.gsub(".", "_", name, fixed=TRUE);
  name <- .internal.gsub("%", "_", name, fixed=TRUE);
  name <- .internal.gsub("__", "_", name, fixed=TRUE);

  if(is.na(width)) {
    if(is.na(height)) {
      width <- 7L;
    } else {
      width <- height * .goldenRatio;
    }
  }
  stopifnot(is.numeric(width),
            length(width) == 1L,
            is.finite(width),
            width >= 0.5,
            width <= 100);

  if(is.na(height)) {
    height <- width / .goldenRatio;
  }
  stopifnot(is.numeric(height),
            length(height) == 1L,
            is.finite(height),
            height >= 0.5,
            height <= 100,
            width/height <= 20,
            height/width <= 20);

  if(is.na(font.size)) {
    font.size <- 10L;
  }
  stopifnot(is.numeric(font.size),
            length(font.size) == 1L,
            is.finite(font.size),
            font.size > 2L,
            font.size < 100L);

  if(is.na(dpi)) {
    dpi <- 900L;
  }
  stopifnot(is.integer(dpi),
            length(dpi) == 1L,
            is.finite(dpi),
            dpi > 10L,
            dpi < 100000L);

  stopifnot(is.character(directory),
            length(directory) == 1L,
            nchar(directory) > 0L);
  directory <- normalizePath(directory, mustWork = FALSE);
  stopifnot(is.character(directory),
            nchar(directory) > 0L);
  dir.create(directory, showWarnings=FALSE, recursive = FALSE);
  directory <- normalizePath(directory, mustWork = TRUE);
  stopifnot(is.character(directory),
            nchar(directory) > 0L);

  type <- match.arg(type);
  stopifnot(is.character(type),
            nchar(type) == 3L);

  file <- normalizePath(file.path(directory,
                                  paste0(name, ".", type)),
                        mustWork = FALSE);

  .logger("Now creating file '", file, "'.");

  if(identical(type, "svg")) {
    svg(filename = file,
        width = width,
        height = height,
        pointsize = font.size,
        onefile = TRUE,
        antialias = "subpixel");
  } else {
    if(identical(type, "eps")) {
      cairo_ps(filename = file,
               width = width,
               height = height,
               pointsize = font.size,
               onefile = TRUE,
               antialias = "subpixel",
               fallback_resolution = dpi)
    }  else {
      if(identical(type, "pdf")) {
        cairo_pdf(filename = file,
                  width = width,
                  height = height,
                  pointsize = font.size,
                  onefile = TRUE,
                  antialias = "subpixel",
                  fallback_resolution = dpi);
      } else {
        if(identical(type, "png")) {;
          png(filename = file,
              width = width,
              height = height,
              units = "in",
              pointsize = font.size,
              res = dpi,
              type = "cairo-png",
              antialias = "subpixel");
        } else {
          stop(paste0("illegal graphics extension: ", type));
        }
      }
    }
  }

  old.par <- .safe.par(ljoin=0L, lend=0);
  eval(body);
  .safe.par(old.par);
  dev.off();

  if(type == "svg") {
    .post.process.svg(file);
  }

  stopifnot(file.exists(file),
            file.size(file) > 10L);

  .logger("Finished creating file '", file, "'.");
  return(file);
}

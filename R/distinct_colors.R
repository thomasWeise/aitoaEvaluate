.base.colors <- c("#FF0000",
                  "#0000FF",
                  "#00FF00",
                  "#A52A2A",
                  "#FFA500",
                  "#008833",
                  "#7788FF",
                  "#CF00FF",
                  "#770066",
                  "#CCCC00");

.color.cache <- new.env();

#' @title Obtain a Set of \code{n} Distinct Colors
#' @description Returns a set of \code{n} differently-looking colors.
#' @param n the number of colors to get
#' @return a character vector of length \code{n} with these colors
#' @include rgb_to_gray.R
#' @include scale_rgb_luminoscity.R
#' @include rgb_mix.R
#' @export aitoa.distinct.colors
#' @importFrom grDevices col2rgb rgb rgb2hsv
aitoa.distinct.colors <- function(n) {
  stopifnot(is.integer(n),
            length(n) == 1L,
            is.finite(n),
            n > 0L);

  if(n <= length(.base.colors)) {
    return(.base.colors[1L:n]);
  }

  n.name <- as.character(n);
  colors <- get0(n.name,
                 .color.cache,
                 inherits = FALSE,
                 ifnotfound = NULL);
  if(!is.null(colors)) {
    stopifnot(is.character(colors),
              length(colors) == n);
    return(colors);
  }

  # OK, we need first need to generate colors
  colors.start <- apply(t(col2rgb(.base.colors)),
                        2L, as.integer);
  colors.all <- colors.start;
  index <- 1L;
  while(nrow(colors.all) < (2.2*n)) {
    factor <- abs(0.8*sin(index))+0.1;

    adder <- unique(do.call(rbind,
               lapply(seq_len(nrow(colors.start)),
                 function(i) {
                   aitoa.rgb.mix(colors.start[i, ],
                                 colors.start[ 1L+((i+index) %% nrow(colors.start))],
                                 weight=factor,
                                 limit=255L,
                                 make.int = TRUE)
             })));
    start <- unique(rbind(colors.start, adder));

    adder <- unique(rbind(adder,
                          apply(start, 2L,
                                aitoa.scale.rgb.luminosity,
                                scale=factor,
                                limit=255L,
                                make.int=TRUE),
                          apply(start, 2L,
                                aitoa.scale.rgb.luminosity,
                                scale=-factor,
                                limit=255L,
                                make.int=TRUE)));
    means <- vapply(seq_len(nrow(adder)),
                    function(r) mean(as.integer(adder[r, ])),
                    NA_real_);
    keep <- (means >= 30L) & (means <= 225L);
    if(any(keep)) {
      colors.all <- unique(rbind(colors.all,
                                 adder[keep, ]));
    }

    index <- index + 1L;
  }

  # now we prune the most similar colors
  points <- lapply(seq_len(nrow(colors.all)),
                   function(r) {
                     row <- as.integer(unname(unlist(colors.all[r, ])));
                     as.integer(unname(unlist(c(row,
                                  aitoa.rgb2gray(row, make.int=TRUE),
                                  255L*rgb2hsv(row[[1L]],
                                      row[[2L]], row[[3L]],
                                      maxColorValue = 255L)))));
                   });

  while(nrow(colors.all) > n) {
    stopifnot(nrow(colors.all) == length(points));
    avg.dist <- vapply(points,
                       function(r1) {
                         sum(vapply(points,
                             function(r2) sum( (r1-r2)^2 ),
                             NA_real_))
                       }, NA_real_);
    stopifnot(all(is.finite(avg.dist)),
              all(avg.dist > 0));

    avg.dist[1L:nrow(colors.start)] <- +Inf;

    choice <- which(avg.dist <= min(avg.dist));
    stopifnot(length(choice) >= 1L);
    if(length(choice) > 1L) {
      choice <- max(choice);
    }
    stopifnot(length(choice) == 1L);

    points <- points[-choice];
    colors.all <- colors.all[-choice, ];
  }

  stopifnot(nrow(colors.all) == n);

  colors <- unname(unlist(vapply(seq_len(nrow(colors.all)),
                   function(r) {
                     row <- colors.all[r, ];
                     rgb(row[[1L]], row[[2L]], row[[3L]],
                         maxColorValue = 255L)
                   }, NA_character_)));
  stopifnot(is.character(colors),
            !any(is.na(colors)),
            !any(is.null(colors)),
            length(colors) == n,
            length(unique(colors)) == n,
            all(nchar(colors) > 0L));

  assign(n.name, colors, .color.cache);

  return(colors);
}

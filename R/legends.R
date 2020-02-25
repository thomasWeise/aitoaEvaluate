#' @title A Wrapper to Create Axis Labels as Legends
#' @description A shorthand with defaults for axis labels drawn as legends.
#' @param x the legend position
#' @param legend the legend text
#' @param cex the font size factor
#' @param bty the box type
#' @param bg the background
#' @param box.lwd the box line width
#' @param box.col the box color
#' @param seg.len the segment length
#' @param y.intersp the y interspacing
#' @param lwd the line width
#' @param pch the plot character
#' @param lty the line type
#' @param pt.lwd the point line width
#' @param pt.cex the point cex
#' @param inset the insets
#' @param ... other parameters to be passed to \link[graphics]{legend}
#' @include common_styles.R
#' @importFrom graphics legend
#' @export aitoa.legend.label
aitoa.legend.label <- function(x,
                               legend,
                               cex=.legend.cex,
                               bty="0",
                               bg=.legend.bg,
                               box.lwd=0L,
                               box.col=bg,
                               seg.len = -0.6,
                               y.intersp = 0,
                               lwd = 0,
                               pch = NA,
                               lty = NA,
                               pt.lwd = 0,
                               pt.cex = 0,
                               inset = 0.01,
                               ...) {
  params <- list(x=x,
                 legend=legend,
                 cex=cex,
                 bty=bty,
                 bg=bg,
                 box.lwd=box.lwd,
                 box.col=box.col,
                 seg.len=seg.len,
                 y.intersp=y.intersp,
                 lwd=lwd,
                 pch=pch,
                 lty=lty,
                 pt.lwd=pt.lwd,
                 pt.cex=pt.cex,
                 inset=inset,
                 ...);
  do.call(graphics::legend, params);
}

#' @title A Wrapper for the Main Legend
#' @description A shorthand with defaults for main legends.
#' @param x the legend position
#' @param legend the legend text
#' @param col the color
#' @param text.col the text color
#' @param cex the character resize factor
#' @param bg the back ground
#' @param bty the box type
#' @param box.lwd the box line width
#' @param box.col the box color
#' @param inset the insets
#' @param ... other parameters to be passed to \link[graphics]{legend}
#' @include common_styles.R
#' @importFrom graphics legend
#' @export aitoa.legend.main
aitoa.legend.main <- function(x,
                              legend,
                              col="black",
                              text.col=col,
                              cex=.legend.cex,
                              bg=.legend.bg,
                              bty="o",
                              box.lwd=0L,
                              box.col=bg,
                              inset=0.005,
                              ...) {
  params <- list(x=x,
                 legend=legend,
                 col=col,
                 text.col=text.col,
                 cex=cex,
                 bg=bg,
                 bty=bty,
                 box.lwd=box.lwd,
                 box.col=box.col,
                 inset=inset,
                 ...);
  do.call(graphics::legend, params);
}

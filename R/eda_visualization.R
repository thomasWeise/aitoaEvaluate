#' @title Visualize the Progress of a Real-Coded EDA
#' @export aitoa.real.coded.eda.visualization
#' @include function_to_matrix.R
#' @include filled_contour.R
#' @include draw_ellipse.R
#' @include make_color_transparent.R
#' @importFrom plot3D persp3D
#' @importFrom grDevices terrain.colors
aitoa.real.coded.eda.visualization <- function() {
  f <- function(x) aitoa.rastrigin(c(1.2, 0.9)*(x + c(1,-1)));
  c.min <- -7L;
  c.max <- 7L;
  n.points <- 101L;

  n.samples <- 101L;
  n.best.samples <- 41;
  col.select <- "blue";
  pch.select <- 1L;
  col.reject <- "red";
  pch.reject <- 2L;
  col.model <- "cyan";
  col.model.t <- aitoa.make.color.transparent(col.model, 0.2);

  par(mfrow=c(3L, 2L))
  par(mar=c(0.034, 0.034, 0.034, 0.034));

  data.3d <- aitoa.function.to.matrix(x.min=c.min,
                                      x.max=c.max,
                                      y.min=c.min,
                                      y.max=c.max,
                                      n.points.x=n.points,
                                      n.points.y=n.points,
                                      f=f);
  z.range <- range(c(data.3d$z));
  colors <- terrain.colors(n=101L);

  persp3D(x=data.3d$x,
          y=data.3d$y,
          z=data.3d$z,
          box=TRUE,
          axes=TRUE,
          ticktype="simple",
          xlim=c(c.min, c.max),
          ylim=c(c.min, c.max),
          zlim=z.range,
          colvar=data.3d$z,
          col=colors,
          clim=z.range,
          facets=TRUE,
          colkey=FALSE,
          bty="b2",
          contour=FALSE,
          cex=0.8,
          xaxt="n",
          yaxt="n",
          zaxt="n",
          xlab="x[1]",
          ylab="x[2]",
          zlab="f(x)");

  samples <- matrix(runif(n=(2L*n.samples),
                             min=c.min, max=c.max),
                    ncol=2L, nrow=n.samples);

  for(plt in 1L:5L) {
    par(mar=c(0.3, 0.3, 0.3, 0.3));

    .filled.contour3(data.3d$x, data.3d$y, data.3d$z,
                     col=colors,
                     nlevels=length(colors),
                     plot.axes=function() invisible());
    # contour(data.3d$x, data.3d$y, data.3d$z,
    #         nlevels=5L,
    #         drawlabels = FALSE,
    #         add=TRUE,
    #         lwd=0.5,
    #         col="black");

    contour(data.3d$x, data.3d$y, data.3d$z,
            nlevels=6L,
            drawlabels = FALSE,
            add=TRUE,
            lwd=0.3,
            col=aitoa.make.color.transparent("black", 0.3));

    if(plt > 1L) {

      f.f <- apply(samples, 1L, f);
      f.o <- order(f.f);
      samples <- samples[f.o, ];

      mean.x <- mean(samples[1L:n.best.samples, 1L]);
      mean.y <- mean(samples[1L:n.best.samples, 2L]);
      sd.x   <- sd(samples[1L:n.best.samples, 1L]);
      sd.y   <- sd(samples[1L:n.best.samples, 2L]);

      aitoa.draw.ellipse(x.center = mean.x,
                         y.center = mean.y,
                         x.radius = sd.x,
                         y.radius = sd.y,
                         border=col.model,
                         lwd=3L);
      lines(x=c(mean.x, mean.x),
           y=c(mean.y, mean.y+sd.y),
           col=col.model,
           lwd=3L);
      lines(x=c(mean.x, mean.x+sd.x),
           y=c(mean.y, mean.y),
           col=col.model,
           lwd=3L);
      points(x=mean.x,
             y=mean.y,
             col=col.model,
             lwd=3,
             pch=3);

      for(i in n.samples:1L) {
        points(x=samples[i, 1L],
               y=samples[i, 2L],
               col=if(i <= n.best.samples) col.select else col.reject,
               lwd=3,
               pch=if(i <= n.best.samples) pch.select else pch.reject);
      }


      aitoa.draw.ellipse(x.center = mean.x,
                         y.center = mean.y,
                         x.radius = sd.x,
                         y.radius = sd.y,
                         border=col.model.t,
                         lwd=3L);
      lines(x=c(mean.x, mean.x),
           y=c(mean.y, mean.y+sd.y),
           col=col.model.t,
           lwd=3L);
      lines(x=c(mean.x, mean.x+sd.x),
           y=c(mean.y, mean.y),
           col=col.model.t,
           lwd=3L);
      points(x=mean.x,
             y=mean.y,
             col=col.model.t,
             lwd=3,
             pch=3);

      samples <- as.matrix(
                   cbind(rnorm(n=n.samples, mean=mean.x, sd=sd.x),
                         rnorm(n=n.samples, mean=mean.y, sd=sd.y)));

      if(plt <= 2L) {
        aitoa.legend.label("bottomleft",
                           "sampled uniformly at random");
      } else {
        aitoa.legend.label("bottomleft",
                           paste0("sampled from N(\u03BC,\u03C3) of step ", (plt-2L)));
      }

      aitoa.legend.label("top", paste0("step ", (plt-1L)));
    } else {
      aitoa.legend.label(x="topright",
                         legend="contour of f(x): view from top down");

      aitoa.legend.main(x="bottomleft",
                        legend=c(paste0(n.samples, " points per iteration"),
                                 paste0(n.best.samples, " best are selected"),
                                 paste0((n.samples-n.best.samples), " worse are discarted"),
                                 "model: center \u03BC, stddev \u03C3"),
                        pch=c(NA, pch.select, pch.reject, NA),
                        col=c("black",
                              col.select,
                              col.reject,
                              col.model));
    }

    aitoa.legend.label("topleft", "x[2]");
    aitoa.legend.label("bottomright", "x[1]");
  }
}

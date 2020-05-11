#' @title Visualize the Progress of a Real-Coded EDA
#' @export aitoa.real.coded.eda.visualization
#' @include function_to_matrix.R
#' @include draw_ellipse.R
#' @include make_color_transparent.R
#' @importFrom plot3D persp3D
#' @importFrom grDevices terrain.colors
#' @importFrom graphics image lines points
aitoa.real.coded.eda.visualization <- function() {
  set.seed(10234402L);

  f <- function(x) aitoa.rastrigin(c(1.2, 0.9)*(x + c(-1, 1)));
  c.min <- -7L;
  c.max <- 7L;
  n.points <- 45L;

  n.samples <- 101L;
  n.best.samples <- 41;
  col.select <- "green";
  pch.select <- 1L;
  col.reject <- "red";
  pch.reject <- 2L;
  col.model <- "blue";
  col.model.t <- aitoa.make.color.transparent(col.model, 0.2);
  legend.bg <- "#FFFFFFD0";

  # par(mfrow=c(3L, 2L))
  layout(matrix(c(1, 2, 1, 3, 4, 5),
                nrow=3, ncol=2, byrow=TRUE));
  par(mar=c(0.034, 0.034, 0.034, 0.034));

  data.3d <- aitoa.function.to.matrix(x.min=c.min,
                                      x.max=c.max,
                                      y.min=c.min,
                                      y.max=c.max,
                                      n.points.x=n.points,
                                      n.points.y=n.points,
                                      f=f);
  z.range <- range(c(data.3d$z));
  colors <- terrain.colors(n=25L);
  #black.t <- aitoa.make.color.transparent("black", 0.6);

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
          cex=0.9,
          xaxt="n",
          yaxt="n",
          zaxt="n",
          xlab="x1",
          ylab="x2",
          zlab="f(x)");

  aitoa.legend.label(x="topleft",
                     legend=paste0("optimize f(x) over\n",
                              "two dimensions\n",
                              "x=(x1, x2)"),
                     bg=NA);

  samples <- matrix(runif(n=(2L*n.samples),
                             min=c.min, max=c.max),
                    ncol=2L, nrow=n.samples);

  for(plt in 1L:4L) {
    par(mar=c(0.3, 0.3, 0.3, 0.3));

    image(x=data.3d$x, y=data.3d$y,
          z=data.3d$z, col=colors,
          axes=FALSE, useRaster = TRUE);

    contour(data.3d$x, data.3d$y, data.3d$z,
            nlevels=7L,
            drawlabels = FALSE,
            add=TRUE,
            lwd=0.5,
            col="#333333");

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
                         lwd=3L,
                         resolution=71L);
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

      aitoa.legend.label("top", paste0("step ", (plt)));

      aitoa.legend.label(x="topright",
                         legend="contour of f(x)");

      if(plt > 1L) {
        aitoa.legend.main(x="bottomleft",
                          legend=c(paste0(n.samples, " samples from M", (plt-1L)),
                                   paste0(n.best.samples, " best selected"),
                                   paste0((n.samples-n.best.samples), " worse discarted"),
                                   paste0("model M", plt)),
                          pch=c(NA, pch.select, pch.reject, NA),
                          col=c("black",
                                col.select,
                                col.reject,
                                col.model),
                          bg=legend.bg);
      } else {
        aitoa.legend.main(x="bottomleft",
                          legend=c(paste0(n.samples, " points uniform"),
                                   paste0(n.best.samples, " best selected"),
                                   paste0((n.samples-n.best.samples), " worse discarted"),
                                   paste0("model M", plt)),
                          pch=c(NA, pch.select, pch.reject, NA),
                          col=c("black",
                                col.select,
                                col.reject,
                                col.model),
                          bg=legend.bg);
      }

    aitoa.legend.label("topleft", "x2");
    aitoa.legend.label("bottomright", "x1");
  }
}

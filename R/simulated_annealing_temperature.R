
#' @title Generate a Sequence of Tau values
#' @description Generate a sequence of time steps
#' @param max.tau the maximum tau
#' @return the sequence
#' @export aitoa.sa.generate.tau
aitoa.sa.generate.tau <- function(max.tau=30000000L) {
  stopifnot(is.integer(max.tau),
            length(max.tau) == 1L,
            max.tau > 0L);

  tau <- suppressWarnings(sort(unique(as.integer(
          unname(unlist(c(1L, max.tau,
           seq.int(from=1L, to=max.tau,
                   length.out=37L),
           max.tau * seq.int(from=0, to=1, length.out=37L)^2,
           max.tau * sqrt(seq.int(from=0, to=1, length.out=37L)),
           lapply(c(2L, 3L, 10L),
            function(base)
              lapply(c(1L, 2L, 3L, 5L, 7L),
                     function(div) (base^(1L:50L))/div
              )))))))));
  as.integer(sort(unique(tau[is.finite(tau) &
                             tau > 0L &
                             tau <= max.tau])));
}

.aitoa.sa.T.exp <- function(x, Ts, epsilon)
  Ts * ((1 - epsilon) ^ (x - 1L))

#' @title The Simulated Annealing Exponential Temperature Schedule
#' @description Compute the temperature at FE index tau
#' @param Ts the start temperature
#' @param epsilon the epsilon
#' @param tau the integer vector of FE indices
#' @return the temperatures
#' @export aitoa.sa.T.exp
aitoa.sa.T.exp <- function(Ts, epsilon, tau) {
  stopifnot(is.numeric(Ts),
            length(Ts) == 1L,
            is.finite(Ts),
            Ts >= 0,
            is.numeric(epsilon),
            length(epsilon) == 1L,
            is.finite(epsilon),
            epsilon >= 0,
            epsilon <= 1,
            is.integer(tau),
            length(tau) > 0L,
            all(is.finite(tau)),
            all(tau > 0L));
  r <- vapply(tau, .aitoa.sa.T.exp, NA_real_,
              Ts=Ts, epsilon=epsilon);
  stopifnot(is.numeric(r),
            length(r) == length(tau),
            all(is.finite(r)),
            all(r >= 0),
            all(r <= Ts));
  return(r);
}


.euler <- exp(1);
.aitoa.sa.T.log <- function(x, Ts, epsilon)
  Ts / (log(epsilon * (x - 1L) + .euler))

#' @title The Simulated Annealing Logarithmic Temperature Schedule
#' @description Compute the temperature at FE index tau
#' @param Ts the start temperature
#' @param epsilon the epsilon
#' @param tau the integer vector of FE indices
#' @return the temperatures
#' @export aitoa.sa.T.log
aitoa.sa.T.log <- function(Ts, epsilon=1, tau) {
  stopifnot(is.numeric(Ts),
            length(Ts) == 1L,
            is.finite(Ts),
            Ts >= 0,
            is.numeric(epsilon),
            length(epsilon) == 1L,
            is.finite(epsilon),
            epsilon >= 0,
            is.integer(tau),
            length(tau) > 0L,
            all(is.finite(tau)),
            all(tau > 0L));
  r <- vapply(tau, .aitoa.sa.T.log, NA_real_,
              Ts=Ts, epsilon=epsilon);
  stopifnot(is.numeric(r),
            length(r) == length(tau),
            all(is.finite(r)),
            all(r >= 0),
            all(r <= Ts));
  return(r);
}


.aitoa.sa.P <- function(T, deltaE) {
  if(deltaE <= 0) 1
  else if(T <= 0) 0
  else exp(-deltaE / T)
}

#' @title Compute the Simulated Annealing Acceptance Probability
#' @description Compute the Acceptance Probability for a given deltaE and
#'   temperature T.
#' @param deltaE the single difference in objective value
#' @param T the vector of temperatures
#' @return the acceptance probabilities
#' @export aitoa.sa.P
aitoa.sa.P <- function(deltaE, T) {
  stopifnot(is.numeric(deltaE),
            length(deltaE) == 1L,
            is.finite(deltaE),
            is.numeric(T),
            length(T) > 0L,
            all(is.finite(T)),
            all(T >= 0));
  r <- vapply(T, .aitoa.sa.P, NA_real_,
              deltaE=deltaE);
  stopifnot(is.numeric(r),
            length(r) == length(T),
            all(is.finite(r)),
            all(r >= 0),
            all(r <= 1));
  return(r);
}

#' @title Create the Temperature/Probability Plot for Simulated Annealing
#' @description This is not really a function for evaluating stuff, but it
#'   creates a nice plot that I use in the book, so I want to keep it.
#' @export aitoa.sa.temperature.plot
#' @include distinct_colors.R
aitoa.sa.temperature.plot <- function() {

  x.lim   <- as.integer(c(1L, 3e7));
  tau <- aitoa.sa.generate.tau(x.lim[[2L]]);

  schedules <- c("exp_20_0.00000005",
                 "exp_20_0.0000001",
                 "exp_20_0.00000015",
                 "exp_20_0.0000002",
                 "exp_20_0.0000004",
                 "exp_20_0.0000008");
  epsilon <- c(5e-8, 1e-7, 1.5e-7,
               2e-7, 4e-7, 8e-7);
  deltaE <- c(1L, 2L, 3L, 5L, 10L, 50L);

  frame <- unlist(list(list(tau=tau),
              lapply(seq_along(schedules),
                     function(i) {
                       name <- schedules[[i]];
                       eps <- epsilon[[i]];
                       T <- aitoa.sa.T.exp(Ts=20L, epsilon=eps, tau=tau);
                       res <- list();
                       res[[name]] <- T;
                       for(d in deltaE) {
                         nn <- paste0("P", d, "_", name);
                         res[[nn]] <- aitoa.sa.P(d, T);
                       }
                       return(res);
                     })),
              recursive = FALSE);

  frame <- do.call(data.frame, frame);
  stopifnot(nrow(frame) > 1L);

  x.ticks <- as.integer(10L^(0L:7L));

  x.tick.ch <- as.expression(lapply(x.ticks,
                                    function(tick) {
                                      if(tick<=10L) { return(tick); }
                                      t <- as.integer(floor(log10(tick)));
                                      return(substitute(10^t, list(t=t)))
                                    }));

  x.grid <- x.ticks;

  x.ticks2 <- sort(unique(as.integer(x.lim[2L] * 0.2 * 0L:5L)));
  x.ticks2[x.ticks2 < 1] <- 1;
  x.ticks2 <- sort(unique(x.ticks2))
  x.tick2.ch <- as.expression(lapply(x.ticks2,
                                     function(tick) {
                                       if(tick<=10L) { return(tick); }
                                       t <- as.integer(floor(log10(tick)));
                                       if(as.integer(tick / (10L^t)) != (tick / (10L^t))) {
                                         t <- t - 1L;
                                       }
                                       v <- as.integer(tick / as.integer(10L^t));
                                       stopifnot( (v*10^t)== tick);
                                       if(v == 1L) {
                                         return(substitute(10^t, list(t=t)))
                                       }
                                       return(substitute(paste(v,"*",10^t), list(v=v, t=t)))
                                     }));
  x.grid2 <- x.ticks2;

  t.lim   <- as.integer(c(0L, 20L));
  t.ticks <- as.integer(4L * (0L:5L));
  t.grid  <- as.integer(2L*(0L:10L));

  p.lim   <- as.integer(c(0L, 1L));
  p.ticks <- 0.2 * as.integer(0L:5L);
  p.grid  <- 0.1 * as.integer(0L:10L);

  stopifnot(sum(colnames(frame) %in% schedules) == length(schedules));

  colors <- aitoa.distinct.colors(length(schedules));

  names <- as.expression(c(
    expression(paste("exponential: ", T(tau), "=", 20 * {(1 - paste(5, "*", 10^-8)) ^ {tau - 1}})),
    expression(paste("exponential: ", T(tau), "=", 20 * {(1 - paste(1, "*", 10^-7)) ^ {tau - 1}})),
    expression(paste("exponential: ", T(tau), "=", 20 * {(1 - paste(1.5, "*", 10^-7)) ^ {tau - 1}})),
    expression(paste("exponential: ", T(tau), "=", 20 * {(1 - paste(2, "*", 10^-7)) ^ {tau - 1}})),
    expression(paste("exponential: ", T(tau), "=", 20 * {(1 - paste(4, "*", 10^-7)) ^ {tau - 1}})),
    expression(paste("exponential: ", T(tau), "=", 20 * {(1 - paste(8, "*", 10^-7)) ^ {tau - 1}}))
  ));

  par(mfrow=c(4L, 1L))

  par(mar=c(1.8, 3.0, 0.034, 0.034));

  plot(x=x.lim,
       y=t.lim,
       xlim=x.lim,
       ylim=t.lim,
       type="n",
       log="x",
       xlab = NA,
       ylab = NA,
       xaxt="n",
       yaxt="n");

  abline(v = x.grid, h = t.grid, col = "lightgray", lty = 1, lwd=1);

  axis(side=1, at=x.ticks, labels=x.tick.ch);
  axis(side=2, at=t.ticks, labels=as.character(t.ticks), las=1L);

  for(i in seq_along(schedules)) {
    lines(x=unname(unlist(frame$tau)),
          y=unname(unlist(frame[schedules[i]])),
          col=colors[[i]],
          lwd=2L)
  }

  mtext(text=expression(tau),    side=1, at=x.lim[2L],     adj=1, line=1.0, cex=0.9);
  mtext(text=expression(T(tau)), side=2, at=0.9*t.lim[2L], adj=1, line=0.5, cex=0.9, las=1L);

  aitoa.legend.main(x="bottomleft",
                    legend=names,
                    col=colors,
                    lwd=2L,
                    seg.len=3L);

  for(P in c(1L, 3L, 10L)) {
    par(mar=c(if(P>=10) 2.25 else 1.8,
              3.0,
              if(P==1) 2.5 else 0.5,
              0.034));

    plot(x=x.lim,
         y=p.lim,
         xlim=x.lim,
         ylim=p.lim,
         type="n",
         #      log="x",
         xlab = NA,
         ylab = NA,
         xaxt="n",
         yaxt="n");

    abline(v = x.grid2, h = p.grid, col = "lightgray", lty = 1, lwd=1);

    axis(side=1, at=x.ticks2, labels=x.tick2.ch);
    axis(side=2, at=p.ticks, labels=as.character(p.ticks), las=1L);

    for(i in seq_along(schedules)) {
      lines(x=unname(unlist(frame$tau)),
            y=unname(unlist(frame[paste("P", P, "_", schedules[i], sep="", collapse="")])),
            col=colors[[i]],
            lwd=2L)
    }

    mtext(text=expression(tau),    side=1, at=x.lim[2L]*0.9,     adj=1, line=1.0, cex=0.9);

    if(P == 1L) {
      mtext(text=as.expression(substitute(paste(P(paste("accept ", Delta, E, "=", y)), "=", e^{-y/T(tau)}), list(y=P))), side=2,
            at=1.12*p.lim[2L], adj=0,
            line=-1, cex=0.9, las=1);
    } else {
      mtext(text=as.expression(substitute(paste(P(paste("accept ", Delta, E, "=", y)), "=", e^{-y/T(tau)}), list(y=P))), side=2, at=0.95*p.lim[2L], adj=0, line=-1, cex=0.9, las=1);
    }
  }
}

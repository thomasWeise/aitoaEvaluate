set.seed(1235513L);
n <- 5L;
c.min <- 0L;
c.max <- 35L;

d <- function(x1, y1, x2, y2) {
  sqrt(((x1 - x2)^2) + (y1 - y2)^2);
}



f0 <- function(j, i, zxy) {
  x1 <- zxy[[i + i - 1L]];
  y1 <- zxy[[i + i]];
  x2 <- zxy[[j + j - 1L]];
  y2 <- zxy[[j + j]];
  r <- d(x1, y1, x2, y2);
  if(r <= 0) {
    return(1e10);
  }
  return  (((r - round(r)) ^ 2) +
           ( (c.max/(1 + abs(x2-x1)))) +
             ( (c.max/(1 + abs(y2-y1)))) );
}

f1 <- function(i, zxy) {
  return(sum(vapply(seq.int(from=(i+1L), to=n),
                    f0, NA_real_, i=i, zxy=zxy)));
}

f2 <- function(zxy) {
  rzxy <- round(zxy);
  return(sum(vapply(seq.int(from=1L, to=(n-1L)),
                    f1, NA_real_, zxy=rzxy)) +
         sum((rzxy - zxy) ^ 2));
}

library(DEoptim);
res <- DEoptim::DEoptim(fn=f2,
                        lower=rep.int(c.min, n+n),
                        upper=rep.int(c.max, n+n),
                        DEoptim.control(NP=1000L,
                                        trace=FALSE));
res <- round(unname(unlist(res$optim$bestmem)));
cat(paste0("DE optimum: ", paste(res, collapse=","), " - ", f2(res), "\n"));

library(cmaes);
res <- cmaes::cma_es(par=res, fn = f2, lower=c.min, upper=c.max);
res <- round(unname(unlist(res$par)));
cat(paste0("CMA-ES optimum: ", paste(res, collapse=","), " - ", f2(res), "\n"));

for(i in seq_len(n)) {
  cat(paste0(i, ": (", res[[i+i-1L]], ",", res[[i+i]], "):\n"));
  if(i < n) {
    for(j in seq.int(from=i+1L, to=n)) {
      cat(paste0("  ", i, "-", j, ": ", round(100*
         d(res[[i+i-1L]], res[[i+i]],
           res[[j+j-1L]], res[[j+j]])), "\n"));
    }
  }
}

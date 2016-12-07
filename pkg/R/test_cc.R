# tests of canonical dimensions
# from http://www.ats.ucla.edu/stat/r/dae/canonical.htm, with slight modifications
test_cc <- function(cc){
  ev <- (1 - cc$cor^2)
  
  n <- length(cc$names$ind.names)
  p <- length(cc$names$Xnames)
  q <- length(cc$names$Ynames)
  k <- min(p, q)
  m <- n - 3/2 - (p + q)/2
  
  w <- rev(cumprod(rev(ev)))
  
  # initialize
  d1 <- d2 <- f <- vector("numeric", k)
  
  for (i in 1:k) {
    s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si <- 1/s
    d1[i] <- p * q
    d2[i] <- m * s - p * q/2 + 1
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
  }
  
  pv <- pf(f, d1, d2, lower.tail = FALSE)
  dmat <- data.frame(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
  return(dmat)
}

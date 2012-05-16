
x <- rnorm(10, mean=50, sd=10)
y <- rnorm(10, mean=50, sd=10)

t.statistic <- function(x, y) {
  m <- length(x)
  n <- length(y)
  s.pooled <-sqrt( ((m - 1) * sd(x)^2 + (n - 1) * sd(y)^2) / (m + n - 2) )
  t.stat <- (mean(x) - mean(y)) / (s.pooled * sqrt( 1/m + 1/n))
  return(t.stat)
}

data.x <- c(1, 4, 3, 6, 5)
data.y <- c(5, 4, 7, 6, 10)
t.statistic(data.x, data.y)

t.simulate <- function(alpha, m, n, N) {
  n.reject <- 0
  for (i in 1:N) {
    x <- rnorm(m, mean=0, sd=1)
    y <- rnorm(n, mean=0, sd=1)
    t.stat <- t.statistic(x, y)
    # the qt() call returns the critical t
    if (abs(t.stat) > qt(1 - alpha/2, m + n -2)) 
      n.reject <- n.reject + 1
  }
  true.sig <- n.reject/N
  return(true.sig)
}

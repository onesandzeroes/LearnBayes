alpha <- .1
m <- 10
n <- 10
N <- 10000

n.reject <- 0
for (i in 1:N) {
  x <- rnorm(m, mean=10, sd=2)
  y <- rexp(n, rate=1/10)
  t.stat <- t.statistic(x, y)
  # the qt() call returns the critical t
  if (abs(t.stat) > qt(1 - alpha/2, m + n -2)) 
    n.reject <- n.reject + 1
}
true.sig.normexp <- n.reject/N

## Results
# > true.sig.allmet
# [1] 0.1073
# > true.sig.differentvariance
# [1] 0.115
# > true.sig.exponentials
# [1] 0.0883
# > true.sig.normexp
# [1] 0.156
# > true.sig.tdistributions
# [1] 0.0963

my.tsim <- function() t.statistic(rnorm(m, mean=10, sd=2), rexp(n, rate=1/10))
tstat.vector <- replicate(10000, my.tsim())

tstat.df <- data.frame(tstat=tstat.vector)
# Plot showing the simulated t-values (for a normal population in one sample and an 
# exponential in the other) in red and the theoretical/t-distribution values in blue
ggplot(tstat.df, aes(tstat)) + 
  geom_density(colour="red") + 
  stat_function(fun = dt, arg=list(df=18), colour="blue") + 
  geom_vline(xintercept=qt(0.975, 18), colour="blue", linetype="dotted")
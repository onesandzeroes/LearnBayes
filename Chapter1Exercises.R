# 1. Movies owned by students
ggplot(studentdata, aes(Dvds)) + geom_histogram()
summary(studentdata$Dvds)
table(studentdata$Dvds)
barplot(table(studentdata$Dvds))

# 2. Student heights
ggplot(studentdata, aes(Gender, Height)) + geom_boxplot()
height.output <- boxplot(studentdata$Height ~ studentdata$Gender)
rm(height.output)
mean(studentdata$Height[studentdata$Gender=="male"], na.rm=TRUE) - 
  mean(studentdata$Height[studentdata$Gender=="female"], na.rm=TRUE)

# 3. Sleeping times
ggplot(studentdata, aes(ToSleep, WakeUp)) + geom_point()
sleepwake <- lm(WakeUp ~ ToSleep, data=studentdata)
rm(sleepwake)
ggplot(studentdata, aes(ToSleep, WakeUp)) + geom_point() + geom_smooth(method="lm")
# lm predicts that someone going to sllep at midnight wakes up at 7.96 AM (i.e. intercept
# of the lm model)

# 4. Performance of the traditional confidence interval of a proportion
binomial.conf.interval <- function(y, n, alpha) {
  z <- qnorm(1 - (alpha / 2))
  # Estimated probality of success p
  p.hat <- y/n
  se <- sqrt(p.hat * (1 - p.hat) / n)
  return(c(p.hat - z*se, p.hat + z*se))
}

simulate.binom <- function(true.p, n, sim.runs, alpha=0.1) {
  p.in.conf <- 0
  for (i in 1:sim.runs) {
    y <- sum(rbinom(n, 1, true.p))
    conf <- binomial.conf.interval(y, n, alpha)
    if (conf[1] < true.p & true.p < conf[2]) p.in.conf <- p.in.conf + 1
  }
  return (p.in.conf / sim.runs)
}

# 5. Monte Carlo study of CI for a propotion
# p = .05
simulate.binom(true.p=0.05, n=10, sim.runs=1000, alpha=0.1)
# 0.372
simulate.binom(true.p=0.05, n=25, sim.runs=1000, alpha=0.1)
# 0.702
simulate.binom(true.p=0.05, n=100, sim.runs=1000, alpha=0.1)
# 0.862

# p = .25
simulate.binom(true.p=0.25, n=10, sim.runs=1000, alpha=0.1)
# 0.923
simulate.binom(true.p=0.25, n=25, sim.runs=1000, alpha=0.1)
# 0.865
simulate.binom(true.p=0.25, n=100, sim.runs=1000, alpha=0.1)
# 0.904

# p = .5
simulate.binom(true.p=0.5, n=10, sim.runs=1000, alpha=0.1)
# 0.878
simulate.binom(true.p=0.5, n=25, sim.runs=1000, alpha=0.1)
# 0.888
simulate.binom(true.p=0.5, n=100, sim.runs=1000, alpha=0.1)
# 0.908

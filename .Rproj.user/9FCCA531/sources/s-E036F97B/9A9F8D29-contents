set.seed(1729)
library("triangle")

rate.std = .1 # relative standard deviation
mttr.std = .05
cost.std = .05
prod.std = .1


# Initial case

event.mtbf = 100 # mean time between failure, days
event.mttr = 2 # mean time to repair, days
event.cost = 50000 # cost per event
rev.hourly = 75000 # $/hr revenue

detect = 0.65 # probability of detection
impact.cost = .5 # relative reduction in cost
impact.mttr = .2 # relative reduction in hours


library(ggplot2)
library(reshape2)

run.base <- function(){
  days <- 365.25*5 # 5 Year
  mtbf = rnorm(1, mean = event.mtbf, sd = event.rate*rate.std)
  duration = rnorm(1, mean = event.mttr, sd = event.mttr*mttr.std)
  cost = rnorm(1, mean = event.cost, sd = event.cost*cost.std)
  production = rnorm(1, rev.hourly, sd = rev.hourly*prod.std)
  
  events <- floor(days/mtbf)
  yearlycost <- (events*cost + events*duration*cost)/5
  return(yearlycost)
}

run.mtell <- function(){
  days <- 365.25*5 # 5 Year
  mtbf = rnorm(1, mean = event.mtbf, sd = event.rate*rate.std)
  duration = rnorm(1, mean = event.mttr, sd = event.mttr*mttr.std)
  cost = rnorm(1, mean = event.cost, sd = event.cost*cost.std)
  production = rnorm(1, rev.hourly, sd = rev.hourly*prod.std)
  
  catch = rbinom(1,1,detect)
  costreduce = rnorm(1, mean = impact.cost, sd = impact.cost*.1)
  mttrreduce = rnorm(1, mean = impact.mttr, sd = impact.mttr*.1)
  
  events <- floor(days/mtbf)
  yearlycost <- ((events*cost + events*duration*cost)-catch*(costreduce*events*cost + mttrreduce*events*duration*cost))/5
  return(yearlycost)
}

base.case <- replicate(runs,run.base())
mtell.case <- replicate(runs,run.mtell())
delta <- base.case-mtell.case
df <- data.frame(base.case,mtell.case,delta)
df <- melt(data = df, measure.vars = c("base.case","mtell.case"))

hist(base.case, col = 'red', xlim =c(0,max(base.case)))
hist(mtell.case, col = 'blue', add=T)

mu <- ddply(df, "variable", summarise, grp.mean=mean(value))
head(mu)


# Basic histogram
p <- ggplot(df, aes(x=value, color=variable)) + 
  geom_histogram(position="identity", alpha=0.5)
p
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=variable),
             linetype="dashed")

# CDF version
p <- ggplot(df, aes(x=value, color=variable)) + 
  stat_ecdf(position="identity", pad = FALSE)
p

p+geom_vline(data=mu, aes(xintercept=grp.mean, color=variable),
             linetype="dashed")

# empirical CDF to get the probability chart
q <- ggplot(df, aes(delta)) + stat_ecdf(geom = "step", pad = FALSE)
q






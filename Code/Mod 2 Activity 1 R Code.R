library(tidyverse)

## 1. Exponential growth equation 
NO <- 2
lambda <- 2
time <- 0:10

Nt <- NO * lambda**time

ggplot(data=NULL,aes(x=time, y=Nt)) + geom_point() + geom_line() + theme_bw()


## Three populations, different starting population size
N0 <- c(10,20,40) 
lambda <- 2
time <- 0:5

Nt.s <- sapply(N0, function(n) n * lambda**time) %>% as.data.frame() %>% cbind(time) %>% 
  pivot_longer(cols=1:3, names_to = 'group', values_to = 'N')

ggplot(data=Nt.s, aes(x=time, y=N, color=group)) + geom_point() + geom_line() + theme_bw()
ggplot(data=Nt.s, aes(x=time, y=N, color=group)) + geom_point() + geom_line() + scale_y_log10() + theme_bw()


## Three populations, different lambdas
N0 <- 100 
lambdas <- c(0.5, 1, 1.5)
time <- 0:5

Nt.all <- sapply(lambdas, function(x) N0 * x**time) %>% as.data.frame() %>% cbind(time) %>% pivot_longer(cols=1:3, names_to = 'group', values_to = 'N')

ggplot(data=Nt.all, aes(x=time, y=N, color=group)) + geom_point() + geom_line() + theme_bw()

ggplot(data=Nt.all, aes(x=time, y=N, color=group)) + geom_point() + geom_line() + scale_y_log10() + theme_bw()



#### generate 21 years of *simulated* Miami blue data ###
year <- 0:20
set.seed(5)
Count <- abs(round(rnorm(21, (100*1.05^year), 70),0))
dat <- as.data.frame(cbind(year,Count))

ggplot(data=dat, aes(x=year, y=Count)) + geom_point() + geom_line() + theme_bw()

dat$obs.R <- NA

dat$obs.R[2:21] <- dat$Count[2:21]/dat$Count[1:20]
obs.R <- dat$Count[2:21]/dat$Count[1:20]

## Plot R for each year-to-year transition
ggplot(data=dat, aes(x=year, y=obs.R)) + geom_point() + geom_line() + geom_abline(intercept=1,slope=0) + theme_bw()

## Plot R against pop size
ggplot(data=NULL, aes(x=dat$Count[1:20], y=obs.R)) + geom_point() + geom_abline(intercept=1,slope=0, linetype="dashed") + 
  labs(x="Population density",y="observed R") + annotate(geom="text", x=50, y=1.15,label="zero growth line", size=5) + theme_bw()


#### conduct 50 year similation analysis
years <- 0:50
set.seed(1)
sim.Rs <- sample(x = obs.R, size = length(years-1), replace = TRUE)

out50yr <- numeric(length(years))
out50yr[1] <- min(dat$Count) 

for (t in 1:50) out50yr[t + 1] <- {
  out50yr[t] * sim.Rs[t]
  }

ggplot(data=NULL, aes(x=years, y=out50yr)) + geom_point() + geom_line() + theme_bw()


### function for running many 50 year simulations
PopSim <- function(Rs, N0, years = 50, sims = 1) {
  sim.RM = matrix(sample(Rs, size = sims * years, replace = TRUE),
                    nrow = years, ncol = sims)
  output <- numeric(years + 1)
  output[1] <- N0
  outmat <- sapply(1:sims, function(i) {
    for (t in 1:years) output[t + 1] <- round(output[t] * sim.RM[t, i], 0)
    output
    })
  return(outmat)
  }


#############################################################################
## run 10 simulations

sim10 <- as.data.frame(PopSim(Rs = obs.R, N0 = 43, sims = 100)) %>% mutate(year=0:50) %>%  gather("sim","count",-year)

ggplot(sim10, aes(x=year,y=count,color=sim))+geom_line() + theme_bw()
ggplot(sim10, aes(x=year,y=log(count+1),color=sim))+geom_line() + theme_bw()

ggplot(data=sim10 %>% filter(year==50), aes(x=count))+geom_histogram()
ggplot(data=sim10 %>% filter(year==50), aes(x=log(count+1)))+geom_histogram()


simsummary <- sim10 %>% filter(year==50) %>% select(count) 
year50_quantiles <- quantile(simsummary$count, probs=c(.025,.975))

n_extinct <- sim10 %>% filter(year==50 & count==0) %>% count() %>% as.data.frame() 

year50_quantiles
n_extinct

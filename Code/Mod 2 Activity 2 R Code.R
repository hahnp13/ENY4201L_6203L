library(tidyverse)

## 1. LOGISTIC GROWTH FUNDTION
dlogistic <- function(K = 100, rd = .5, N0 = 2, t = 15) {
  N <- c(N0, numeric(t))
  for (i in 1:t) N[i + 1] <- {
    N[i] * exp(rd * (1 - N[i]/K))
  }
  return(N)
}

Nts <- dlogistic()

t <- 15; k <- 100;

ggplot(data=NULL, aes(x=0:15, y=Nts)) + geom_point() + geom_line() + geom_hline(yintercept=(k), lty=2) + labs(x="time",y="Count") + theme_bw()


## Populations with different starting populations
N0s <- c(0, 1, 10, 25, 50, 90, 110)
time <- 0:15
Ndat1 <- sapply(N0s, function(n) dlogistic(N0 = n)) %>% as.data.frame() %>% cbind(time) %>% pivot_longer(cols=1:7, names_to = 'group', values_to = 'N')

ggplot(data=Ndat1, aes(x=time, y=N, color=group)) + geom_point() + geom_line() + theme_bw()

## Populations with carrying capacities from 50 to 1000 
k.s <- c(50, 100, 250, 500, 750, 1000)
time <- 0:15
Ndat2 <- sapply(k.s, function(k) dlogistic(K = k, t = 15)) %>% as.data.frame() %>% cbind(time) %>%
  pivot_longer(cols=1:6, names_to = 'group', values_to = 'N')

ggplot(data=Ndat2, aes(x=time, y=N, color=group)) + geom_point() + geom_line() + theme_bw()

## Populations with different growth rates rd
rd.v <- seq(.9, 3, by = 0.4)
t <- 15
Ndat3 <- sapply(rd.v, function(r) dlogistic(rd = r, t = t)) %>% as.data.frame() %>% cbind(time) %>% pivot_longer(cols=1:6, names_to = 'group', values_to = 'N')

ggplot(data=Ndat3, aes(x=time, y=N)) + geom_point() + geom_line() + theme_bw() + geom_hline(yintercept = 100, lty=2) + 
  facet_wrap(~group, )

### Populations with sensativity to starting sizes
N.init <- c(97, 98, 99); time <- 0:30
Ndat4 <- sapply(N.init, function(n0) dlogistic(rd = 2.9, N0 = n0, t = 30)) %>% as.data.frame() %>% cbind(time) %>%
  pivot_longer(cols=1:3, names_to = 'group', values_to = 'N')

ggplot(data=Ndat4, aes(x=time, y=N)) + geom_point() + geom_line() + geom_hline(yintercept = 100, lty=2) + facet_wrap(~group, nrow=3) + theme_bw()


## plot population growth rate against population size for group V1. What do you see? Think about this...
DD.V1 <- Ndat4 %>% filter(group=='V1')
DD.R <- DD.V1$N[2:31]/DD.V1$N[1:30]

ggplot(data=NULL, aes(x=DD.V1$N[1:30], y=DD.R)) + geom_point() + geom_hline(yintercept=1,lty=2)+geom_vline(xintercept = 100, color="red")+ 
  xlab("Population density") + ylab("Observed R in any given year") + theme_bw()

#####################################################################
#### generate 21 years of *fake* data ###
year <- 0:20
set.seed(5)
Count <- abs(round(rnorm(21, (100*1.05^year), 70),0))
dat <- as.data.frame(cbind(year,Count))

ggplot(data=dat, aes(x=year, y=Count)) + geom_point() + geom_line() + theme_bw()

dat$obs.R <- NA

dat$obs.R[2:21] <- dat$Count[2:21]/dat$Count[1:20]
obs.R <- dat$Count[2:21]/dat$Count[1:20]

ggplot(data=dat, aes(x=year, y=obs.R)) + geom_point() + geom_line() + geom_abline(intercept=1,slope=0) + theme_bw()


## R against pop size
ggplot(data=NULL, aes(x=dat$Count[1:20], y=obs.R)) + geom_point() + geom_smooth(method='lm', formula=y~log(x)) + geom_abline(intercept=1,slope=0) + theme_bw()


##########################################################################################################################
### conduct 50 year similation analysis 
years <- 0:50
set.seed(12)

DD50yr <- numeric(length(years))
DD50yr[1] <- min(dat$Count) 

#create vector of K's with variability
Ksim = rnorm(50,154,50) 

for (t in 1:50) {
  DD50yr[t + 1] <- 
    DD50yr[t] * exp(log(2) * (1 - DD50yr[t]/Ksim[t]))
}

ggplot(data=NULL, aes(x=years, y=DD50yr)) + geom_point() + geom_line() + theme_bw()

simR <- NA
simR <- DD50yr[2:51]/DD50yr[1:50]

## R against pop size
ggplot(data=NULL, aes(x=DD50yr[1:50], y=simR)) + geom_point() + geom_abline(intercept=1,slope=0)+ theme_bw()


#############################################################################################
### function for running many 50 year simulations ###########################################
PopSim1 <- function(N0, years = 50, sims = 1, Ks) {
  sim.K = matrix(sample(Ks, size = sims * years, replace = TRUE),
                 nrow = years, ncol = sims)
  output <- numeric(years + 1)
  output[1] <- N0
  outmat <- sapply(1:sims, function(i) {
    for (t in 1:years) output[t + 1] <- 
        round(output[t] * exp(log(2) * (1 - output[t]/sim.K[t,i])), 0)
    output
  })
  return(outmat)
}

#############################################################################
## run 10 simulations

simDD10 <- as.data.frame(PopSim1(N0 = 22, sims = 100, Ks=Ksim)) %>% mutate(year=0:50)  %>%  gather("sim","count",-year)

ggplot(simDD10, aes(x=year,y=count,color=sim))+geom_line() + theme_bw()

## histogram of minimum population size for each simulation
ggplot(data=simDD10 %>% group_by(sim) %>% summarize(min_count=min(count)), aes(x=min_count))+geom_histogram()

## summary of density-dependent simulations
simDDsummary <- simDD10 %>% filter(year==50) %>% select(count) 
year50_DD_quantiles <- quantile(simDDsummary$count, probs=c(.025,.975))

n_DD_extinct <- simDD10 %>% group_by(sim) %>% summarize(min_count=min(count)) %>% filter(min_count<22) %>% count() %>% as.data.frame

year50_DD_quantiles
n_DD_extinct

mean_DD <- simDD10 %>% summarize(mean_count=mean(count)) %>%  as.data.frame()



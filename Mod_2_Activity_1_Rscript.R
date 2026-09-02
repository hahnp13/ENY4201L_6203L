# Load libraries
library(tidyverse)
library(viridis)

# Question 1-2: Exponential growth basics
NO <- 2
lambda <- 2
time <- 0:10
Nt <- NO * lambda**time

ggplot(data=NULL, aes(x=time, y=Nt)) + 
  geom_point(size=2) + geom_line() + theme_bw()

# Question 2: Effect of starting population sizes
N0 <- c(2, 10, 20) 
lambda <- 2
time <- 0:10

Nt.s <- sapply(N0, function(n) n * lambda**time) %>% 
  as.data.frame() %>% cbind(time) %>% 
  pivot_longer(cols=1:3, names_to = 'group', values_to = 'N')

ggplot(data=Nt.s, aes(x=time, y=N, color=group)) + 
  geom_point() + geom_line(linewidth = 1.5) + 
  scale_color_viridis(discrete = T) + theme_bw()

# Log scale version
ggplot(data=Nt.s, aes(x=time, y=N, color=group)) + 
  geom_point(size=3) + geom_line(linewidth = 1.5) + 
  scale_color_viridis(discrete = T) + scale_y_log10() + theme_bw()

# Question 4: Effect of lambda
N0 <- 2 
lambdas <- c(0.7, 1, 1.5)
time <- 0:5

Nt.all <- sapply(lambdas, function(x) N0 * x**time) %>% 
  as.data.frame() %>% cbind(time) %>% 
  pivot_longer(cols=1:3, names_to = 'group', values_to = 'N')

ggplot(data=Nt.all, aes(x=time, y=N, color=group)) + 
  geom_point(size=3) + geom_line(linewidth = 1.5) + 
  scale_color_viridis(discrete = T) + theme_bw()

ggplot(data=Nt.all, aes(x=time, y=N, color=group)) + 
  geom_point(size=3) + geom_line(linewidth = 1.5) + 
  scale_color_viridis(discrete = T) + scale_y_log10() + theme_bw()

# Part 2: Miami blue butterfly data
year <- 0:20
set.seed(5)
Count <- abs(round(rnorm(21, (100*1.05^year), 70), 0))
dat <- as.data.frame(cbind(year, Count))

# Plot dynamics
ggplot(data=dat, aes(x=year, y=Count)) + 
  geom_point() + geom_line() + theme_bw()

# Calculate rate of increase
dat$obs.R <- NA
dat$obs.R[2:21] <- dat$Count[2:21]/dat$Count[1:20]
obs.R <- dat$Count[2:21]/dat$Count[1:20]

ggplot(data=dat, aes(x=year, y=obs.R)) + 
  geom_point(size=2) + geom_line() + 
  geom_hline(yintercept=1, lty=2) + theme_bw()

# Plot R vs population density
ggplot(data=NULL, aes(x=dat$Count[1:20], y=obs.R)) + 
  geom_point(size=2) + 
  geom_abline(intercept=1, slope=0, linetype="dashed") + 
  labs(x="Population density", y="observed R") + theme_bw()

# 50-year single simulation
years <- 0:50
set.seed(1)
sim.Rs <- sample(x = obs.R, size = 50, replace = TRUE)

out50yr <- numeric(length(years))
out50yr[1] <- min(dat$Count) 

for (t in 1:50) out50yr[t + 1] <- {
  out50yr[t] * sim.Rs[t]
}

ggplot(data=NULL, aes(x=years, y=out50yr)) + 
  labs(y="count") + geom_point() + geom_line() + theme_bw()

# Population simulation function
PopSim <- function(Rs, N0, years = 50, sims = 1) {
  sim.RM = matrix(sample(Rs, size = sims * years, replace = TRUE),
                  nrow = years, ncol = sims)
  output <- numeric(years + 1)
  output[1] <- N0
  outmat <- sapply(1:sims, function(i) {
    for (t in 1:years) output[t + 1] <- round(output[t] * sim.RM[t, i], 0)
    output
  })
  return(as.data.frame(outmat))
}

# Run 100 simulations
sim10 <- as_tibble(PopSim(Rs = obs.R, N0 = 43, sims = 100)) %>% 
  mutate(year = 0:50) %>%  
  pivot_longer(-year, names_to = "sim", values_to = "count") |> 
  as.data.frame()

# Plot linear scale
ggplot(sim10, aes(x=year, y=count, color=sim)) +
  geom_line(linewidth = 1) + scale_color_viridis(discrete = T) + 
  theme_bw() + theme(legend.position = "none")

# Plot log scale
ggplot(sim10, aes(x=year, y=log(count+1), color=sim)) +
  geom_line(linewidth = 0.75, alpha=0.75) + 
  scale_color_viridis(discrete = T) + theme_bw() + 
  theme(legend.position = "none")

# Summary statistics at year 50
year50_data <- sim10[sim10$year == 50, ]

data.frame(
  n_extinct = sum(year50_data$count == 0),
  n_greater_1mil = sum(year50_data$count > 1000000),
  q02.5 = quantile(year50_data$count, probs = 0.025),
  q97.5 = quantile(year50_data$count, probs = 0.975)
)


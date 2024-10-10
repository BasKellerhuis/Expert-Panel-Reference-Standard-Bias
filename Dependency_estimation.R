source("Expert_Panel_Functions.R")

n <- 1000
prevalence <- 0.5
sens_t1 <- 0.7
sens_t2 <- 0.7
spec_t1 <- 0.7
spec_t2 <- 0.7

dat <- rbinom(n, 1, prevalence)

results <- 0
for (i in 1:10^3){
  results <- results + cor(SimTest(dat, sens_t1, spec_t1), SimTest(dat, sens_t2, spec_t2))
}
results / 10^3

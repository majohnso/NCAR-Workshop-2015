#
# Rat Tumor example using Stan
#

library(rstan)

rats <- read.csv("rats.csv")
head(rats)

J <- 71
y <- rats$y
n <- rats$N


rats_data <- c("J", "y", "n") # pass stan() all data in one packet

fit1 <- stan(file="rats2.stan",data=rats_data, iter = 1000, chains = 4)

traceplot(fit1)

fit2 <- stan(fit=fit1, data=rats_data, iter=10000, thin=5, chains=4)

traceplot(fit2,inc_warmup=F)

results2 <- extract(fit2, pars="avg",permuted = F, inc_warmup = FALSE) # the samples
str(results2) # list of 3
avg <- results2[,1:4,1]
par(mfrow=c(2,2))
hist(avg[,1])
acf(avg[,1])
pacf(avg[,1])

print(fit2)

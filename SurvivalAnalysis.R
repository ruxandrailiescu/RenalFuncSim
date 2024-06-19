library(survival)

### Survival analysis ###

# baseline hazard: Weibull

# N = sample size
# lambda = scale parameter in h0() (lambda>0)
# rho = shape parameter in h0() (rho>0)
# beta = fixed effect parameter
# rateC = rate parameter of the exponential distribution of C

simulWeib <- function(N, lambda, rho, beta, rateC)
{
  # covariate - N Bernoulli trials
  x <- sample(x=c(0, 1), size=N, replace=TRUE, prob=c(0.5, 0.5))
  
  # Weibull latent event times
  v <- runif(n=N, 0, 1)
  Tlat <- (-log(v) / (lambda * exp(x * beta)))^(1 / rho)
  
  # censoring times
  C <- rexp(n=N, rate=rateC)
  
  # event indicator
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # data set
  return(data.frame(time=time,
                    status=status,
                    x=x))
}


# baseline hazard: Exponential

# N = sample size
# lambda = scale parameter in h0() (lambda>0)
# beta = fixed effect parameter
# rateC = rate parameter of the exponential distribution of C

simulExp <- function(N, lambda, beta, rateC)
{
  # covariate - N Bernoulli trials
  x <- sample(x=c(0, 1), size=N, replace=TRUE, prob=c(0.5, 0.5))
  
  # Exponential latent event times
  v <- runif(n=N, 0, 1)
  Tlat <- (-log(v) / (lambda * exp(x * beta)))
  
  # censoring times
  C <- rexp(n=N, rate=rateC)
  
  # event indicator
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # data set
  return(data.frame(time=time,
                    status=status,
                    x=x))
}


set.seed(1234)

# Kaplan-Meier survival curve
simulatedData <- simulWeib(N=10^4, lambda=0.01, rho=1, beta=-0.6, rateC=0.001)
fit <- survfit(Surv(time, status) ~ 1, data=simulatedData)
plot(fit, main = "Kaplan-Meier Survival Curve", xlab = "Time", ylab = "Survival Probability", col = "blue")

simulatedData1 <- simulExp(N=10^4, lambda=0.01, beta=-0.6, rateC=0.001)
fit1 <- survfit(Surv(time, status) ~ 1, data=simulatedData1)
lines(fit1, col="red")

#betaHat <- rep(NA, 1e3)
#for(k in 1:1e3)
#{
#  dat <- simulWeib(N=100, lambda=0.01, rho=1, beta=-0.6, rateC=0.001)
#  fit <- coxph(Surv(time, status) ~ x, data=dat)
#  betaHat[k] <- fit$coef
#}

#mean(betaHat)


# Gamma distribution

# Function to calculate the hazard function for a gamma distribution
hgamma <- function(x, shape, scale) 
{
  pdf <- dgamma(x, shape = shape, scale = scale)
  surv <- pgamma(x, shape = shape, scale = scale, lower.tail = FALSE)
  hazard <- pdf / surv
  return(hazard)
}

set.seed(123)
n <- 10^4
shape_param <- 2
scale_param <- 1
random_numbers <- rgamma(n, shape = shape_param, scale = scale_param)
sorted_random_numbers <- sort(random_numbers)
hazard_values <- hgamma(sorted_random_numbers, shape = shape_param, scale = scale_param)
plot(sorted_random_numbers, hazard_values, type = 'l', col = 'blue', lwd = 2,
     xlab = 'Value', ylab = 'Hazard Function', main = 'Hazard Function of Gamma Distribution')
